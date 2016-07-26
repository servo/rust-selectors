/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use bloom::BloomHash;
use cssparser::{Token, Parser, parse_nth};
use std::ascii::AsciiExt;
use std::borrow::{Borrow, Cow};
use std::cmp;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use HashMap;

/// An empty trait that requires `HeapSizeOf` if the `heap_size` Cargo feature is enabled.
#[cfg(feature = "heap_size")] pub trait MaybeHeapSizeOf: ::heapsize::HeapSizeOf {}
#[cfg(feature = "heap_size")] impl<T: ::heapsize::HeapSizeOf> MaybeHeapSizeOf for T {}

/// An empty trait that requires `HeapSizeOf` if the `heap_size` Cargo feature is enabled.
#[cfg(not(feature = "heap_size"))] pub trait MaybeHeapSizeOf {}
#[cfg(not(feature = "heap_size"))] impl<T> MaybeHeapSizeOf for T {}

/// Although it could, String does not implement From<Cow<str>>
pub trait FromCowStr {
    fn from_cow_str(s: Cow<str>) -> Self;
}

impl FromCowStr for String {
    fn from_cow_str(s: Cow<str>) -> Self {
        s.into_owned()
    }
}

impl FromCowStr for ::string_cache::Atom {
    fn from_cow_str(s: Cow<str>) -> Self {
        s.into()
    }
}

/// This trait allows to define the parser implementation in regards
/// of pseudo-classes/elements
pub trait SelectorImpl: Sized + Debug {
    type AttrValue: Clone + Debug + MaybeHeapSizeOf + Eq + FromCowStr;
    type Identifier: Clone + Debug + MaybeHeapSizeOf + Eq + FromCowStr + Hash + BloomHash;
    type ClassName: Clone + Debug + MaybeHeapSizeOf + Eq + FromCowStr + Hash + BloomHash;
    type LocalName: Clone + Debug + MaybeHeapSizeOf + Eq + FromCowStr + Hash + BloomHash
                    + Borrow<Self::BorrowedLocalName> + for<'a> From<&'a str>;
    type Namespace: Clone + Debug + MaybeHeapSizeOf + Eq + Default + Hash + BloomHash
                    + Borrow<Self::BorrowedNamespace>;
    type BorrowedNamespace: ?Sized + Eq;
    type BorrowedLocalName: ?Sized + Eq + Hash;

    /// Declares if the following "attribute exists" selector is considered
    /// "common" enough to be shareable. If that's not the case, when matching
    /// over an element, the relation
    /// AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE would be set.
    fn attr_exists_selector_is_shareable(_attr_selector: &AttrSelector<Self>) -> bool {
        false
    }

    /// Declares if the following "equals" attribute selector is considered
    /// "common" enough to be shareable.
    fn attr_equals_selector_is_shareable(_attr_selector: &AttrSelector<Self>,
                                         _value: &Self::AttrValue) -> bool {
        false
    }

    /// non tree-structural pseudo-classes
    /// (see: https://drafts.csswg.org/selectors/#structural-pseudos)
    type NonTSPseudoClass: Sized + PartialEq + Clone + Debug + MaybeHeapSizeOf;

    /// This function can return an "Err" pseudo-element in order to support CSS2.1
    /// pseudo-elements.
    fn parse_non_ts_pseudo_class(_context: &ParserContext<Self>,
                                 _name: &str)
        -> Result<Self::NonTSPseudoClass, ()> { Err(()) }

    fn parse_non_ts_functional_pseudo_class(_context: &ParserContext<Self>,
                                            _name: &str,
                                            _arguments: &mut Parser)
        -> Result<Self::NonTSPseudoClass, ()> { Err(()) }

    /// pseudo-elements
    type PseudoElement: Sized + PartialEq + Eq + Clone + Debug + Hash + MaybeHeapSizeOf;
    fn parse_pseudo_element(_context: &ParserContext<Self>,
                            _name: &str)
        -> Result<Self::PseudoElement, ()> { Err(()) }
}

pub struct ParserContext<Impl: SelectorImpl> {
    pub in_user_agent_stylesheet: bool,
    pub default_namespace: Option<Impl::Namespace>,
    pub namespace_prefixes: HashMap<String, Impl::Namespace>,
}

impl<Impl: SelectorImpl> ParserContext<Impl> {
    pub fn new() -> Self {
        ParserContext {
            in_user_agent_stylesheet: false,
            default_namespace: None,
            namespace_prefixes: HashMap::default(),
        }
    }
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(PartialEq, Clone, Debug)]
pub struct Selector<Impl: SelectorImpl> {
    pub compound_selectors: Arc<CompoundSelector<Impl>>,
    pub pseudo_element: Option<Impl::PseudoElement>,
    pub specificity: u32,
}

fn affects_sibling<Impl: SelectorImpl>(simple_selector: &SimpleSelector<Impl>) -> bool {
    match *simple_selector {
        SimpleSelector::Negation(ref negated) => {
            negated.iter().any(|ref selector| affects_sibling(selector))
        }

        SimpleSelector::FirstChild |
        SimpleSelector::LastChild |
        SimpleSelector::OnlyChild |
        SimpleSelector::NthChild(..) |
        SimpleSelector::NthLastChild(..) |
        SimpleSelector::NthOfType(..) |
        SimpleSelector::NthLastOfType(..) |
        SimpleSelector::FirstOfType |
        SimpleSelector::LastOfType |
        SimpleSelector::OnlyOfType => true,

        _ => false,
    }
}

fn matches_non_common_style_affecting_attribute<Impl: SelectorImpl>(simple_selector: &SimpleSelector<Impl>) -> bool {
    match *simple_selector {
        SimpleSelector::Negation(ref negated) => {
            negated.iter().any(|ref selector| matches_non_common_style_affecting_attribute(selector))
        }
        SimpleSelector::AttrEqual(ref attr, ref val, _) => {
            !Impl::attr_equals_selector_is_shareable(attr, val)
        }
        SimpleSelector::AttrExists(ref attr) => {
            !Impl::attr_exists_selector_is_shareable(attr)
        }
        SimpleSelector::AttrIncludes(..) |
        SimpleSelector::AttrDashMatch(..) |
        SimpleSelector::AttrPrefixMatch(..) |
        SimpleSelector::AttrSuffixMatch(..) |
        SimpleSelector::AttrSubstringMatch(..) => true,
        _ => false,
    }
}

impl<Impl: SelectorImpl> Selector<Impl> {
    /// Whether this selector, if matching on a set of siblings, could affect
    /// other sibling's style.
    pub fn affects_siblings(&self) -> bool {
        match self.compound_selectors.next {
            Some((_, Combinator::NextSibling)) |
            Some((_, Combinator::LaterSibling)) => return true,
            _ => {},
        }

        match self.compound_selectors.simple_selectors.last() {
            Some(ref selector) => affects_sibling(selector),
            None => false,
        }
    }

    pub fn matches_non_common_style_affecting_attribute(&self) -> bool {
        match self.compound_selectors.simple_selectors.last() {
            Some(ref selector) => matches_non_common_style_affecting_attribute(selector),
            None => false,
        }
    }
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(PartialEq, Clone, Debug)]
pub struct CompoundSelector<Impl: SelectorImpl> {
    pub simple_selectors: Vec<SimpleSelector<Impl>>,
    pub next: Option<(Arc<CompoundSelector<Impl>>, Combinator)>,  // c.next is left of c
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Combinator {
    Child,  //  >
    Descendant,  // space
    NextSibling,  // +
    LaterSibling,  // ~
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum SimpleSelector<Impl: SelectorImpl> {
    ID(Impl::Identifier),
    Class(Impl::ClassName),
    LocalName(LocalName<Impl>),
    Namespace(Impl::Namespace),

    // Attribute selectors
    AttrExists(AttrSelector<Impl>),  // [foo]
    AttrEqual(AttrSelector<Impl>, Impl::AttrValue, CaseSensitivity),  // [foo=bar]
    AttrIncludes(AttrSelector<Impl>, Impl::AttrValue),  // [foo~=bar]
    AttrDashMatch(AttrSelector<Impl>, Impl::AttrValue), // [foo|=bar]
    AttrPrefixMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo^=bar]
    AttrSubstringMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo*=bar]
    AttrSuffixMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo$=bar]

    // Pseudo-classes
    Negation(Vec<SimpleSelector<Impl>>),
    FirstChild, LastChild, OnlyChild,
    Root,
    Empty,
    NthChild(i32, i32),
    NthLastChild(i32, i32),
    NthOfType(i32, i32),
    NthLastOfType(i32, i32),
    FirstOfType,
    LastOfType,
    OnlyOfType,
    NonTSPseudoClass(Impl::NonTSPseudoClass),
    // ...
}


#[derive(Eq, PartialEq, Clone, Hash, Copy, Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub enum CaseSensitivity {
    CaseSensitive,  // Selectors spec says language-defined, but HTML says sensitive.
    CaseInsensitive,
}


#[derive(Eq, PartialEq, Clone, Hash, Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct LocalName<Impl: SelectorImpl> {
    pub name: Impl::LocalName,
    pub lower_name: Impl::LocalName,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct AttrSelector<Impl: SelectorImpl> {
    pub name: Impl::LocalName,
    pub lower_name: Impl::LocalName,
    pub namespace: NamespaceConstraint<Impl>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub enum NamespaceConstraint<Impl: SelectorImpl> {
    Any,
    Specific(Impl::Namespace),
}


fn compute_specificity<Impl: SelectorImpl>(mut selector: &CompoundSelector<Impl>,
                                           pseudo_element: &Option<Impl::PseudoElement>) -> u32 {
    struct Specificity {
        id_selectors: u32,
        class_like_selectors: u32,
        element_selectors: u32,
    }
    let mut specificity = Specificity {
        id_selectors: 0,
        class_like_selectors: 0,
        element_selectors: 0,
    };
    if pseudo_element.is_some() { specificity.element_selectors += 1 }

    simple_selectors_specificity(&selector.simple_selectors, &mut specificity);
    loop {
        match selector.next {
            None => break,
            Some((ref next_selector, _)) => {
                selector = &**next_selector;
                simple_selectors_specificity(&selector.simple_selectors, &mut specificity)
            }
        }
    }

    fn simple_selectors_specificity<Impl: SelectorImpl>(simple_selectors: &[SimpleSelector<Impl>],
                                                        specificity: &mut Specificity) {
        for simple_selector in simple_selectors.iter() {
            match *simple_selector {
                SimpleSelector::LocalName(..) =>
                    specificity.element_selectors += 1,
                SimpleSelector::ID(..) =>
                    specificity.id_selectors += 1,
                SimpleSelector::Class(..) |
                SimpleSelector::AttrExists(..) |
                SimpleSelector::AttrEqual(..) |
                SimpleSelector::AttrIncludes(..) |
                SimpleSelector::AttrDashMatch(..) |
                SimpleSelector::AttrPrefixMatch(..) |
                SimpleSelector::AttrSubstringMatch(..) |
                SimpleSelector::AttrSuffixMatch(..) |

                SimpleSelector::FirstChild | SimpleSelector::LastChild |
                SimpleSelector::OnlyChild | SimpleSelector::Root |
                SimpleSelector::Empty |
                SimpleSelector::NthChild(..) |
                SimpleSelector::NthLastChild(..) |
                SimpleSelector::NthOfType(..) |
                SimpleSelector::NthLastOfType(..) |
                SimpleSelector::FirstOfType | SimpleSelector::LastOfType |
                SimpleSelector::OnlyOfType |
                SimpleSelector::NonTSPseudoClass(..) =>
                    specificity.class_like_selectors += 1,

                SimpleSelector::Namespace(..) => (),
                SimpleSelector::Negation(ref negated) =>
                    simple_selectors_specificity(negated, specificity),
            }
        }
    }

    static MAX_10BIT: u32 = (1u32 << 10) - 1;
    cmp::min(specificity.id_selectors, MAX_10BIT) << 20
    | cmp::min(specificity.class_like_selectors, MAX_10BIT) << 10
    | cmp::min(specificity.element_selectors, MAX_10BIT)
}



pub fn parse_author_origin_selector_list_from_str<Impl: SelectorImpl>(input: &str) -> Result<Vec<Selector<Impl>>, ()> {
    let context = ParserContext::new();
    parse_selector_list(&context, &mut Parser::new(input))
}

/// Parse a comma-separated list of Selectors.
/// aka Selector Group in http://www.w3.org/TR/css3-selectors/#grouping
///
/// Return the Selectors or None if there is an invalid selector.
pub fn parse_selector_list<Impl: SelectorImpl>(context: &ParserContext<Impl>, input: &mut Parser)
                           -> Result<Vec<Selector<Impl>>,()> {
    input.parse_comma_separated(|input| parse_selector(context, input))
}


/// Build up a Selector.
/// selector : simple_selector_sequence [ combinator simple_selector_sequence ]* ;
///
/// `Err` means invalid selector.
fn parse_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>, input: &mut Parser)
                                      -> Result<Selector<Impl>, ()> {
    let (first, mut pseudo_element) = try!(parse_simple_selectors(context, input));
    let mut compound = CompoundSelector{ simple_selectors: first, next: None };

    'outer_loop: while pseudo_element.is_none() {
        let combinator;
        let mut any_whitespace = false;
        loop {
            let position = input.position();
            match input.next_including_whitespace() {
                Err(()) => break 'outer_loop,
                Ok(Token::WhiteSpace(_)) => any_whitespace = true,
                Ok(Token::Delim('>')) => {
                    combinator = Combinator::Child;
                    break
                }
                Ok(Token::Delim('+')) => {
                    combinator = Combinator::NextSibling;
                    break
                }
                Ok(Token::Delim('~')) => {
                    combinator = Combinator::LaterSibling;
                    break
                }
                Ok(_) => {
                    input.reset(position);
                    if any_whitespace {
                        combinator = Combinator::Descendant;
                        break
                    } else {
                        break 'outer_loop
                    }
                }
            }
        }
        let (simple_selectors, pseudo) = try!(parse_simple_selectors(context, input));
        compound = CompoundSelector {
            simple_selectors: simple_selectors,
            next: Some((Arc::new(compound), combinator))
        };
        pseudo_element = pseudo;
    }
    Ok(Selector {
        specificity: compute_specificity(&compound, &pseudo_element),
        compound_selectors: Arc::new(compound),
        pseudo_element: pseudo_element,
    })
}


/// * `Err(())`: Invalid selector, abort
/// * `Ok(None)`: Not a type selector, could be something else. `input` was not consumed.
/// * `Ok(Some(vec))`: Length 0 (`*|*`), 1 (`*|E` or `ns|*`) or 2 (`|E` or `ns|E`)
fn parse_type_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>, input: &mut Parser)
                       -> Result<Option<Vec<SimpleSelector<Impl>>>, ()> {
    match try!(parse_qualified_name(context, input, /* in_attr_selector = */ false)) {
        None => Ok(None),
        Some((namespace, local_name)) => {
            let mut simple_selectors = vec!();
            match namespace {
                NamespaceConstraint::Specific(ns) => {
                    simple_selectors.push(SimpleSelector::Namespace(ns))
                },
                NamespaceConstraint::Any => (),
            }
            match local_name {
                Some(name) => {
                    simple_selectors.push(SimpleSelector::LocalName(LocalName {
                        lower_name: Impl::LocalName::from(&*name.to_ascii_lowercase()),
                        name: Impl::LocalName::from_cow_str(name),
                    }))
                }
                None => (),
            }
            Ok(Some(simple_selectors))
        }
    }
}


#[derive(Debug)]
enum SimpleSelectorParseResult<Impl: SelectorImpl> {
    SimpleSelector(SimpleSelector<Impl>),
    PseudoElement(Impl::PseudoElement),
}


/// * `Err(())`: Invalid selector, abort
/// * `Ok(None)`: Not a simple selector, could be something else. `input` was not consumed.
/// * `Ok(Some((namespace, local_name)))`: `None` for the local name means a `*` universal selector
fn parse_qualified_name<'i, 't, Impl: SelectorImpl>
                       (context: &ParserContext<Impl>, input: &mut Parser<'i, 't>,
                        in_attr_selector: bool)
                        -> Result<Option<(NamespaceConstraint<Impl>, Option<Cow<'i, str>>)>, ()> {
    let default_namespace = |local_name| {
        let namespace = match context.default_namespace {
            Some(ref ns) => NamespaceConstraint::Specific(ns.clone()),
            None => NamespaceConstraint::Any,
        };
        Ok(Some((namespace, local_name)))
    };

    let explicit_namespace = |input: &mut Parser<'i, 't>, namespace| {
        match input.next_including_whitespace() {
            Ok(Token::Delim('*')) if !in_attr_selector => {
                Ok(Some((namespace, None)))
            },
            Ok(Token::Ident(local_name)) => {
                Ok(Some((namespace, Some(local_name))))
            },
            _ => Err(()),
        }
    };

    let position = input.position();
    match input.next_including_whitespace() {
        Ok(Token::Ident(value)) => {
            let position = input.position();
            match input.next_including_whitespace() {
                Ok(Token::Delim('|')) => {
                    let result = context.namespace_prefixes.get(&*value);
                    let namespace = try!(result.ok_or(()));
                    explicit_namespace(input, NamespaceConstraint::Specific(namespace.clone()))
                },
                _ => {
                    input.reset(position);
                    if in_attr_selector {
                        Ok(Some((NamespaceConstraint::Specific(Default::default()), Some(value))))
                    } else {
                        default_namespace(Some(value))
                    }
                }
            }
        },
        Ok(Token::Delim('*')) => {
            let position = input.position();
            match input.next_including_whitespace() {
                Ok(Token::Delim('|')) => explicit_namespace(input, NamespaceConstraint::Any),
                _ => {
                    input.reset(position);
                    if in_attr_selector {
                        Err(())
                    } else {
                        default_namespace(None)
                    }
                },
            }
        },
        Ok(Token::Delim('|')) => {
            explicit_namespace(input, NamespaceConstraint::Specific(Default::default()))
        }
        _ => {
            input.reset(position);
            Ok(None)
        }
    }
}


fn parse_attribute_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>, input: &mut Parser)
                            -> Result<SimpleSelector<Impl>, ()> {
    let attr = match try!(parse_qualified_name(context, input, /* in_attr_selector = */ true)) {
        None => return Err(()),
        Some((_, None)) => unreachable!(),
        Some((namespace, Some(local_name))) => AttrSelector {
            namespace: namespace,
            lower_name: Impl::LocalName::from(&*local_name.to_ascii_lowercase()),
            name: Impl::LocalName::from_cow_str(local_name),
        },
    };

    fn parse_value<Impl: SelectorImpl>(input: &mut Parser) -> Result<Impl::AttrValue, ()> {
        Ok(Impl::AttrValue::from_cow_str(try!(input.expect_ident_or_string())))
    }
    // TODO: deal with empty value or value containing whitespace (see spec)
    match input.next() {
        // [foo]
        Err(()) => Ok(SimpleSelector::AttrExists(attr)),

        // [foo=bar]
        Ok(Token::Delim('=')) => {
            Ok(SimpleSelector::AttrEqual(attr, try!(parse_value::<Impl>(input)),
                                         try!(parse_attribute_flags(input))))
        }
        // [foo~=bar]
        Ok(Token::IncludeMatch) => {
            Ok(SimpleSelector::AttrIncludes(attr, try!(parse_value::<Impl>(input))))
        }
        // [foo|=bar]
        Ok(Token::DashMatch) => {
            let value = try!(parse_value::<Impl>(input));
            Ok(SimpleSelector::AttrDashMatch(attr, value))
        }
        // [foo^=bar]
        Ok(Token::PrefixMatch) => {
            Ok(SimpleSelector::AttrPrefixMatch(attr, try!(parse_value::<Impl>(input))))
        }
        // [foo*=bar]
        Ok(Token::SubstringMatch) => {
            Ok(SimpleSelector::AttrSubstringMatch(attr, try!(parse_value::<Impl>(input))))
        }
        // [foo$=bar]
        Ok(Token::SuffixMatch) => {
            Ok(SimpleSelector::AttrSuffixMatch(attr, try!(parse_value::<Impl>(input))))
        }
        _ => Err(())
    }
}


fn parse_attribute_flags(input: &mut Parser) -> Result<CaseSensitivity, ()> {
    match input.next() {
        Err(()) => Ok(CaseSensitivity::CaseSensitive),
        Ok(Token::Ident(ref value)) if value.eq_ignore_ascii_case("i") => {
            Ok(CaseSensitivity::CaseInsensitive)
        }
        _ => Err(())
    }
}


/// Level 3: Parse **one** simple_selector.  (Though we might insert a second
/// implied "<defaultns>|*" type selector.)
fn parse_negation<Impl: SelectorImpl>(context: &ParserContext<Impl>,
                                      input: &mut Parser)
                                      -> Result<SimpleSelector<Impl>, ()> {
    match try!(parse_type_selector(context, input)) {
        Some(type_selector) => Ok(SimpleSelector::Negation(type_selector)),
        None => {
            match try!(parse_one_simple_selector(context,
                                                 input,
                                                 /* inside_negation = */ true)) {
                Some(SimpleSelectorParseResult::SimpleSelector(simple_selector)) => {
                    let simple_selectors = match context.default_namespace {
                        // If there was no explicit type selector, but there is
                        // a default namespace, there is an implicit
                        // "<defaultns>|*" type selector before simple_selector.
                        Some(ref ns) => vec![SimpleSelector::Namespace(ns.clone()), simple_selector],
                        None => vec![simple_selector],
                    };
                    Ok(SimpleSelector::Negation(simple_selectors))
                }
                _ => Err(())
            }
        },
    }
}

/// simple_selector_sequence
/// : [ type_selector | universal ] [ HASH | class | attrib | pseudo | negation ]*
/// | [ HASH | class | attrib | pseudo | negation ]+
///
/// `Err(())` means invalid selector
fn parse_simple_selectors<Impl: SelectorImpl>(context: &ParserContext<Impl>,
                                              input: &mut Parser)
                                              -> Result<(Vec<SimpleSelector<Impl>>, Option<Impl::PseudoElement>), ()> {
    // Consume any leading whitespace.
    loop {
        let position = input.position();
        if !matches!(input.next_including_whitespace(), Ok(Token::WhiteSpace(_))) {
            input.reset(position);
            break
        }
    }
    let mut empty = true;
    let mut simple_selectors = match try!(parse_type_selector(context, input)) {
        None => {
            match context.default_namespace {
                // If there was no explicit type selector, but there is a
                // default namespace, there is an implicit "<defaultns>|*" type
                // selector.
                Some(ref ns) => vec![SimpleSelector::Namespace(ns.clone())],
                None => vec![],
            }
        }
        Some(s) => { empty = false; s }
    };

    let mut pseudo_element = None;
    loop {
        match try!(parse_one_simple_selector(context,
                                             input,
                                             /* inside_negation = */ false)) {
            None => break,
            Some(SimpleSelectorParseResult::SimpleSelector(s)) => {
                simple_selectors.push(s);
                empty = false
            }
            Some(SimpleSelectorParseResult::PseudoElement(p)) => {
                pseudo_element = Some(p);
                empty = false;
                break
            }
        }
    }
    if empty {
        // An empty selector is invalid.
        Err(())
    } else {
        Ok((simple_selectors, pseudo_element))
    }
}

fn parse_functional_pseudo_class<Impl: SelectorImpl>(context: &ParserContext<Impl>,
                                                     input: &mut Parser,
                                                     name: &str,
                                                     inside_negation: bool)
                                                     -> Result<SimpleSelector<Impl>, ()> {
    match_ignore_ascii_case! { name,
        "nth-child" => parse_nth_pseudo_class(input, SimpleSelector::NthChild),
        "nth-of-type" => parse_nth_pseudo_class(input, SimpleSelector::NthOfType),
        "nth-last-child" => parse_nth_pseudo_class(input, SimpleSelector::NthLastChild),
        "nth-last-of-type" => parse_nth_pseudo_class(input, SimpleSelector::NthLastOfType),
        "not" => {
            if inside_negation {
                Err(())
            } else {
                parse_negation(context, input)
            }
        },
        _ => Impl::parse_non_ts_functional_pseudo_class(context, name, input)
            .map(SimpleSelector::NonTSPseudoClass)
    }
}


fn parse_nth_pseudo_class<Impl: SelectorImpl, F>(input: &mut Parser, selector: F) -> Result<SimpleSelector<Impl>, ()>
where F: FnOnce(i32, i32) -> SimpleSelector<Impl> {
    let (a, b) = try!(parse_nth(input));
    Ok(selector(a, b))
}


/// Parse a simple selector other than a type selector.
///
/// * `Err(())`: Invalid selector, abort
/// * `Ok(None)`: Not a simple selector, could be something else. `input` was not consumed.
/// * `Ok(Some(_))`: Parsed a simple selector or pseudo-element
fn parse_one_simple_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>,
                             input: &mut Parser,
                             inside_negation: bool)
                             -> Result<Option<SimpleSelectorParseResult<Impl>>,()> {
    let start_position = input.position();
    match input.next_including_whitespace() {
        Ok(Token::IDHash(id)) => {
            let id = SimpleSelector::ID(Impl::Identifier::from_cow_str(id));
            Ok(Some(SimpleSelectorParseResult::SimpleSelector(id)))
        }
        Ok(Token::Delim('.')) => {
            match input.next_including_whitespace() {
                Ok(Token::Ident(class)) => {
                    let class = SimpleSelector::Class(Impl::ClassName::from_cow_str(class));
                    Ok(Some(SimpleSelectorParseResult::SimpleSelector(class)))
                }
                _ => Err(()),
            }
        }
        Ok(Token::SquareBracketBlock) => {
            let attr = try!(input.parse_nested_block(|input| {
                parse_attribute_selector(context, input)
            }));
            Ok(Some(SimpleSelectorParseResult::SimpleSelector(attr)))
        }
        Ok(Token::Colon) => {
            match input.next_including_whitespace() {
                Ok(Token::Ident(name)) => {
                    // Supported CSS 2.1 pseudo-elements only.
                    // ** Do not add to this list! **
                    if name.eq_ignore_ascii_case("before") ||
                       name.eq_ignore_ascii_case("after") ||
                       name.eq_ignore_ascii_case("first-line") ||
                       name.eq_ignore_ascii_case("first-letter") {
                        let pseudo_element = try!(Impl::parse_pseudo_element(context, &name));
                        Ok(Some(SimpleSelectorParseResult::PseudoElement(pseudo_element)))
                    } else {
                        let pseudo_class = try!(parse_simple_pseudo_class(context, &name));
                        Ok(Some(SimpleSelectorParseResult::SimpleSelector(pseudo_class)))
                    }
                }
                Ok(Token::Function(name)) => {
                    let pseudo = try!(input.parse_nested_block(|input| {
                        parse_functional_pseudo_class(context, input, &name, inside_negation)
                    }));
                    Ok(Some(SimpleSelectorParseResult::SimpleSelector(pseudo)))
                }
                Ok(Token::Colon) => {
                    match input.next() {
                        Ok(Token::Ident(name)) => {
                            let pseudo = try!(Impl::parse_pseudo_element(context, &name));
                            Ok(Some(SimpleSelectorParseResult::PseudoElement(pseudo)))
                        }
                        _ => Err(())
                    }
                }
                _ => Err(())
            }
        }
        _ => {
            input.reset(start_position);
            Ok(None)
        }
    }
}

fn parse_simple_pseudo_class<Impl: SelectorImpl>(context: &ParserContext<Impl>, name: &str)
                             -> Result<SimpleSelector<Impl>, ()> {
    match_ignore_ascii_case! { name,
        "first-child" => Ok(SimpleSelector::FirstChild),
        "last-child"  => Ok(SimpleSelector::LastChild),
        "only-child"  => Ok(SimpleSelector::OnlyChild),
        "root" => Ok(SimpleSelector::Root),
        "empty" => Ok(SimpleSelector::Empty),
        "first-of-type" => Ok(SimpleSelector::FirstOfType),
        "last-of-type"  => Ok(SimpleSelector::LastOfType),
        "only-of-type"  => Ok(SimpleSelector::OnlyOfType),
        _ => Impl::parse_non_ts_pseudo_class(context, name).map(|pc| SimpleSelector::NonTSPseudoClass(pc))
    }
}

// NB: pub module in order to access the DummySelectorImpl
#[cfg(test)]
pub mod tests {
    use std::sync::Arc;
    use cssparser::Parser;
    use super::*;

    #[derive(PartialEq, Clone, Debug)]
    pub enum PseudoClass {
        ServoNonZeroBorder,
        Lang(String),
    }

    #[derive(Eq, PartialEq, Clone, Debug, Hash)]
    pub enum PseudoElement {
        Before,
        After,
    }

    #[derive(PartialEq, Debug)]
    pub struct DummySelectorImpl;

    impl SelectorImpl for DummySelectorImpl {
        type AttrValue = String;
        type Identifier = String;
        type ClassName = String;
        type Namespace = String;
        type LocalName = String;
        type BorrowedNamespace = str;
        type BorrowedLocalName = str;

        type NonTSPseudoClass = PseudoClass;

        fn parse_non_ts_pseudo_class(context: &ParserContext<Self>, name: &str)
                                     -> Result<PseudoClass, ()> {
            match_ignore_ascii_case! { name,
                "-servo-nonzero-border" => {
                    if context.in_user_agent_stylesheet {
                        Ok(PseudoClass::ServoNonZeroBorder)
                    } else {
                        Err(())
                    }
                },
                _ => Err(())
            }
        }

        fn parse_non_ts_functional_pseudo_class(_context: &ParserContext<Self>, name: &str,
                                                parser: &mut Parser) -> Result<PseudoClass, ()> {
            match_ignore_ascii_case! { name,
                "lang" => Ok(PseudoClass::Lang(try!(parser.expect_ident_or_string()).into_owned())),
                _ => Err(())
            }
        }

        type PseudoElement = PseudoElement;
        fn parse_pseudo_element(_context: &ParserContext<Self>, name: &str)
                                -> Result<PseudoElement, ()> {
            match_ignore_ascii_case! { name,
                "before" => Ok(PseudoElement::Before),
                "after" => Ok(PseudoElement::After),
                _ => Err(())
            }
        }
    }

    fn parse(input: &str) -> Result<Vec<Selector<DummySelectorImpl>>, ()> {
        parse_ns(input, &ParserContext::new())
    }

    fn parse_ns(input: &str, context: &ParserContext<DummySelectorImpl>)
                -> Result<Vec<Selector<DummySelectorImpl>>, ()> {
        parse_selector_list(context, &mut Parser::new(input))
    }

    fn specificity(a: u32, b: u32, c: u32) -> u32 {
        a << 20 | b << 10 | c
    }

    #[test]
    fn test_empty() {
        let list = parse_author_origin_selector_list_from_str::<DummySelectorImpl>(":empty");
        assert!(list.is_ok());
    }

    const MATHML: &'static str = "http://www.w3.org/1998/Math/MathML";

    #[test]
    fn test_parsing() {
        assert_eq!(parse(""), Err(())) ;
        assert_eq!(parse(":lang(4)"), Err(())) ;
        assert_eq!(parse(":lang(en US)"), Err(())) ;
        assert_eq!(parse("EeÉ"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(SimpleSelector::LocalName(LocalName {
                    name: String::from("EeÉ"),
                    lower_name: String::from("eeÉ") })),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse(".foo:lang(en-US)"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec![
                    SimpleSelector::Class(String::from("foo")),
                    SimpleSelector::NonTSPseudoClass(PseudoClass::Lang("en-US".to_owned()))
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 2, 0),
        })));
        assert_eq!(parse("#bar"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(SimpleSelector::ID(String::from("bar"))),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(1, 0, 0),
        })));
        assert_eq!(parse("e.foo#bar"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(SimpleSelector::LocalName(LocalName {
                                            name: String::from("e"),
                                            lower_name: String::from("e") }),
                                       SimpleSelector::Class(String::from("foo")),
                                       SimpleSelector::ID(String::from("bar"))),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(1, 1, 1),
        })));
        assert_eq!(parse("e.foo #bar"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(SimpleSelector::ID(String::from("bar"))),
                next: Some((Arc::new(CompoundSelector {
                    simple_selectors: vec!(SimpleSelector::LocalName(LocalName {
                                                name: String::from("e"),
                                                lower_name: String::from("e") }),
                                           SimpleSelector::Class(String::from("foo"))),
                    next: None,
                }), Combinator::Descendant)),
            }),
            pseudo_element: None,
            specificity: specificity(1, 1, 1),
        })));
        // Default namespace does not apply to attribute selectors
        // https://github.com/mozilla/servo/pull/1652
        let mut context = ParserContext::new();
        assert_eq!(parse_ns("[Foo]", &context), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(SimpleSelector::AttrExists(AttrSelector {
                    name: String::from("Foo"),
                    lower_name: String::from("foo"),
                    namespace: NamespaceConstraint::Specific("".into()),
                })),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        })));
        // Default namespace does not apply to attribute selectors
        // https://github.com/mozilla/servo/pull/1652
        // but it does apply to implicit type selectors
        // https://github.com/servo/rust-selectors/pull/82
        context.default_namespace = Some(MATHML.into());
        assert_eq!(parse_ns("[Foo]", &context), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec![
                    SimpleSelector::Namespace(MATHML.into()),
                    SimpleSelector::AttrExists(AttrSelector {
                        name: String::from("Foo"),
                        lower_name: String::from("foo"),
                        namespace: NamespaceConstraint::Specific("".into()),
                    }),
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        })));
        // Default namespace does apply to type selectors
        assert_eq!(parse_ns("e", &context), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(
                    SimpleSelector::Namespace(MATHML.into()),
                    SimpleSelector::LocalName(LocalName {
                        name: String::from("e"),
                        lower_name: String::from("e") }),
                ),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse("[attr|=\"foo\"]"), Ok(vec![Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec![
                    SimpleSelector::AttrDashMatch(AttrSelector {
                        name: String::from("attr"),
                        lower_name: String::from("attr"),
                        namespace: NamespaceConstraint::Specific("".into()),
                    }, "foo".to_owned())
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        }]));
        // https://github.com/mozilla/servo/issues/1723
        assert_eq!(parse("::before"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(),
                next: None,
            }),
            pseudo_element: Some(PseudoElement::Before),
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse("div :after"), Ok(vec!(Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec!(),
                next: Some((Arc::new(CompoundSelector {
                    simple_selectors: vec!(SimpleSelector::LocalName(LocalName {
                        name: String::from("div"),
                        lower_name: String::from("div") })),
                    next: None,
                }), Combinator::Descendant)),
            }),
            pseudo_element: Some(PseudoElement::After),
            specificity: specificity(0, 0, 2),
        })));
        assert_eq!(parse("#d1 > .ok"), Ok(vec![Selector {
            compound_selectors: Arc::new(CompoundSelector {
                simple_selectors: vec![
                    SimpleSelector::Class(String::from("ok")),
                ],
                next: Some((Arc::new(CompoundSelector {
                    simple_selectors: vec![
                        SimpleSelector::ID(String::from("d1")),
                    ],
                    next: None,
                }), Combinator::Child)),
            }),
            pseudo_element: None,
            specificity: (1 << 20) + (1 << 10) + (0 << 0),
        }]))
    }
}
