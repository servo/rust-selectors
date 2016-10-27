/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use cssparser::{Token, Parser, parse_nth, ToCss, serialize_identifier, CssStringWriter};
use std::ascii::AsciiExt;
use std::borrow::{Borrow, Cow};
use std::cmp;
use std::fmt::{self, Display, Debug, Write};
use std::hash::Hash;
use std::ops::Add;
use std::sync::Arc;

use HashMap;

macro_rules! with_bounds {
    ( [ $( $CommonBounds: tt )* ] [ $( $FromStr: tt )* ]) => {
        fn from_cow_str<T>(cow: Cow<str>) -> T where T: $($FromStr)* {
            match cow {
                Cow::Borrowed(s) => T::from(s),
                Cow::Owned(s) => T::from(s),
            }
        }

        fn from_ascii_lowercase<T>(s: &str) -> T where T: $($FromStr)* {
            if let Some(first_uppercase) = s.bytes().position(|byte| byte >= b'A' && byte <= b'Z') {
                let mut string = s.to_owned();
                string[first_uppercase..].make_ascii_lowercase();
                T::from(string)
            } else {
                T::from(s)
            }
        }

        /// This trait allows to define the parser implementation in regards
        /// of pseudo-classes/elements
        pub trait SelectorImpl: Sized + Debug {
            type AttrValue: $($CommonBounds)* + $($FromStr)* + Display;
            type Identifier: $($CommonBounds)* + $($FromStr)* + Display;
            type ClassName: $($CommonBounds)* + $($FromStr)* + Display;
            type LocalName: $($CommonBounds)* + $($FromStr)* + Display
                            + Borrow<Self::BorrowedLocalName>;
            type NamespaceUrl: $($CommonBounds)* + Display + Default
                               + Borrow<Self::BorrowedNamespaceUrl>;
            type NamespacePrefix: $($CommonBounds)* + $($FromStr)* + Display + Default;
            type BorrowedNamespaceUrl: ?Sized + Eq;
            type BorrowedLocalName: ?Sized + Eq + Hash;

            /// non tree-structural pseudo-classes
            /// (see: https://drafts.csswg.org/selectors/#structural-pseudos)
            type NonTSPseudoClass: $($CommonBounds)* + Sized + ToCss;

            /// pseudo-elements
            type PseudoElement: $($CommonBounds)* + Sized + ToCss;

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

            /// This function can return an "Err" pseudo-element in order to support CSS2.1
            /// pseudo-elements.
            fn parse_non_ts_pseudo_class(_context: &ParserContext<Self>,
                                         _name: &str)
                -> Result<Self::NonTSPseudoClass, ()> { Err(()) }

            fn parse_non_ts_functional_pseudo_class(_context: &ParserContext<Self>,
                                                    _name: &str,
                                                    _arguments: &mut Parser)
                -> Result<Self::NonTSPseudoClass, ()> { Err(()) }
            fn parse_pseudo_element(_context: &ParserContext<Self>,
                                    _name: &str)
                -> Result<Self::PseudoElement, ()> { Err(()) }
        }
    }
}

macro_rules! with_heap_size_bound {
    ($( $HeapSizeOf: tt )*) => {
        with_bounds! {
            [Clone + Eq + Hash $($HeapSizeOf)*]
            [From<String> + for<'a> From<&'a str>]
        }
    }
}

#[cfg(feature = "heap_size")]
with_heap_size_bound!(+ ::heapsize::HeapSizeOf);

#[cfg(not(feature = "heap_size"))]
with_heap_size_bound!();

pub struct ParserContext<Impl: SelectorImpl> {
    pub in_user_agent_stylesheet: bool,
    pub default_namespace: Option<Impl::NamespaceUrl>,
    pub namespace_prefixes: HashMap<Impl::NamespacePrefix, Impl::NamespaceUrl>,
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
#[derive(PartialEq, Clone)]
pub struct Selector<Impl: SelectorImpl> {
    pub complex_selector: Arc<ComplexSelector<Impl>>,
    pub pseudo_element: Option<Impl::PseudoElement>,
    pub specificity: u32,
}

fn affects_sibling<Impl: SelectorImpl>(simple_selector: &SimpleSelector<Impl>) -> bool {
    match *simple_selector {
        SimpleSelector::Negation(ref negated) => {
            negated.iter().any(|ref selector| selector.affects_siblings())
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
            negated.iter().any(|ref selector| selector.matches_non_common_style_affecting_attribute())
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
        self.complex_selector.affects_siblings()
    }

    pub fn matches_non_common_style_affecting_attribute(&self) -> bool {
        self.complex_selector.matches_non_common_style_affecting_attribute()
    }
}

impl<Impl: SelectorImpl> ComplexSelector<Impl> {
    /// Whether this complex selector, if matching on a set of siblings,
    /// could affect other sibling's style.
    pub fn affects_siblings(&self) -> bool {
        match self.next {
            Some((_, Combinator::NextSibling)) |
            Some((_, Combinator::LaterSibling)) => return true,
            _ => {},
        }

        match self.compound_selector.last() {
            Some(ref selector) => affects_sibling(selector),
            None => false,
        }
    }

    pub fn matches_non_common_style_affecting_attribute(&self) -> bool {
        match self.compound_selector.last() {
            Some(ref selector) => matches_non_common_style_affecting_attribute(selector),
            None => false,
        }
    }
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct ComplexSelector<Impl: SelectorImpl> {
    pub compound_selector: Vec<SimpleSelector<Impl>>,
    pub next: Option<(Arc<ComplexSelector<Impl>>, Combinator)>,  // c.next is left of c
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum Combinator {
    Child,  //  >
    Descendant,  // space
    NextSibling,  // +
    LaterSibling,  // ~
}

#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(Eq, PartialEq, Clone, Hash)]
pub enum SimpleSelector<Impl: SelectorImpl> {
    ID(Impl::Identifier),
    Class(Impl::ClassName),
    LocalName(LocalName<Impl>),
    Namespace(Namespace<Impl>),

    // Attribute selectors
    AttrExists(AttrSelector<Impl>),  // [foo]
    AttrEqual(AttrSelector<Impl>, Impl::AttrValue, CaseSensitivity),  // [foo=bar]
    AttrIncludes(AttrSelector<Impl>, Impl::AttrValue),  // [foo~=bar]
    AttrDashMatch(AttrSelector<Impl>, Impl::AttrValue), // [foo|=bar]
    AttrPrefixMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo^=bar]
    AttrSubstringMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo*=bar]
    AttrSuffixMatch(AttrSelector<Impl>, Impl::AttrValue),  // [foo$=bar]

    // Pseudo-classes
    Negation(Vec<Arc<ComplexSelector<Impl>>>),
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


#[derive(Eq, PartialEq, Clone, Hash)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct LocalName<Impl: SelectorImpl> {
    pub name: Impl::LocalName,
    pub lower_name: Impl::LocalName,
}

#[derive(Eq, PartialEq, Clone, Hash)]
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
    Specific(Namespace<Impl>),
}

/// FIXME(SimonSapin): should Hash only hash the URL? What is it used for?
#[derive(Eq, PartialEq, Clone, Hash)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct Namespace<Impl: SelectorImpl> {
    pub prefix: Option<Impl::NamespacePrefix>,
    pub url: Impl::NamespaceUrl,
}

impl<Impl: SelectorImpl> Default for Namespace<Impl> {
    fn default() -> Self {
        Namespace {
            prefix: None,
            url: Impl::NamespaceUrl::default(),  // empty string
        }
    }
}


impl<Impl: SelectorImpl> Debug for Selector<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.write_str("Selector("));
        try!(self.to_css(f));
        write!(f, ", specificity = 0x{:x})", self.specificity)
    }
}

impl<Impl: SelectorImpl> Debug for ComplexSelector<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_css(f) }
}
impl<Impl: SelectorImpl> Debug for SimpleSelector<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_css(f) }
}
impl<Impl: SelectorImpl> Debug for AttrSelector<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_css(f) }
}
impl<Impl: SelectorImpl> Debug for Namespace<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_css(f) }
}
impl<Impl: SelectorImpl> Debug for LocalName<Impl> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_css(f) }
}

impl<Impl: SelectorImpl> ToCss for Selector<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        try!(self.complex_selector.to_css(dest));
        if let Some(ref pseudo) = self.pseudo_element {
            try!(pseudo.to_css(dest));
        }
        Ok(())
    }
}

impl<Impl: SelectorImpl> ToCss for ComplexSelector<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        if let Some((ref next, ref combinator)) = self.next {
            try!(next.to_css(dest));
            try!(combinator.to_css(dest));
        }
        for simple in &self.compound_selector {
            try!(simple.to_css(dest));
        }
        Ok(())
    }
}

impl ToCss for Combinator {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        match *self {
            Combinator::Child => dest.write_str(" > "),
            Combinator::Descendant => dest.write_str(" "),
            Combinator::NextSibling => dest.write_str(" + "),
            Combinator::LaterSibling => dest.write_str(" ~ "),
        }
    }
}

impl<Impl: SelectorImpl> ToCss for SimpleSelector<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        use self::SimpleSelector::*;
        match *self {
            ID(ref s) => {
                try!(dest.write_char('#'));
                display_to_css_identifier(s, dest)
            }
            Class(ref s) => {
                try!(dest.write_char('.'));
                display_to_css_identifier(s, dest)
            }
            LocalName(ref s) => s.to_css(dest),
            Namespace(ref ns) => ns.to_css(dest),

            // Attribute selectors
            AttrExists(ref a) => {
                try!(dest.write_char('['));
                try!(a.to_css(dest));
                dest.write_char(']')
            }
            AttrEqual(ref a, ref v, case) => {
                attr_selector_to_css(a, " = ", v, match case {
                    CaseSensitivity::CaseSensitive => None,
                    CaseSensitivity::CaseInsensitive => Some(" i"),
                 }, dest)
            }
            AttrIncludes(ref a, ref v) => attr_selector_to_css(a, " ~= ", v, None, dest),
            AttrDashMatch(ref a, ref v) => attr_selector_to_css(a, " |= ", v, None, dest),
            AttrPrefixMatch(ref a, ref v) => attr_selector_to_css(a, " ^= ", v, None, dest),
            AttrSubstringMatch(ref a, ref v) => attr_selector_to_css(a, " *= ", v, None, dest),
            AttrSuffixMatch(ref a, ref v) => attr_selector_to_css(a, " $= ", v, None, dest),

            // Pseudo-classes
            Negation(ref args) => {
                try!(dest.write_str(":not("));
                let mut args = args.iter();
                let first = args.next().unwrap();
                try!(first.to_css(dest));
                for arg in args {
                    try!(dest.write_str(", "));
                    try!(arg.to_css(dest));
                }
                dest.write_str(")")
            }

            FirstChild => dest.write_str(":first-child"),
            LastChild => dest.write_str(":last-child"),
            OnlyChild => dest.write_str(":only-child"),
            Root => dest.write_str(":root"),
            Empty => dest.write_str(":empty"),
            FirstOfType => dest.write_str(":first-of-type"),
            LastOfType => dest.write_str(":last-of-type"),
            OnlyOfType => dest.write_str(":only-of-type"),
            NthChild(a, b) => write!(dest, ":nth-child({}n{:+})", a, b),
            NthLastChild(a, b) => write!(dest, ":nth-last-child({}n{:+})", a, b),
            NthOfType(a, b) => write!(dest, ":nth-of-type({}n{:+})", a, b),
            NthLastOfType(a, b) => write!(dest, ":nth-last-of-type({}n{:+})", a, b),
            NonTSPseudoClass(ref pseudo) => pseudo.to_css(dest),
        }
    }
}

impl<Impl: SelectorImpl> ToCss for AttrSelector<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        if let NamespaceConstraint::Specific(ref ns) = self.namespace {
            try!(ns.to_css(dest));
        }
        display_to_css_identifier(&self.name, dest)
    }
}

impl<Impl: SelectorImpl> ToCss for Namespace<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        if let Some(ref prefix) = self.prefix {
            try!(display_to_css_identifier(prefix, dest));
            try!(dest.write_char('|'));
        }
        Ok(())
    }
}

impl<Impl: SelectorImpl> ToCss for LocalName<Impl> {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        display_to_css_identifier(&self.name, dest)
    }
}

fn attr_selector_to_css<Impl, W>(attr: &AttrSelector<Impl>,
                                 operator: &str,
                                 value: &Impl::AttrValue,
                                 modifier: Option<&str>,
                                 dest: &mut W)
                                 -> fmt::Result
where Impl: SelectorImpl, W: fmt::Write
{
    try!(dest.write_char('['));
    try!(attr.to_css(dest));
    try!(dest.write_str(operator));
    try!(dest.write_char('"'));
    try!(write!(CssStringWriter::new(dest), "{}", value));
    try!(dest.write_char('"'));
    if let Some(m) = modifier {
        try!(dest.write_str(m));
    }
    dest.write_char(']')
}

/// Serialize the output of Display as a CSS identifier
fn display_to_css_identifier<T: Display, W: fmt::Write>(x: &T, dest: &mut W) -> fmt::Result {
    // FIXME(SimonSapin): it is possible to avoid this heap allocation
    // by creating a stream adapter like cssparser::CssStringWriter
    // that holds and writes to `&mut W` and itself implements `fmt::Write`.
    //
    // I haven’t done this yet because it would require somewhat complex and fragile state machine
    // to support in `fmt::Write::write_char` cases that,
    // in `serialize_identifier` (which has the full value as a `&str` slice),
    // can be expressed as
    // `string.starts_with("--")`, `string == "-"`, `string.starts_with("-")`, etc.
    //
    // And I don’t even know if this would be a performance win: jemalloc is good at what it does
    // and the state machine might be slower than `serialize_identifier` as currently written.
    let string = x.to_string();

    serialize_identifier(&string, dest)
}

const MAX_10BIT: u32 = (1u32 << 10) - 1;

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
struct Specificity {
    id_selectors: u32,
    class_like_selectors: u32,
    element_selectors: u32,
}

impl Add for Specificity {
    type Output = Specificity;

    fn add(self, rhs: Specificity) -> Specificity {
        Specificity {
            id_selectors: self.id_selectors + rhs.id_selectors,
            class_like_selectors:
                self.class_like_selectors + rhs.class_like_selectors,
            element_selectors:
                self.element_selectors + rhs.element_selectors,
        }
    }
}

impl Default for Specificity {
    fn default() -> Specificity {
        Specificity {
            id_selectors: 0,
            class_like_selectors: 0,
            element_selectors: 0,
        }
    }
}

impl From<u32> for Specificity {
    fn from(value: u32) -> Specificity {
        assert!(value <= MAX_10BIT << 20 | MAX_10BIT << 10 | MAX_10BIT);
        Specificity {
            id_selectors: value >> 20,
            class_like_selectors: (value >> 10) & MAX_10BIT,
            element_selectors: value & MAX_10BIT,
        }
    }
}

impl From<Specificity> for u32 {
    fn from(specificity: Specificity) -> u32 {
        cmp::min(specificity.id_selectors, MAX_10BIT) << 20
        | cmp::min(specificity.class_like_selectors, MAX_10BIT) << 10
        | cmp::min(specificity.element_selectors, MAX_10BIT)
    }
}

fn specificity<Impl>(complex_selector: &ComplexSelector<Impl>,
                     pseudo_element: Option<&Impl::PseudoElement>)
                     -> u32
				     where Impl: SelectorImpl {
    let mut specificity = complex_selector_specificity(complex_selector);
    if pseudo_element.is_some() {
        specificity.element_selectors += 1;
    }
    specificity.into()
}

fn complex_selector_specificity<Impl>(mut selector: &ComplexSelector<Impl>)
                                      -> Specificity
                                      where Impl: SelectorImpl {
    fn compound_selector_specificity<Impl>(compound_selector: &[SimpleSelector<Impl>],
                                           specificity: &mut Specificity)
                                           where Impl: SelectorImpl {
        for simple_selector in compound_selector.iter() {
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
                SimpleSelector::Negation(ref negated) => {
                    let max =
                        negated.iter().map(|s| complex_selector_specificity(&s))
                               .max().unwrap();
                    *specificity = *specificity + max;
                }
            }
        }
    }

    let mut specificity = Default::default();
    compound_selector_specificity(&selector.compound_selector,
                              &mut specificity);
    loop {
        match selector.next {
            None => break,
            Some((ref next_selector, _)) => {
                selector = &**next_selector;
                compound_selector_specificity(&selector.compound_selector,
                                          &mut specificity)
            }
        }
    }
    specificity
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
fn parse_selector<Impl>(context: &ParserContext<Impl>,
                        input: &mut Parser)
                        -> Result<Selector<Impl>, ()>
    where Impl: SelectorImpl
{
    let (complex, pseudo_element) =
        try!(parse_complex_selector_and_pseudo_element(context, input));
    Ok(Selector {
        specificity: specificity(&complex, pseudo_element.as_ref()),
        complex_selector: Arc::new(complex),
        pseudo_element: pseudo_element,
    })
}

fn parse_complex_selector_and_pseudo_element<Impl>(
        context: &ParserContext<Impl>,
        input: &mut Parser)
        -> Result<(ComplexSelector<Impl>, Option<Impl::PseudoElement>), ()>
    where Impl: SelectorImpl
{
    let (first, mut pseudo_element) = try!(parse_compound_selector(context, input));
    let mut complex = ComplexSelector{ compound_selector: first, next: None };

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
        let (compound_selector, pseudo) = try!(parse_compound_selector(context, input));
        complex = ComplexSelector {
            compound_selector: compound_selector,
            next: Some((Arc::new(complex), combinator))
        };
        pseudo_element = pseudo;
    }

    Ok((complex, pseudo_element))
}

fn parse_complex_selector<Impl>(
        context: &ParserContext<Impl>,
        input: &mut Parser)
        -> Result<ComplexSelector<Impl>, ()>
    where Impl: SelectorImpl
{
    let (complex, pseudo_element) =
        try!(parse_complex_selector_and_pseudo_element(context, input));
    if pseudo_element.is_some() {
        return Err(())
    }
    Ok(complex)
}

/// * `Err(())`: Invalid selector, abort
/// * `Ok(None)`: Not a type selector, could be something else. `input` was not consumed.
/// * `Ok(Some(vec))`: Length 0 (`*|*`), 1 (`*|E` or `ns|*`) or 2 (`|E` or `ns|E`)
fn parse_type_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>, input: &mut Parser)
                       -> Result<Option<Vec<SimpleSelector<Impl>>>, ()> {
    match try!(parse_qualified_name(context, input, /* in_attr_selector = */ false)) {
        None => Ok(None),
        Some((namespace, local_name)) => {
            let mut compound_selector = vec!();
            match namespace {
                NamespaceConstraint::Specific(ns) => {
                    compound_selector.push(SimpleSelector::Namespace(ns))
                },
                NamespaceConstraint::Any => (),
            }
            match local_name {
                Some(name) => {
                    compound_selector.push(SimpleSelector::LocalName(LocalName {
                        lower_name: from_ascii_lowercase(&name),
                        name: from_cow_str(name),
                    }))
                }
                None => (),
            }
            Ok(Some(compound_selector))
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
            Some(ref url) => NamespaceConstraint::Specific(Namespace {
                prefix: None,
                url: url.clone()
            }),
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
                    let prefix = from_cow_str(value);
                    let result = context.namespace_prefixes.get(&prefix);
                    let url = try!(result.ok_or(()));
                    explicit_namespace(input, NamespaceConstraint::Specific(Namespace {
                        prefix: Some(prefix),
                        url: url.clone()
                    }))
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
            lower_name: from_ascii_lowercase(&local_name),
            name: from_cow_str(local_name),
        },
    };

    fn parse_value<Impl: SelectorImpl>(input: &mut Parser) -> Result<Impl::AttrValue, ()> {
        Ok(from_cow_str(try!(input.expect_ident_or_string())))
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
    input.parse_comma_separated(|input| parse_complex_selector(context, input).map(Arc::new))
         .map(SimpleSelector::Negation)
}

/// simple_selector_sequence
/// : [ type_selector | universal ] [ HASH | class | attrib | pseudo | negation ]*
/// | [ HASH | class | attrib | pseudo | negation ]+
///
/// `Err(())` means invalid selector
fn parse_compound_selector<Impl: SelectorImpl>(context: &ParserContext<Impl>,
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
    let mut compound_selector = match try!(parse_type_selector(context, input)) {
        None => {
            match context.default_namespace {
                // If there was no explicit type selector, but there is a
                // default namespace, there is an implicit "<defaultns>|*" type
                // selector.
                Some(ref url) => vec![SimpleSelector::Namespace(Namespace {
                    prefix: None,
                    url: url.clone()
                })],
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
                compound_selector.push(s);
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
        Ok((compound_selector, pseudo_element))
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
            let id = SimpleSelector::ID(from_cow_str(id));
            Ok(Some(SimpleSelectorParseResult::SimpleSelector(id)))
        }
        Ok(Token::Delim('.')) => {
            match input.next_including_whitespace() {
                Ok(Token::Ident(class)) => {
                    let class = SimpleSelector::Class(from_cow_str(class));
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
    use std::fmt;
    use std::sync::Arc;
    use cssparser::{Parser, ToCss, serialize_identifier};
    use super::*;

    #[derive(PartialEq, Clone, Debug, Hash, Eq)]
    pub enum PseudoClass {
        ServoNonZeroBorder,
        Lang(String),
    }

    #[derive(Eq, PartialEq, Clone, Debug, Hash)]
    pub enum PseudoElement {
        Before,
        After,
    }

    impl ToCss for PseudoClass {
        fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
            match *self {
                PseudoClass::ServoNonZeroBorder => dest.write_str(":-servo-nonzero-border"),
                PseudoClass::Lang(ref lang) => {
                    try!(dest.write_str(":lang("));
                    try!(serialize_identifier(lang, dest));
                    dest.write_char(')')
                }
            }
        }
    }

    impl ToCss for PseudoElement {
        fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
            match *self {
                PseudoElement::Before => dest.write_str("::before"),
                PseudoElement::After => dest.write_str("::after"),
            }
        }
    }

    #[derive(PartialEq, Debug)]
    pub struct DummySelectorImpl;

    impl SelectorImpl for DummySelectorImpl {
        type AttrValue = String;
        type Identifier = String;
        type ClassName = String;
        type LocalName = String;
        type NamespaceUrl = String;
        type NamespacePrefix = String;
        type BorrowedLocalName = str;
        type BorrowedNamespaceUrl = str;

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
        let result = parse_selector_list(context, &mut Parser::new(input));
        if let Ok(ref selectors) = result {
            assert_eq!(selectors.len(), 1);
            assert_eq!(selectors[0].to_css_string(), input);
        }
        result
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
    const SVG: &'static str = "http://www.w3.org/2000/svg";

    #[test]
    fn test_parsing() {
        assert_eq!(parse(""), Err(())) ;
        assert_eq!(parse(":lang(4)"), Err(())) ;
        assert_eq!(parse(":lang(en US)"), Err(())) ;
        assert_eq!(parse("EeÉ"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::LocalName(LocalName {
                    name: String::from("EeÉ"),
                    lower_name: String::from("eeÉ") })),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse(".foo:lang(en-US)"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec![
                    SimpleSelector::Class(String::from("foo")),
                    SimpleSelector::NonTSPseudoClass(PseudoClass::Lang("en-US".to_owned()))
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 2, 0),
        })));
        assert_eq!(parse("#bar"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::ID(String::from("bar"))),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(1, 0, 0),
        })));
        assert_eq!(parse("e.foo#bar"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::LocalName(LocalName {
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
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::ID(String::from("bar"))),
                next: Some((Arc::new(ComplexSelector {
                    compound_selector: vec!(SimpleSelector::LocalName(LocalName {
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
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::AttrExists(AttrSelector {
                    name: String::from("Foo"),
                    lower_name: String::from("foo"),
                    namespace: NamespaceConstraint::Specific(Namespace {
                        prefix: None,
                        url: "".into(),
                    }),
                })),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        })));
        assert_eq!(parse_ns("svg|circle", &context), Err(()));
        context.namespace_prefixes.insert("svg".into(), SVG.into());
        assert_eq!(parse_ns("svg|circle", &context), Ok(vec![Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec![
                    SimpleSelector::Namespace(Namespace {
                        prefix: Some("svg".into()),
                        url: SVG.into(),
                    }),
                    SimpleSelector::LocalName(LocalName {
                        name: String::from("circle"),
                        lower_name: String::from("circle")
                    })
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 0, 1),
        }]));
        // Default namespace does not apply to attribute selectors
        // https://github.com/mozilla/servo/pull/1652
        // but it does apply to implicit type selectors
        // https://github.com/servo/rust-selectors/pull/82
        context.default_namespace = Some(MATHML.into());
        assert_eq!(parse_ns("[Foo]", &context), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec![
                    SimpleSelector::Namespace(Namespace {
                        prefix: None,
                        url: MATHML.into(),
                    }),
                    SimpleSelector::AttrExists(AttrSelector {
                        name: String::from("Foo"),
                        lower_name: String::from("foo"),
                        namespace: NamespaceConstraint::Specific(Namespace {
                            prefix: None,
                            url: "".into(),
                        }),
                    }),
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        })));
        // Default namespace does apply to type selectors
        assert_eq!(parse_ns("e", &context), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(
                    SimpleSelector::Namespace(Namespace {
                        prefix: None,
                        url: MATHML.into(),
                    }),
                    SimpleSelector::LocalName(LocalName {
                        name: String::from("e"),
                        lower_name: String::from("e") }),
                ),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse("[attr |= \"foo\"]"), Ok(vec![Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec![
                    SimpleSelector::AttrDashMatch(AttrSelector {
                        name: String::from("attr"),
                        lower_name: String::from("attr"),
                        namespace: NamespaceConstraint::Specific(Namespace {
                            prefix: None,
                            url: "".into(),
                        }),
                    }, "foo".to_owned())
                ],
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(0, 1, 0),
        }]));
        // https://github.com/mozilla/servo/issues/1723
        assert_eq!(parse("::before"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(),
                next: None,
            }),
            pseudo_element: Some(PseudoElement::Before),
            specificity: specificity(0, 0, 1),
        })));
        assert_eq!(parse("div ::after"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(),
                next: Some((Arc::new(ComplexSelector {
                    compound_selector: vec!(SimpleSelector::LocalName(LocalName {
                        name: String::from("div"),
                        lower_name: String::from("div") })),
                    next: None,
                }), Combinator::Descendant)),
            }),
            pseudo_element: Some(PseudoElement::After),
            specificity: specificity(0, 0, 2),
        })));
        assert_eq!(parse("#d1 > .ok"), Ok(vec![Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec![
                    SimpleSelector::Class(String::from("ok")),
                ],
                next: Some((Arc::new(ComplexSelector {
                    compound_selector: vec![
                        SimpleSelector::ID(String::from("d1")),
                    ],
                    next: None,
                }), Combinator::Child)),
            }),
            pseudo_element: None,
            specificity: (1 << 20) + (1 << 10) + (0 << 0),
        }]));
        assert_eq!(parse(":not(.babybel, #provel.old)"), Ok(vec!(Selector {
            complex_selector: Arc::new(ComplexSelector {
                compound_selector: vec!(SimpleSelector::Negation(
                    vec!(
                        Arc::new(ComplexSelector {
                            compound_selector: vec!(SimpleSelector::Class(String::from("babybel"))),
                            next: None
                        }),
                        Arc::new(ComplexSelector {
                            compound_selector: vec!(
                                SimpleSelector::ID(String::from("provel")),
                                SimpleSelector::Class(String::from("old")),
                            ),
                            next: None
                        }),
                    )
                )),
                next: None,
            }),
            pseudo_element: None,
            specificity: specificity(1, 1, 0),
        })));
    }
}
