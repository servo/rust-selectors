/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::hash::Hash;
use std::sync::Arc;

use bloom::BloomFilter;
use smallvec::VecLike;
use quickersort::sort_by;

use parser::{CaseSensitivity, Combinator, ComplexSelector, LocalName};
use parser::{SimpleSelector, Selector, SelectorImpl};
use tree::Element;
use HashMap;

/// Map element data to Rules whose last simple selector starts with them.
///
/// e.g.,
/// "p > img" would go into the set of Rules corresponding to the
/// element "img"
/// "a .foo .bar.baz" would go into the set of Rules corresponding to
/// the class "bar"
///
/// Because we match Rules right-to-left (i.e., moving up the tree
/// from an element), we need to compare the last simple selector in the
/// Rule with the element.
///
/// So, if an element has ID "id1" and classes "foo" and "bar", then all
/// the rules it matches will have their last simple selector starting
/// either with "#id1" or with ".foo" or with ".bar".
///
/// Hence, the union of the rules keyed on each of element's classes, ID,
/// element name, etc. will contain the Rules that actually match that
/// element.
#[derive(Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct SelectorMap<T, Impl: SelectorImpl> {
    // TODO: Tune the initial capacity of the HashMap
    id_hash: HashMap<Impl::Identifier, Vec<Rule<T, Impl>>>,
    class_hash: HashMap<Impl::ClassName, Vec<Rule<T, Impl>>>,
    local_name_hash: HashMap<Impl::LocalName, Vec<Rule<T, Impl>>>,
    /// Same as local_name_hash, but keys are lower-cased.
    /// For HTML elements in HTML documents.
    lower_local_name_hash: HashMap<Impl::LocalName, Vec<Rule<T, Impl>>>,
    /// Rules that don't have ID, class, or element selectors.
    other_rules: Vec<Rule<T, Impl>>,
    /// Whether this hash is empty.
    empty: bool,
}

#[inline]
fn compare<T>(a: &DeclarationBlock<T>, b: &DeclarationBlock<T>) -> Ordering {
    (a.specificity, a.source_order).cmp(&(b.specificity, b.source_order))
}

impl<T, Impl: SelectorImpl> SelectorMap<T, Impl> {
    pub fn new() -> SelectorMap<T, Impl> {
        SelectorMap {
            id_hash: HashMap::default(),
            class_hash: HashMap::default(),
            local_name_hash: HashMap::default(),
            lower_local_name_hash: HashMap::default(),
            other_rules: vec!(),
            empty: true,
        }
    }

    /// Append to `rule_list` all Rules in `self` that match element.
    ///
    /// Extract matching rules as per element's ID, classes, tag name, etc..
    /// Sort the Rules at the end to maintain cascading order.
    pub fn get_all_matching_rules<E, V>(&self,
                                        element: &E,
                                        parent_bf: Option<&BloomFilter>,
                                        matching_rules_list: &mut V,
                                        relations: &mut StyleRelations)
        where E: Element<Impl=Impl>,
              V: VecLike<DeclarationBlock<T>>
    {
        if self.empty {
            return
        }

        // At the end, we're going to sort the rules that we added, so remember where we began.
        let init_len = matching_rules_list.len();
        if let Some(id) = element.get_id() {
            SelectorMap::get_matching_rules_from_hash(element,
                                                      parent_bf,
                                                      &self.id_hash,
                                                      &id,
                                                      matching_rules_list,
                                                      relations)
        }

        element.each_class(|class| {
            SelectorMap::get_matching_rules_from_hash(element,
                                                      parent_bf,
                                                      &self.class_hash,
                                                      class,
                                                      matching_rules_list,
                                                      relations);
        });

        let local_name_hash = if element.is_html_element_in_html_document() {
            &self.lower_local_name_hash
        } else {
            &self.local_name_hash
        };
        SelectorMap::get_matching_rules_from_hash(element,
                                                  parent_bf,
                                                  local_name_hash,
                                                  &element.get_local_name(),
                                                  matching_rules_list,
                                                  relations);

        SelectorMap::get_matching_rules(element,
                                        parent_bf,
                                        &self.other_rules,
                                        matching_rules_list,
                                        relations);

        // Sort only the rules we just added.
        sort_by(&mut matching_rules_list[init_len..], &compare);
    }

    /// Append to `rule_list` all universal Rules (rules with selector `*|*`) in
    /// `self` sorted by specifity and source order.
    pub fn get_universal_rules<V>(&self,
                                  matching_rules_list: &mut V)
        where V: VecLike<DeclarationBlock<T>>
    {
        if self.empty {
            return
        }

        let init_len = matching_rules_list.len();

        for rule in self.other_rules.iter() {
            if rule.selector.compound_selector.is_empty() &&
               rule.selector.next.is_none() {
                matching_rules_list.push(rule.declarations.clone());
            }
        }

        sort_by(&mut matching_rules_list[init_len..], &compare);
    }

    fn get_matching_rules_from_hash<E, Str, BorrowedStr: ?Sized, Vector>(
        element: &E,
        parent_bf: Option<&BloomFilter>,
        hash: &HashMap<Str, Vec<Rule<T, Impl>>>,
        key: &BorrowedStr,
        matching_rules: &mut Vector,
        relations: &mut StyleRelations)
        where E: Element<Impl=Impl>,
              Str: Borrow<BorrowedStr> + Eq + Hash,
              BorrowedStr: Eq + Hash,
              Vector: VecLike<DeclarationBlock<T>>
    {
        if let Some(rules) = hash.get(key) {
            SelectorMap::get_matching_rules(element,
                                            parent_bf,
                                            rules,
                                            matching_rules,
                                            relations)
        }
    }

    /// Adds rules in `rules` that match `element` to the `matching_rules` list.
    fn get_matching_rules<E, V>(element: &E,
                                parent_bf: Option<&BloomFilter>,
                                rules: &[Rule<T, Impl>],
                                matching_rules: &mut V,
                                relations: &mut StyleRelations)
        where E: Element<Impl=Impl>,
              V: VecLike<DeclarationBlock<T>>
    {
        for rule in rules.iter() {
            if matches_complex_selector(&*rule.selector,
                                        element, parent_bf, relations) {
                matching_rules.push(rule.declarations.clone());
            }
        }
    }

    /// Insert rule into the correct hash.
    /// Order in which to try: id_hash, class_hash, local_name_hash, other_rules.
    pub fn insert(&mut self, rule: Rule<T, Impl>) {
        self.empty = false;

        if let Some(id_name) = SelectorMap::get_id_name(&rule) {
            find_push(&mut self.id_hash, id_name, rule);
            return;
        }

        if let Some(class_name) = SelectorMap::get_class_name(&rule) {
            find_push(&mut self.class_hash, class_name, rule);
            return;
        }

        if let Some(LocalName { name, lower_name }) = SelectorMap::get_local_name(&rule) {
            find_push(&mut self.local_name_hash, name, rule.clone());
            find_push(&mut self.lower_local_name_hash, lower_name, rule);
            return;
        }

        self.other_rules.push(rule);
    }

    /// Retrieve the first ID name in Rule, or None otherwise.
    fn get_id_name(rule: &Rule<T, Impl>) -> Option<Impl::Identifier> {
        for ss in &rule.selector.compound_selector {
            // TODO(pradeep): Implement case-sensitivity based on the
            // document type and quirks mode.
            if let SimpleSelector::ID(ref id) = *ss {
                return Some(id.clone());
            }
        }

        None
    }

    /// Retrieve the FIRST class name in Rule, or None otherwise.
    fn get_class_name(rule: &Rule<T, Impl>) -> Option<Impl::ClassName> {
        for ss in &rule.selector.compound_selector {
            // TODO(pradeep): Implement case-sensitivity based on the
            // document type and quirks mode.
            if let SimpleSelector::Class(ref class) = *ss {
                return Some(class.clone());
            }
        }

        None
    }

    /// Retrieve the name if it is a type selector, or None otherwise.
    fn get_local_name(rule: &Rule<T, Impl>) -> Option<LocalName<Impl>> {
        for ss in &rule.selector.compound_selector {
            if let SimpleSelector::LocalName(ref n) = *ss {
                return Some(LocalName {
                    name: n.name.clone(),
                    lower_name: n.lower_name.clone(),
                })
            }
        }

        None
    }
}

// The bloom filter for descendant CSS selectors will have a <1% false
// positive rate until it has this many selectors in it, then it will
// rapidly increase.
pub static RECOMMENDED_SELECTOR_BLOOM_FILTER_SIZE: usize = 4096;

#[derive(Debug)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct Rule<T, Impl: SelectorImpl> {
    // This is an Arc because Rule will essentially be cloned for every element
    // that it matches. Selector contains an owned vector (through
    // ComplexSelector) and we want to avoid the allocation.
    pub selector: Arc<ComplexSelector<Impl>>,
    pub declarations: DeclarationBlock<T>,
}

/// A property declaration together with its precedence among rules of equal specificity so that
/// we can sort them.
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
#[derive(Debug)]
pub struct DeclarationBlock<T> {
    pub declarations: Arc<T>,
    pub source_order: usize,
    pub specificity: u32,
}

// FIXME(https://github.com/rust-lang/rust/issues/7671)
// derive(Clone) requires T: Clone, even though Arc<T>: T regardless.
impl<T> Clone for DeclarationBlock<T> {
    fn clone(&self) -> DeclarationBlock<T> {
        DeclarationBlock {
            declarations: self.declarations.clone(),
            source_order: self.source_order,
            specificity: self.specificity,
        }
    }
}

// FIXME(https://github.com/rust-lang/rust/issues/7671)
impl<T, Impl: SelectorImpl> Clone for Rule<T, Impl> {
    fn clone(&self) -> Rule<T, Impl> {
        Rule {
            selector: self.selector.clone(),
            declarations: self.declarations.clone(),
        }
    }
}

impl<T> DeclarationBlock<T> {
    #[inline]
    pub fn from_declarations(declarations: Arc<T>) -> DeclarationBlock<T> {
        DeclarationBlock {
            declarations: declarations,
            source_order: 0,
            specificity: 0,
        }
    }
}

bitflags! {
    /// Set of flags that determine the different kind of elements affected by
    /// the selector matching process.
    ///
    /// This is used to implement efficient sharing.
    pub flags StyleRelations: u16 {
        /// Whether this element has matched any rule that is determined by a
        /// sibling (when using the `+` or `~` combinators).
        const AFFECTED_BY_SIBLINGS = 1 << 0,

        /// Whether this element has matched any rule whose matching is
        /// determined by its position in the tree (i.e., first-child,
        /// nth-child, etc.).
        ///
        /// XXX improve this description and name, "position" is too generic.
        const AFFECTED_BY_CHILD_INDEX = 1 << 1,

        /// Whether this flag is affected by any state (i.e., non
        /// tree-structural pseudo-class).
        const AFFECTED_BY_STATE = 1 << 2,

        /// Whether this element is affected by an ID selector.
        const AFFECTED_BY_ID_SELECTOR = 1 << 3,

        /// Whether this element is affected by a non-common style-affecting
        /// attribute.
        const AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR = 1 << 4,

        /// Whether this element matches the :empty pseudo class.
        const AFFECTED_BY_EMPTY = 1 << 5,

        /// Whether this element has a style attribute. Computed
        /// externally.
        const AFFECTED_BY_STYLE_ATTRIBUTE = 1 << 6,

        /// Whether this element is affected by presentational hints. This is
        /// computed externally (that is, in Servo).
        const AFFECTED_BY_PRESENTATIONAL_HINTS = 1 << 7,

        /// Whether this element has pseudo-element styles. Computed externally.
        const AFFECTED_BY_PSEUDO_ELEMENTS = 1 << 8,
    }
}

bitflags! {
    /// Set of flags that are set on the parent depending on whether a child
    /// matches a selector.
    ///
    /// These setters, in the case of Servo, must be atomic, due to the parallel
    /// traversal.
    pub flags ElementFlags: u8 {
        /// When a child is added or removed from this element, all the children
        /// must be restyled, because they may match :nth-last-child,
        /// :last-of-type, :nth-last-of-type, or :only-of-type.
        const HAS_SLOW_SELECTOR = 1 << 0,

        /// When a child is added or removed from this element, any later
        /// children must be restyled, because they may match :nth-child,
        /// :first-of-type, or :nth-of-type.
        const HAS_SLOW_SELECTOR_LATER_SIBLINGS = 1 << 1,

        /// When a child is added or removed from this element, the first and
        /// last children must be restyled, because they may match :first-child,
        /// :last-child, or :only-child.
        const HAS_EDGE_CHILD_SELECTOR = 1 << 2,
    }
}

pub fn matches<E>(selector_list: &[Selector<E::Impl>],
                  element: &E,
                  parent_bf: Option<&BloomFilter>)
                  -> bool
                  where E: Element {
    selector_list.iter().any(|selector| {
        selector.pseudo_element.is_none() &&
        matches_complex_selector(&*selector.complex_selector, element, parent_bf, &mut StyleRelations::empty())
    })
}

/// Determines whether the given element matches the given complex selector.
///
/// NB: If you add support for any new kinds of selectors to this routine, be sure to set
/// `shareable` to false unless you are willing to update the style sharing logic. Otherwise things
/// will almost certainly break as elements will start mistakenly sharing styles. (See
/// `can_share_style_with` in `servo/components/style/matching.rs`.)
pub fn matches_complex_selector<E>(selector: &ComplexSelector<E::Impl>,
                                   element: &E,
                                   parent_bf: Option<&BloomFilter>,
                                   relations: &mut StyleRelations)
                                   -> bool
    where E: Element
{
    match matches_complex_selector_internal(selector, element, parent_bf, relations) {
        SelectorMatchingResult::Matched => {
            match selector.next {
                Some((_, Combinator::NextSibling)) |
                Some((_, Combinator::LaterSibling)) => *relations |= AFFECTED_BY_SIBLINGS,
                _ => {}
            }

            true
        }
        _ => false
    }
}

/// A result of selector matching, includes 3 failure types,
///
///   NotMatchedAndRestartFromClosestLaterSibling
///   NotMatchedAndRestartFromClosestDescendant
///   NotMatchedGlobally
///
/// When NotMatchedGlobally appears, stop selector matching completely since
/// the succeeding selectors never matches.
/// It is raised when
///   Child combinator cannot find the candidate element.
///   Descendant combinator cannot find the candidate element.
///
/// When NotMatchedAndRestartFromClosestDescendant appears, the selector
/// matching does backtracking and restarts from the closest Descendant
/// combinator.
/// It is raised when
///   NextSibling combinator cannot find the candidate element.
///   LaterSibling combinator cannot find the candidate element.
///   Child combinator doesn't match on the found element.
///
/// When NotMatchedAndRestartFromClosestLaterSibling appears, the selector
/// matching does backtracking and restarts from the closest LaterSibling
/// combinator.
/// It is raised when
///   NextSibling combinator doesn't match on the found element.
///
/// For example, when the selector "d1 d2 a" is provided and we cannot *find*
/// an appropriate ancestor element for "d1", this selector matching raises
/// NotMatchedGlobally since even if "d2" is moved to more upper element, the
/// candidates for "d1" becomes less than before and d1 .
///
/// The next example is siblings. When the selector "b1 + b2 ~ d1 a" is
/// provided and we cannot *find* an appropriate brother element for b1,
/// the selector matching raises NotMatchedAndRestartFromClosestDescendant.
/// The selectors ("b1 + b2 ~") doesn't match and matching restart from "d1".
///
/// The additional example is child and sibling. When the selector
/// "b1 + c1 > b2 ~ d1 a" is provided and the selector "b1" doesn't match on
/// the element, this "b1" raises NotMatchedAndRestartFromClosestLaterSibling.
/// However since the selector "c1" raises
/// NotMatchedAndRestartFromClosestDescendant. So the selector
/// "b1 + c1 > b2 ~ " doesn't match and restart matching from "d1".
#[derive(PartialEq, Eq, Copy, Clone)]
enum SelectorMatchingResult {
    Matched,
    NotMatchedAndRestartFromClosestLaterSibling,
    NotMatchedAndRestartFromClosestDescendant,
    NotMatchedGlobally,
}

/// Quickly figures out whether or not the complex selector is worth doing more
/// work on. If the simple selectors don't match, or there's a child selector
/// that does not appear in the bloom parent bloom filter, we can exit early.
fn can_fast_reject<E>(mut selector: &ComplexSelector<E::Impl>,
                      element: &E,
                      parent_bf: Option<&BloomFilter>,
                      relations: &mut StyleRelations)
                      -> Option<SelectorMatchingResult>
    where E: Element
{
    if !selector.compound_selector.iter().all(|simple_selector| {
      matches_simple_selector(simple_selector, element, parent_bf, relations) }) {
        return Some(SelectorMatchingResult::NotMatchedAndRestartFromClosestLaterSibling);
    }

    let bf: &BloomFilter = match parent_bf {
        None => return None,
        Some(ref bf) => bf,
    };

    // See if the bloom filter can exclude any of the descendant selectors, and
    // reject if we can.
    loop {
         match selector.next {
             None => break,
             Some((ref cs, Combinator::Descendant)) => selector = &**cs,
             Some((ref cs, _)) => {
                 selector = &**cs;
                 continue;
             }
         };

        for ss in selector.compound_selector.iter() {
            match *ss {
                SimpleSelector::LocalName(LocalName { ref name, ref lower_name })  => {
                    if !bf.might_contain(name)
                    && !bf.might_contain(lower_name) {
                        return Some(SelectorMatchingResult::NotMatchedGlobally);
                    }
                },
                SimpleSelector::Namespace(ref namespace) => {
                    if !bf.might_contain(&namespace.url) {
                        return Some(SelectorMatchingResult::NotMatchedGlobally);
                    }
                },
                SimpleSelector::ID(ref id) => {
                    if !bf.might_contain(id) {
                        return Some(SelectorMatchingResult::NotMatchedGlobally);
                    }
                },
                SimpleSelector::Class(ref class) => {
                    if !bf.might_contain(class) {
                        return Some(SelectorMatchingResult::NotMatchedGlobally);
                    }
                },
                _ => {},
            }
        }
    }

    // Can't fast reject.
    None
}

fn matches_complex_selector_internal<E>(selector: &ComplexSelector<E::Impl>,
                                         element: &E,
                                         parent_bf: Option<&BloomFilter>,
                                         relations: &mut StyleRelations)
                                         -> SelectorMatchingResult
     where E: Element
{
    if let Some(result) = can_fast_reject(selector, element, parent_bf, relations) {
        return result;
    }

    match selector.next {
        None => SelectorMatchingResult::Matched,
        Some((ref next_selector, combinator)) => {
            let (siblings, candidate_not_found) = match combinator {
                Combinator::Child => (false, SelectorMatchingResult::NotMatchedGlobally),
                Combinator::Descendant => (false, SelectorMatchingResult::NotMatchedGlobally),
                Combinator::NextSibling => (true, SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant),
                Combinator::LaterSibling => (true, SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant),
            };
            let mut next_element = if siblings {
                element.prev_sibling_element()
            } else {
                element.parent_element()
            };
            loop {
                let element = match next_element {
                    None => return candidate_not_found,
                    Some(next_element) => next_element,
                };
                let result = matches_complex_selector_internal(&**next_selector,
                                                                &element,
                                                                parent_bf,
                                                                relations);
                match (result, combinator) {
                    // Return the status immediately.
                    (SelectorMatchingResult::Matched, _) => return result,
                    (SelectorMatchingResult::NotMatchedGlobally, _) => return result,

                    // Upgrade the failure status to
                    // NotMatchedAndRestartFromClosestDescendant.
                    (_, Combinator::Child) => return SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant,

                    // Return the status directly.
                    (_, Combinator::NextSibling) => return result,

                    // If the failure status is NotMatchedAndRestartFromClosestDescendant
                    // and combinator is Combinator::LaterSibling, give up this Combinator::LaterSibling matching
                    // and restart from the closest descendant combinator.
                    (SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant, Combinator::LaterSibling) => return result,

                    // The Combinator::Descendant combinator and the status is
                    // NotMatchedAndRestartFromClosestLaterSibling or
                    // NotMatchedAndRestartFromClosestDescendant,
                    // or the Combinator::LaterSibling combinator and the status is
                    // NotMatchedAndRestartFromClosestDescendant
                    // can continue to matching on the next candidate element.
                    _ => {},
                }
                next_element = if siblings {
                    element.prev_sibling_element()
                } else {
                    element.parent_element()
                };
            }
        }
    }
}

/// Determines whether the given element matches the given single selector.
///
/// NB: If you add support for any new kinds of selectors to this routine, be sure to set
/// `shareable` to false unless you are willing to update the style sharing logic. Otherwise things
/// will almost certainly break as elements will start mistakenly sharing styles. (See
/// `can_share_style_with` in `servo/components/style/matching.rs`.)
#[inline]
fn matches_simple_selector<E>(
        selector: &SimpleSelector<E::Impl>,
        element: &E,
        parent_bf: Option<&BloomFilter>,
        relations: &mut StyleRelations)
        -> bool
    where E: Element
{
    macro_rules! relation_if {
        ($ex:expr, $flag:ident) => {
            if $ex {
                *relations |= $flag;
                true
            } else {
                false
            }
        }
    }

    match *selector {
        SimpleSelector::LocalName(LocalName { ref name, ref lower_name }) => {
            let name = if element.is_html_element_in_html_document() { lower_name } else { name };
            element.get_local_name() == name.borrow()
        }
        SimpleSelector::Namespace(ref namespace) => {
            element.get_namespace() == namespace.url.borrow()
        }
        // TODO: case-sensitivity depends on the document type and quirks mode
        SimpleSelector::ID(ref id) => {
            relation_if!(element.get_id().map_or(false, |attr| attr == *id),
                         AFFECTED_BY_ID_SELECTOR)
        }
        SimpleSelector::Class(ref class) => {
            element.has_class(class)
        }
        SimpleSelector::AttrExists(ref attr) => {
            let matches = element.match_attr_has(attr);

            if matches && !E::Impl::attr_exists_selector_is_shareable(attr) {
                *relations |= AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR;
            }

            matches
        }
        SimpleSelector::AttrEqual(ref attr, ref value, case_sensitivity) => {
            let matches = match case_sensitivity {
                CaseSensitivity::CaseSensitive => element.match_attr_equals(attr, value),
                CaseSensitivity::CaseInsensitive => element.match_attr_equals_ignore_ascii_case(attr, value),
            };

            if matches && !E::Impl::attr_equals_selector_is_shareable(attr, value) {
                *relations |= AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR;
            }

            matches
        }
        SimpleSelector::AttrIncludes(ref attr, ref value) => {
            relation_if!(element.match_attr_includes(attr, value),
                         AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR)
        }
        SimpleSelector::AttrDashMatch(ref attr, ref value) => {
            relation_if!(element.match_attr_dash(attr, value),
                         AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR)
        }
        SimpleSelector::AttrPrefixMatch(ref attr, ref value) => {
            relation_if!(element.match_attr_prefix(attr, value),
                         AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR)
        }
        SimpleSelector::AttrSubstringMatch(ref attr, ref value) => {
            relation_if!(element.match_attr_substring(attr, value),
                         AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR)
        }
        SimpleSelector::AttrSuffixMatch(ref attr, ref value) => {
            relation_if!(element.match_attr_suffix(attr, value),
                         AFFECTED_BY_NON_COMMON_STYLE_AFFECTING_ATTRIBUTE_SELECTOR)
        }
        SimpleSelector::NonTSPseudoClass(ref pc) => {
            relation_if!(element.match_non_ts_pseudo_class(pc.clone()),
                         AFFECTED_BY_STATE)
        }
        SimpleSelector::FirstChild => {
            relation_if!(matches_first_child(element), AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::LastChild => {
            relation_if!(matches_last_child(element), AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::OnlyChild => {
            relation_if!(matches_first_child(element) && matches_last_child(element), AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::Root => {
            // We never share styles with an element with no parent, so no point
            // in creating a new StyleRelation.
            element.is_root()
        }
        SimpleSelector::Empty => {
            relation_if!(element.is_empty(), AFFECTED_BY_EMPTY)
        }
        SimpleSelector::NthChild(a, b) => {
            relation_if!(matches_generic_nth_child(element, a, b, false, false),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::NthLastChild(a, b) => {
            relation_if!(matches_generic_nth_child(element, a, b, false, true),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::NthOfType(a, b) => {
            relation_if!(matches_generic_nth_child(element, a, b, true, false),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::NthLastOfType(a, b) => {
            relation_if!(matches_generic_nth_child(element, a, b, true, true),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::FirstOfType => {
            relation_if!(matches_generic_nth_child(element, 0, 1, true, false),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::LastOfType => {
            relation_if!(matches_generic_nth_child(element, 0, 1, true, true),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::OnlyOfType => {
            relation_if!(matches_generic_nth_child(element, 0, 1, true, false) &&
                         matches_generic_nth_child(element, 0, 1, true, true),
                         AFFECTED_BY_CHILD_INDEX)
        }
        SimpleSelector::Negation(ref negated) => {
            !negated.iter().all(|s| {
                matches_complex_selector(s, element, parent_bf, relations)
            })
        }
    }
}

#[inline]
fn matches_generic_nth_child<E>(element: &E,
                                a: i32,
                                b: i32,
                                is_of_type: bool,
                                is_from_end: bool) -> bool
    where E: Element
{
    // Selectors Level 4 changed from Level 3:
    // This can match without a parent element:
    // https://drafts.csswg.org/selectors-4/#child-index

    let mut index = 1;
    let mut next_sibling = if is_from_end {
        element.next_sibling_element()
    } else {
        element.prev_sibling_element()
    };

    loop {
        let sibling = match next_sibling {
            None => break,
            Some(next_sibling) => next_sibling
        };

        if is_of_type {
            if element.get_local_name() == sibling.get_local_name() &&
                element.get_namespace() == sibling.get_namespace() {
                index += 1;
            }
        } else {
          index += 1;
        }
        next_sibling = if is_from_end {
            sibling.next_sibling_element()
        } else {
            sibling.prev_sibling_element()
        };
    }

    let result = if a == 0 {
        b == index
    } else {
        (index - b) / a >= 0 &&
        (index - b) % a == 0
    };

    if result {
        if let Some(parent) = element.parent_element() {
            parent.insert_flags(if is_from_end {
                HAS_SLOW_SELECTOR
            } else {
                HAS_SLOW_SELECTOR_LATER_SIBLINGS
            });
        }
    }

    result
}

#[inline]
fn matches_first_child<E>(element: &E) -> bool where E: Element {
    // Selectors Level 4 changed from Level 3:
    // This can match without a parent element:
    // https://drafts.csswg.org/selectors-4/#child-index
    let result = element.prev_sibling_element().is_none();
    if result {
        if let Some(parent) = element.parent_element() {
            parent.insert_flags(HAS_EDGE_CHILD_SELECTOR);
        }
    }
    result
}

#[inline]
fn matches_last_child<E>(element: &E) -> bool where E: Element {
    // Selectors Level 4 changed from Level 3:
    // This can match without a parent element:
    // https://drafts.csswg.org/selectors-4/#child-index
    let result = element.next_sibling_element().is_none();
    if result {
        if let Some(parent) = element.parent_element() {
            parent.insert_flags(HAS_EDGE_CHILD_SELECTOR);
        }
    }
    result
}

fn find_push<T, Impl: SelectorImpl, Str: Eq + Hash>(
    map: &mut HashMap<Str, Vec<Rule<T, Impl>>>,
    key: Str,
    value: Rule<T, Impl>
) {
    if let Some(vec) = map.get_mut(&key) {
        vec.push(value);
        return
    }
    map.insert(key, vec![value]);
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use super::{DeclarationBlock, Rule, SelectorMap};
    use parser::LocalName;
    use parser::tests::DummySelectorImpl;
    use cssparser::Parser;
    use parser::ParserContext;

    /// Helper method to get some Rules from selector strings.
    /// Each sublist of the result contains the Rules for one StyleRule.
    fn get_mock_rules(css_selectors: &[&str]) -> Vec<Vec<Rule<(), DummySelectorImpl>>> {
        use parser::parse_selector_list;

        css_selectors.iter().enumerate().map(|(i, selectors)| {
            let context = ParserContext::new();
            parse_selector_list(&context, &mut Parser::new(*selectors))
            .unwrap().into_iter().map(|s| {
                Rule {
                    selector: s.complex_selector.clone(),
                    declarations: DeclarationBlock {
                        specificity: s.specificity,
                        declarations: Arc::new(()),
                        source_order: i,
                    }
                }
            }).collect()
        }).collect()
    }

    fn get_mock_map(selectors: &[&str]) -> SelectorMap<(), DummySelectorImpl> {
        let mut map = SelectorMap::new();
        let selector_rules = get_mock_rules(selectors);

        for rules in selector_rules.into_iter() {
            for rule in rules.into_iter() {
                map.insert(rule)
            }
        }

        map
    }

    #[test]
    fn test_rule_ordering_same_specificity(){
        let rules_list = get_mock_rules(&["a.intro", "img.sidebar"]);
        let a = &rules_list[0][0].declarations;
        let b = &rules_list[1][0].declarations;
        assert!((a.specificity, a.source_order) < ((b.specificity, b.source_order)),
                "The rule that comes later should win.");
    }


    #[test]
    fn test_get_id_name(){
        let rules_list = get_mock_rules(&[".intro", "#top"]);
        assert_eq!(SelectorMap::get_id_name(&rules_list[0][0]), None);
        assert_eq!(SelectorMap::get_id_name(&rules_list[1][0]), Some(String::from("top")));
    }

    #[test]
    fn test_get_class_name(){
        let rules_list = get_mock_rules(&[".intro.foo", "#top"]);
        assert_eq!(SelectorMap::get_class_name(&rules_list[0][0]), Some(String::from("intro")));
        assert_eq!(SelectorMap::get_class_name(&rules_list[1][0]), None);
    }

    #[test]
    fn test_get_local_name(){
        let rules_list = get_mock_rules(&["img.foo", "#top", "IMG", "ImG"]);
        let check = |i: usize, names: Option<(&str, &str)>| {
            assert!(SelectorMap::get_local_name(&rules_list[i][0])
                    == names.map(|(name, lower_name)| LocalName {
                            name: String::from(name),
                            lower_name: String::from(lower_name) }))
        };
        check(0, Some(("img", "img")));
        check(1, None);
        check(2, Some(("IMG", "img")));
        check(3, Some(("ImG", "img")));
    }

    #[test]
    fn test_insert(){
        let rules_list = get_mock_rules(&[".intro.foo", "#top"]);
        let mut selector_map = SelectorMap::new();
        selector_map.insert(rules_list[1][0].clone());
        assert_eq!(1, selector_map.id_hash.get("top").unwrap()[0].declarations.source_order);
        selector_map.insert(rules_list[0][0].clone());
        assert_eq!(0, selector_map.class_hash.get("intro").unwrap()[0].declarations.source_order);
        assert!(selector_map.class_hash.get("foo").is_none());
    }

    #[test]
    fn test_get_universal_rules(){
        let map = get_mock_map(&["*|*", "#foo > *|*", ".klass", "#id"]);
        let mut decls = vec![];

        map.get_universal_rules(&mut decls);

        assert_eq!(decls.len(), 1);
    }
}
