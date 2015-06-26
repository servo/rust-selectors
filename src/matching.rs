/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::AsciiExt;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::hash_state::DefaultState;
use std::default::Default;
use std::sync::Arc;

use bloom::BloomFilter;
use smallvec::VecLike;
use quicksort::quicksort_by;
use string_cache::Atom;

use fnv::FnvHasher;
use parser::{CaseSensitivity, Combinator, CompoundSelector, LocalName};
use parser::{SimpleSelector, Selector};
use tree::{TElement, TNode};

/// The definition of whitespace per CSS Selectors Level 3 § 4.
pub static SELECTOR_WHITESPACE: &'static [char] = &[' ', '\t', '\n', '\r', '\x0C'];

/// Map node attributes to Rules whose last simple selector starts with them.
///
/// e.g.,
/// "p > img" would go into the set of Rules corresponding to the
/// element "img"
/// "a .foo .bar.baz" would go into the set of Rules corresponding to
/// the class "bar"
///
/// Because we match Rules right-to-left (i.e., moving up the tree
/// from a node), we need to compare the last simple selector in the
/// Rule with the node.
///
/// So, if a node has ID "id1" and classes "foo" and "bar", then all
/// the rules it matches will have their last simple selector starting
/// either with "#id1" or with ".foo" or with ".bar".
///
/// Hence, the union of the rules keyed on each of node's classes, ID,
/// element name, etc. will contain the Rules that actually match that
/// node.
pub struct SelectorMap<T> {
    // TODO: Tune the initial capacity of the HashMap
    id_hash: HashMap<Atom, Vec<Rule<T>>, DefaultState<FnvHasher>>,
    class_hash: HashMap<Atom, Vec<Rule<T>>, DefaultState<FnvHasher>>,
    local_name_hash: HashMap<Atom, Vec<Rule<T>>, DefaultState<FnvHasher>>,
    /// Same as local_name_hash, but keys are lower-cased.
    /// For HTML elements in HTML documents.
    lower_local_name_hash: HashMap<Atom, Vec<Rule<T>>, DefaultState<FnvHasher>>,
    // For Rules that don't have ID, class, or element selectors.
    universal_rules: Vec<Rule<T>>,
    /// Whether this hash is empty.
    empty: bool,
}

impl<T> SelectorMap<T> {
    pub fn new() -> SelectorMap<T> {
        SelectorMap {
            id_hash: HashMap::with_hash_state(Default::default()),
            class_hash: HashMap::with_hash_state(Default::default()),
            local_name_hash: HashMap::with_hash_state(Default::default()),
            lower_local_name_hash: HashMap::with_hash_state(Default::default()),
            universal_rules: vec!(),
            empty: true,
        }
    }

    /// Append to `rule_list` all Rules in `self` that match node.
    ///
    /// Extract matching rules as per node's ID, classes, tag name, etc..
    /// Sort the Rules at the end to maintain cascading order.
    pub fn get_all_matching_rules<N,V>(&self,
                                       node: &N,
                                       parent_bf: &Option<Box<BloomFilter>>,
                                       matching_rules_list: &mut V,
                                       shareable: &mut bool)
                                       where N: TNode,
                                             V: VecLike<DeclarationBlock<T>> {
        if self.empty {
            return
        }

        // At the end, we're going to sort the rules that we added, so remember where we began.
        let init_len = matching_rules_list.len();
        let element = match node.as_element() {
            Some(e) => e,
            None => return,
        };

        match element.get_id() {
            Some(id) => {
                SelectorMap::get_matching_rules_from_hash(node,
                                                          parent_bf,
                                                          &self.id_hash,
                                                          &id,
                                                          matching_rules_list,
                                                          shareable)
            }
            None => {}
        }

        element.each_class(|class| {
            SelectorMap::get_matching_rules_from_hash(node,
                                                      parent_bf,
                                                      &self.class_hash,
                                                      class,
                                                      matching_rules_list,
                                                      shareable);
        });

        let local_name_hash = if node.is_html_element_in_html_document() {
            &self.lower_local_name_hash
        } else {
            &self.local_name_hash
        };
        SelectorMap::get_matching_rules_from_hash(node,
                                                  parent_bf,
                                                  local_name_hash,
                                                  element.get_local_name(),
                                                  matching_rules_list,
                                                  shareable);

        SelectorMap::get_matching_rules(node,
                                        parent_bf,
                                        &self.universal_rules,
                                        matching_rules_list,
                                        shareable);

        // Sort only the rules we just added.
        quicksort_by(&mut matching_rules_list[init_len..], compare);

        fn compare<T>(a: &DeclarationBlock<T>, b: &DeclarationBlock<T>) -> Ordering {
            (a.specificity, a.source_order).cmp(&(b.specificity, b.source_order))
        }
    }

    fn get_matching_rules_from_hash<N,V>(node: &N,
                                         parent_bf: &Option<Box<BloomFilter>>,
                                         hash: &HashMap<Atom,
                                                        Vec<Rule<T>>,
                                                        DefaultState<FnvHasher>>,
                                         key: &Atom,
                                         matching_rules: &mut V,
                                         shareable: &mut bool)
                                         where N: TNode,
                                               V: VecLike<DeclarationBlock<T>> {
        match hash.get(key) {
            Some(rules) => {
                SelectorMap::get_matching_rules(node,
                                                parent_bf,
                                                rules,
                                                matching_rules,
                                                shareable)
            }
            None => {}
        }
    }

    /// Adds rules in `rules` that match `node` to the `matching_rules` list.
    fn get_matching_rules<N,V>(node: &N,
                               parent_bf: &Option<Box<BloomFilter>>,
                               rules: &[Rule<T>],
                               matching_rules: &mut V,
                               shareable: &mut bool)
                               where N: TNode,
                                     V: VecLike<DeclarationBlock<T>> {
        for rule in rules.iter() {
            if matches_compound_selector(&*rule.selector, node, parent_bf, shareable) {
                matching_rules.push(rule.declarations.clone());
            }
        }
    }

    /// Insert rule into the correct hash.
    /// Order in which to try: id_hash, class_hash, local_name_hash, universal_rules.
    pub fn insert(&mut self, rule: Rule<T>) {
        self.empty = false;

        match SelectorMap::get_id_name(&rule) {
            Some(id_name) => {
                find_push(&mut self.id_hash, id_name, rule);
                return;
            }
            None => {}
        }
        match SelectorMap::get_class_name(&rule) {
            Some(class_name) => {
                find_push(&mut self.class_hash, class_name, rule);
                return;
            }
            None => {}
        }

        match SelectorMap::get_local_name(&rule) {
            Some(LocalName { name, lower_name }) => {
                find_push(&mut self.local_name_hash, name, rule.clone());
                find_push(&mut self.lower_local_name_hash, lower_name, rule);
                return;
            }
            None => {}
        }

        self.universal_rules.push(rule);
    }

    /// Retrieve the first ID name in Rule, or None otherwise.
    fn get_id_name(rule: &Rule<T>) -> Option<Atom> {
        let simple_selector_sequence = &rule.selector.simple_selectors;
        for ss in simple_selector_sequence.iter() {
            match *ss {
                // TODO(pradeep): Implement case-sensitivity based on the document type and quirks
                // mode.
                SimpleSelector::ID(ref id) => return Some(id.clone()),
                _ => {}
            }
        }
        return None
    }

    /// Retrieve the FIRST class name in Rule, or None otherwise.
    fn get_class_name(rule: &Rule<T>) -> Option<Atom> {
        let simple_selector_sequence = &rule.selector.simple_selectors;
        for ss in simple_selector_sequence.iter() {
            match *ss {
                // TODO(pradeep): Implement case-sensitivity based on the document type and quirks
                // mode.
                SimpleSelector::Class(ref class) => return Some(class.clone()),
                _ => {}
            }
        }
        return None
    }

    /// Retrieve the name if it is a type selector, or None otherwise.
    fn get_local_name(rule: &Rule<T>) -> Option<LocalName> {
        let simple_selector_sequence = &rule.selector.simple_selectors;
        for ss in simple_selector_sequence.iter() {
            match *ss {
                SimpleSelector::LocalName(ref name) => {
                    return Some(name.clone())
                }
                _ => {}
            }
        }
        return None
    }
}

// The bloom filter for descendant CSS selectors will have a <1% false
// positive rate until it has this many selectors in it, then it will
// rapidly increase.
pub static RECOMMENDED_SELECTOR_BLOOM_FILTER_SIZE: usize = 4096;


pub struct Rule<T> {
    // This is an Arc because Rule will essentially be cloned for every node
    // that it matches. Selector contains an owned vector (through
    // CompoundSelector) and we want to avoid the allocation.
    pub selector: Arc<CompoundSelector>,
    pub declarations: DeclarationBlock<T>,
}

/// A property declaration together with its precedence among rules of equal specificity so that
/// we can sort them.
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
impl<T> Clone for Rule<T> {
    fn clone(&self) -> Rule<T> {
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

pub fn matches<N>(selector_list: &Vec<Selector>,
                  element: &N,
                  parent_bf: &Option<Box<BloomFilter>>)
                  -> bool
                  where N: TNode {
    selector_list.iter().any(|selector| {
        selector.pseudo_element.is_none() &&
        matches_compound_selector(&*selector.compound_selectors, element, parent_bf, &mut false)
    })
}

/// Determines whether the given element matches the given single or compound selector.
///
/// NB: If you add support for any new kinds of selectors to this routine, be sure to set
/// `shareable` to false unless you are willing to update the style sharing logic. Otherwise things
/// will almost certainly break as nodes will start mistakenly sharing styles. (See the code in
/// `main/css/matching.rs`.)
fn matches_compound_selector<N>(selector: &CompoundSelector,
                                element: &N,
                                parent_bf: &Option<Box<BloomFilter>>,
                                shareable: &mut bool)
                                -> bool
                                where N: TNode {
    match matches_compound_selector_internal(selector, element, parent_bf, shareable) {
        SelectorMatchingResult::Matched => true,
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
/// an appropriate ancestor node for "d1", this selector matching raises
/// NotMatchedGlobally since even if "d2" is moved to more upper node, the
/// candidates for "d1" becomes less than before and d1 .
///
/// The next example is siblings. When the selector "b1 + b2 ~ d1 a" is
/// provided and we cannot *find* an appropriate brother node for b1,
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

/// Quickly figures out whether or not the compound selector is worth doing more
/// work on. If the simple selectors don't match, or there's a child selector
/// that does not appear in the bloom parent bloom filter, we can exit early.
fn can_fast_reject<N>(mut selector: &CompoundSelector,
                      element: &N,
                      parent_bf: &Option<Box<BloomFilter>>,
                      shareable: &mut bool)
                      -> Option<SelectorMatchingResult>
                      where N: TNode {
    if !selector.simple_selectors.iter().all(|simple_selector| {
      matches_simple_selector(simple_selector, element, shareable) }) {
        return Some(SelectorMatchingResult::NotMatchedAndRestartFromClosestLaterSibling);
    }

    let bf: &BloomFilter = match *parent_bf {
        None => return None,
        Some(ref bf) => &**bf,
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

        for ss in selector.simple_selectors.iter() {
            match *ss {
                SimpleSelector::LocalName(LocalName { ref name, ref lower_name })  => {
                    if !bf.might_contain(name)
                    && !bf.might_contain(lower_name) {
                        return Some(SelectorMatchingResult::NotMatchedGlobally);
                    }
                },
                SimpleSelector::Namespace(ref namespace) => {
                    if !bf.might_contain(namespace) {
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
    return None;
}

fn matches_compound_selector_internal<N>(selector: &CompoundSelector,
                                         element: &N,
                                         parent_bf: &Option<Box<BloomFilter>>,
                                         shareable: &mut bool)
                                         -> SelectorMatchingResult
                                         where N: TNode {
    match can_fast_reject(selector, element, parent_bf, shareable) {
        None => {},
        Some(result) => return result,
    };

    match selector.next {
        None => SelectorMatchingResult::Matched,
        Some((ref next_selector, combinator)) => {
            let (siblings, candidate_not_found) = match combinator {
                Combinator::Child => (false, SelectorMatchingResult::NotMatchedGlobally),
                Combinator::Descendant => (false, SelectorMatchingResult::NotMatchedGlobally),
                Combinator::NextSibling => (true, SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant),
                Combinator::LaterSibling => (true, SelectorMatchingResult::NotMatchedAndRestartFromClosestDescendant),
            };
            let mut node = (*element).clone();
            loop {
                let next_node = if siblings {
                    node.prev_sibling()
                } else {
                    node.parent_node()
                };
                match next_node {
                    None => return candidate_not_found,
                    Some(next_node) => node = next_node,
                }
                if node.is_element() {
                    let result = matches_compound_selector_internal(&**next_selector,
                                                                    &node,
                                                                    parent_bf,
                                                                    shareable);
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
                }
            }
        }
    }
}

bitflags! {
    flags CommonStyleAffectingAttributes: u8 {
        const HIDDEN_ATTRIBUTE = 0x01,
        const NO_WRAP_ATTRIBUTE = 0x02,
        const ALIGN_LEFT_ATTRIBUTE = 0x04,
        const ALIGN_CENTER_ATTRIBUTE = 0x08,
        const ALIGN_RIGHT_ATTRIBUTE = 0x10,
    }
}

pub struct CommonStyleAffectingAttributeInfo {
    pub atom: Atom,
    pub mode: CommonStyleAffectingAttributeMode,
}

#[derive(Copy, Clone)]
pub enum CommonStyleAffectingAttributeMode {
    IsPresent(CommonStyleAffectingAttributes),
    IsEqual(&'static str, CommonStyleAffectingAttributes),
}

// NB: This must match the order in `layout::css::matching::CommonStyleAffectingAttributes`.
#[inline]
pub fn common_style_affecting_attributes() -> [CommonStyleAffectingAttributeInfo; 5] {
    [
        CommonStyleAffectingAttributeInfo {
            atom: atom!("hidden"),
            mode: CommonStyleAffectingAttributeMode::IsPresent(HIDDEN_ATTRIBUTE),
        },
        CommonStyleAffectingAttributeInfo {
            atom: atom!("nowrap"),
            mode: CommonStyleAffectingAttributeMode::IsPresent(NO_WRAP_ATTRIBUTE),
        },
        CommonStyleAffectingAttributeInfo {
            atom: atom!("align"),
            mode: CommonStyleAffectingAttributeMode::IsEqual("left", ALIGN_LEFT_ATTRIBUTE),
        },
        CommonStyleAffectingAttributeInfo {
            atom: atom!("align"),
            mode: CommonStyleAffectingAttributeMode::IsEqual("center", ALIGN_CENTER_ATTRIBUTE),
        },
        CommonStyleAffectingAttributeInfo {
            atom: atom!("align"),
            mode: CommonStyleAffectingAttributeMode::IsEqual("right", ALIGN_RIGHT_ATTRIBUTE),
        }
    ]
}

/// Attributes that, if present, disable style sharing. All legacy HTML attributes must be in
/// either this list or `common_style_affecting_attributes`. See the comment in
/// `synthesize_presentational_hints_for_legacy_attributes`.
pub fn rare_style_affecting_attributes() -> [Atom; 3] {
    [ atom!("bgcolor"), atom!("border"), atom!("colspan") ]
}

/// Determines whether the given element matches the given single selector.
///
/// NB: If you add support for any new kinds of selectors to this routine, be sure to set
/// `shareable` to false unless you are willing to update the style sharing logic. Otherwise things
/// will almost certainly break as nodes will start mistakenly sharing styles. (See the code in
/// `main/css/matching.rs`.)
#[inline]
pub fn matches_simple_selector<N>(selector: &SimpleSelector,
                                  element: &N,
                                  shareable: &mut bool)
                                  -> bool
                                  where N: TNode {
    match *selector {
        SimpleSelector::LocalName(LocalName { ref name, ref lower_name }) => {
            element.as_element()
                   .map_or(false, |e| {
                       let name = if element.is_html_element_in_html_document() {
                            lower_name
                        } else {
                             name
                        };
                       e.get_local_name() == name
                   })
        }

        SimpleSelector::Namespace(ref namespace) => {
            element.as_element()
                   .map_or(false, |e| e.get_namespace() == namespace)
        }
        // TODO: case-sensitivity depends on the document type and quirks mode
        SimpleSelector::ID(ref id) => {
            *shareable = false;
            element.as_element()
                   .and_then(|e| e.get_id())
                   .map_or(false, |attr| attr == *id)
        }
        SimpleSelector::Class(ref class) => {
            element.as_element()
                   .map_or(false, |e| e.has_class(class))
        }

        SimpleSelector::AttrExists(ref attr) => {
            // NB(pcwalton): If you update this, remember to update the corresponding list in
            // `can_share_style_with()` as well.
            if common_style_affecting_attributes().iter().all(|common_attr_info| {
                !(common_attr_info.atom == attr.name && match common_attr_info.mode {
                    CommonStyleAffectingAttributeMode::IsPresent(_) => true,
                    CommonStyleAffectingAttributeMode::IsEqual(..) => false,
                })
            }) {
                *shareable = false;
            }
            element.match_attr(attr, |_| true)
        }
        SimpleSelector::AttrEqual(ref attr, ref value, case_sensitivity) => {
            if *value != "DIR" &&
                    common_style_affecting_attributes().iter().all(|common_attr_info| {
                        !(common_attr_info.atom == attr.name && match common_attr_info.mode {
                            CommonStyleAffectingAttributeMode::IsEqual(target_value, _) => *value == target_value,
                            CommonStyleAffectingAttributeMode::IsPresent(_) => false,
                        })
                    }) {
                // FIXME(pcwalton): Remove once we start actually supporting RTL text. This is in
                // here because the UA style otherwise disables all style sharing completely.
                *shareable = false
            }
            element.match_attr(attr, |attr_value| {
                match case_sensitivity {
                    CaseSensitivity::CaseSensitive => attr_value == *value,
                    CaseSensitivity::CaseInsensitive => attr_value.eq_ignore_ascii_case(value),
                }
            })
        }
        SimpleSelector::AttrIncludes(ref attr, ref value) => {
            *shareable = false;
            element.match_attr(attr, |attr_value| {
                attr_value.split(SELECTOR_WHITESPACE).any(|v| v == *value)
            })
        }
        SimpleSelector::AttrDashMatch(ref attr, ref value, ref dashing_value) => {
            *shareable = false;
            element.match_attr(attr, |attr_value| {
                attr_value == *value ||
                attr_value.starts_with(dashing_value)
            })
        }
        SimpleSelector::AttrPrefixMatch(ref attr, ref value) => {
            *shareable = false;
            element.match_attr(attr, |attr_value| {
                attr_value.starts_with(value)
            })
        }
        SimpleSelector::AttrSubstringMatch(ref attr, ref value) => {
            *shareable = false;
            element.match_attr(attr, |attr_value| {
                attr_value.contains(value)
            })
        }
        SimpleSelector::AttrSuffixMatch(ref attr, ref value) => {
            *shareable = false;
            element.match_attr(attr, |attr_value| {
                attr_value.ends_with(value)
            })
        }

        SimpleSelector::AnyLink => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.is_link())
        }
        SimpleSelector::Link => {
            element.as_element()
                   .map(|e| e.is_unvisited_link())
                   .unwrap_or(false)
        }
        SimpleSelector::Visited => {
            element.as_element()
                   .map_or(false, |e| e.is_visited_link())
        }
        // https://html.spec.whatwg.org/multipage/scripting.html#selector-hover
        SimpleSelector::Hover => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_hover_state())
        },
        // https://html.spec.whatwg.org/multipage/scripting.html#selector-focus
        SimpleSelector::Focus => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_focus_state())
        },
        // http://www.whatwg.org/html/#selector-disabled
        SimpleSelector::Disabled => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_disabled_state())
        },
        // http://www.whatwg.org/html/#selector-enabled
        SimpleSelector::Enabled => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_enabled_state())
        },
        // https://html.spec.whatwg.org/multipage/scripting.html#selector-checked
        SimpleSelector::Checked => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_checked_state())
        }
        // https://html.spec.whatwg.org/multipage/scripting.html#selector-indeterminate
        SimpleSelector::Indeterminate => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.get_indeterminate_state())
        }
        SimpleSelector::FirstChild => {
            *shareable = false;
            matches_first_child(element)
        }
        SimpleSelector::LastChild => {
            *shareable = false;
            matches_last_child(element)
        }
        SimpleSelector::OnlyChild => {
            *shareable = false;
            matches_first_child(element) && matches_last_child(element)
        }

        SimpleSelector::Root => {
            *shareable = false;
            matches_root(element)
        }

        SimpleSelector::NthChild(a, b) => {
            *shareable = false;
            matches_generic_nth_child(element, a, b, false, false)
        }
        SimpleSelector::NthLastChild(a, b) => {
            *shareable = false;
            matches_generic_nth_child(element, a, b, false, true)
        }
        SimpleSelector::NthOfType(a, b) => {
            *shareable = false;
            matches_generic_nth_child(element, a, b, true, false)
        }
        SimpleSelector::NthLastOfType(a, b) => {
            *shareable = false;
            matches_generic_nth_child(element, a, b, true, true)
        }

        SimpleSelector::FirstOfType => {
            *shareable = false;
            matches_generic_nth_child(element, 0, 1, true, false)
        }
        SimpleSelector::LastOfType => {
            *shareable = false;
            matches_generic_nth_child(element, 0, 1, true, true)
        }
        SimpleSelector::OnlyOfType => {
            *shareable = false;
            matches_generic_nth_child(element, 0, 1, true, false) &&
                matches_generic_nth_child(element, 0, 1, true, true)
        }

        SimpleSelector::ServoNonzeroBorder => {
            *shareable = false;
            element.as_element()
                   .map_or(false, |e| e.has_servo_nonzero_border())
        }

        SimpleSelector::Negation(ref negated) => {
            *shareable = false;
            !negated.iter().all(|s| matches_simple_selector(s, element, shareable))
        },
    }
}

#[inline]
fn matches_generic_nth_child<N>(element: &N,
                                a: i32,
                                b: i32,
                                is_of_type: bool,
                                is_from_end: bool)
                                -> bool
                                where N: TNode {
    let mut node = element.clone();
    // fail if we can't find a parent or if the node is the root element
    // of the document (Cf. Selectors Level 3)
    match node.parent_node() {
        Some(parent) => if parent.is_document() {
            return false;
        },
        None => return false
    };

    let mut index = 1;
    loop {
        if is_from_end {
            match node.next_sibling() {
                None => break,
                Some(next_sibling) => node = next_sibling
            }
        } else {
            match node.prev_sibling() {
                None => break,
                Some(prev_sibling) => node = prev_sibling
            }
        }

        if node.is_element() {
            if is_of_type {
                if let (Some(e), Some(n)) = (element.as_element(), node.as_element()) {
                    if e.get_local_name() == n.get_local_name() && e.get_namespace() == n.get_namespace() {
                        index += 1;
                    }
                }
            } else {
              index += 1;
            }
        }
    }

    if a == 0 {
        b == index
    } else {
        (index - b) / a >= 0 &&
        (index - b) % a == 0
    }
}

#[inline]
fn matches_root<N>(element: &N) -> bool where N: TNode {
    match element.parent_node() {
        Some(parent) => parent.is_document(),
        None => false
    }
}

#[inline]
fn matches_first_child<N>(element: &N) -> bool where N: TNode {
    let mut node = element.clone();
    loop {
        match node.prev_sibling() {
            Some(prev_sibling) => {
                node = prev_sibling;
                if node.is_element() {
                    return false
                }
            },
            None => match node.parent_node() {
                // Selectors level 3 says :first-child does not match the
                // root of the document; Warning, level 4 says, for the time
                // being, the contrary...
                Some(parent) => return !parent.is_document(),
                None => return false
            }
        }
    }
}

#[inline]
fn matches_last_child<N>(element: &N) -> bool where N: TNode {
    let mut node = element.clone();
    loop {
        match node.next_sibling() {
            Some(next_sibling) => {
                node = next_sibling;
                if node.is_element() {
                    return false
                }
            },
            None => match node.parent_node() {
                // Selectors level 3 says :last-child does not match the
                // root of the document; Warning, level 4 says, for the time
                // being, the contrary...
                Some(parent) => return !parent.is_document(),
                None => return false
            }
        }
    }
}

fn find_push<T>(map: &mut HashMap<Atom, Vec<Rule<T>>, DefaultState<FnvHasher>>,
                key: Atom,
                value: Rule<T>) {
    match map.get_mut(&key) {
        Some(vec) => {
            vec.push(value);
            return
        }
        None => {}
    }
    map.insert(key, vec![value]);
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;
    use std::sync::Arc;
    use super::{DeclarationBlock, Rule, SelectorMap};
    use parser::LocalName;
    use string_cache::Atom;
    use cssparser::Parser;
    use parser::ParserContext;

    /// Helper method to get some Rules from selector strings.
    /// Each sublist of the result contains the Rules for one StyleRule.
    fn get_mock_rules(css_selectors: &[&str]) -> Vec<Vec<Rule<()>>> {
        use parser::parse_selector_list;

        css_selectors.iter().enumerate().map(|(i, selectors)| {
            let context = ParserContext::new();
            parse_selector_list(&context, &mut Parser::new(*selectors))
            .unwrap().into_iter().map(|s| {
                Rule {
                    selector: s.compound_selectors.clone(),
                    declarations: DeclarationBlock {
                        specificity: s.specificity,
                        declarations: Arc::new(()),
                        source_order: i,
                    }
                }
            }).collect()
        }).collect()
    }

    #[test]
    fn test_rule_ordering_same_specificity(){
        let rules_list = get_mock_rules(&["a.intro", "img.sidebar"]);
        let a = &rules_list[0][0].declarations;
        let b = &rules_list[1][0].declarations;
        assert!((a.specificity, a.source_order).cmp(&(b.specificity, b.source_order)) == Ordering::Less,
                "The rule that comes later should win.");
    }

    #[test]
    fn test_get_id_name(){
        let rules_list = get_mock_rules(&[".intro", "#top"]);
        assert_eq!(SelectorMap::get_id_name(&rules_list[0][0]), None);
        assert_eq!(SelectorMap::get_id_name(&rules_list[1][0]), Some(atom!("top")));
    }

    #[test]
    fn test_get_class_name(){
        let rules_list = get_mock_rules(&[".intro.foo", "#top"]);
        assert_eq!(SelectorMap::get_class_name(&rules_list[0][0]), Some(Atom::from_slice("intro")));
        assert_eq!(SelectorMap::get_class_name(&rules_list[1][0]), None);
    }

    #[test]
    fn test_get_local_name(){
        let rules_list = get_mock_rules(&["img.foo", "#top", "IMG", "ImG"]);
        let check = |i: usize, names: Option<(&str, &str)>| {
            assert!(SelectorMap::get_local_name(&rules_list[i][0])
                    == names.map(|(name, lower_name)| LocalName {
                            name: Atom::from_slice(name),
                            lower_name: Atom::from_slice(lower_name) }))
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
        assert_eq!(1, selector_map.id_hash.get(&atom!("top")).unwrap()[0].declarations.source_order);
        selector_map.insert(rules_list[0][0].clone());
        assert_eq!(0, selector_map.class_hash.get(&Atom::from_slice("intro")).unwrap()[0].declarations.source_order);
        assert!(selector_map.class_hash.get(&Atom::from_slice("foo")).is_none());
    }
}
