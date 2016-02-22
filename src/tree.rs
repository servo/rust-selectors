/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Traits that nodes must implement. Breaks the otherwise-cyclic dependency between layout and
//! style.

use matching::ElementFlags;
use parser::{AttrSelector, SelectorImpl};
use string_cache::{Atom, Namespace};

pub trait Element: Sized {
    type Impl: SelectorImpl;

    fn parent_element(&self) -> Option<Self>;

    // Skips non-element nodes
    fn first_child_element(&self) -> Option<Self>;

    // Skips non-element nodes
    fn last_child_element(&self) -> Option<Self>;

    // Skips non-element nodes
    fn prev_sibling_element(&self) -> Option<Self>;

    // Skips non-element nodes
    fn next_sibling_element(&self) -> Option<Self>;

    fn is_html_element_in_html_document(&self) -> bool;
    fn get_local_name<'a>(&'a self) -> &'a Atom;
    fn get_namespace<'a>(&'a self) -> &'a Namespace;

    fn match_non_ts_pseudo_class(&self, pc: <Self::Impl as SelectorImpl>::NonTSPseudoClass) -> bool;

    fn get_id(&self) -> Option<Atom>;
    fn has_class(&self, name: &Atom) -> bool;
    fn match_attr<F>(&self, attr: &AttrSelector, test: F) -> bool where F: Fn(&str) -> bool;

    /// Returns whether this element matches `:empty`.
    ///
    /// That is, whether it does not contain any child element or any non-zero-length text node.
    /// See http://dev.w3.org/csswg/selectors-3/#empty-pseudo
    fn is_empty(&self) -> bool;

    /// Returns whether this element matches `:root`,
    /// i.e. whether it is the root element of a document.
    ///
    /// Note: this can be false even if `.parent_element()` is `None`
    /// if the parent node is a `DocumentFragment`.
    fn is_root(&self) -> bool;

    // Ordinarily I wouldn't use callbacks like this, but the alternative is
    // really messy, since there is a `JSRef` and a `RefCell` involved. Maybe
    // in the future when we have associated types and/or a more convenient
    // JS GC story... --pcwalton
    fn each_class<F>(&self, callback: F) where F: FnMut(&Atom);

    /// Add flags to the element. See the `ElementFlags` docs for details.
    ///
    /// This may be called while the element *or one of its children* is being matched. Therefore
    /// the implementation must be thread-safe if children may be matched in parallel.
    fn insert_flags(&self, _flags: ElementFlags) {}
}
