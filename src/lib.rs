/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![cfg_attr(all(test, feature = "unstable"), feature(test))]
#![cfg_attr(feature = "heap_size", feature(plugin, custom_derive))]
#![cfg_attr(feature = "heap_size", plugin(heapsize_plugin))]

#[cfg(feature = "heap_size")] extern crate heapsize;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate cssparser;
#[macro_use] extern crate matches;
#[macro_use] extern crate string_cache;
extern crate fnv;

pub mod bloom;
pub mod matching;
pub mod parser;
mod tree;

pub use tree::Element;
pub use tree::{MatchAttr, MatchAttrGeneric};

pub type HashMap<K, V> = ::std::collections::HashMap<K, V, ::std::hash::BuildHasherDefault<::fnv::FnvHasher>>;
