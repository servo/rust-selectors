/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![feature(box_syntax, plugin, owned_ascii_ext, hashmap_hasher)]
#![cfg_attr(test, feature(test, hash_default))]
#![plugin(string_cache_plugin)]

#[macro_use] extern crate bitflags;
#[macro_use] extern crate cssparser;
#[macro_use] extern crate matches;
#[cfg(test)] extern crate rand;
extern crate string_cache;
extern crate quicksort;
extern crate smallvec;
extern crate fnv;

pub mod bloom;
pub mod matching;
pub mod parser;
mod tree;

pub use tree::{Node, Element};
