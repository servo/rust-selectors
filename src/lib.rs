/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![feature(box_syntax, int_uint, plugin, std_misc, core, hash, unsafe_destructor, alloc)]
#![cfg_attr(test, feature(test))]

#[macro_use] extern crate bitflags;
#[macro_use] extern crate cssparser;
#[macro_use] extern crate matches;
#[cfg(test)] extern crate rand;
extern crate string_cache;
#[no_link] #[macro_use] #[plugin] extern crate string_cache_macros;

pub mod bloom;
pub mod matching;
pub mod parser;
pub mod quicksort;
pub mod smallvec;
pub mod tree;
