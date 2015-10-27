/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![cfg_attr(feature = "unstable", feature(plugin, hashmap_hasher, custom_derive))]
#![cfg_attr(feature = "unstable", plugin(string_cache_plugin))]
#![cfg_attr(all(test, feature = "unstable"), feature(test))]
#![cfg_attr(feature = "heap_size", feature(plugin, custom_derive))]
#![cfg_attr(feature = "heap_size", plugin(heapsize_plugin))]

#[cfg(feature = "heap_size")] extern crate heapsize;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate cssparser;
#[macro_use] extern crate matches;
#[cfg(test)] extern crate rand;
#[macro_use] extern crate string_cache;
extern crate quickersort;
extern crate smallvec;
extern crate fnv;

pub mod bloom;
#[macro_use] pub mod states;
pub mod matching;
pub mod parser;
mod tree;

pub use tree::Element;


#[cfg(feature = "unstable")]
mod hash_map {
    use std::collections::hash_state::DefaultState;
    use std::hash::Hash;

    pub type HashMap<K, V> = ::std::collections::HashMap<K, V, DefaultState<::fnv::FnvHasher>>;

    pub fn new<K, V>() -> HashMap<K, V> where K: Hash + Eq {
        ::std::collections::HashMap::with_hash_state(Default::default())
    }
}

#[cfg(not(feature = "unstable"))]
mod hash_map {
    use std::hash::Hash;

    // Default state: Random SipHasher
    pub type HashMap<K, V> = ::std::collections::HashMap<K, V>;

    pub fn new<K, V>() -> HashMap<K, V> where K: Hash + Eq {
        ::std::collections::HashMap::new()
    }
}
