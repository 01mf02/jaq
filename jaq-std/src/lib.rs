//! Standard library for a JSON query language.
//!
//! Just like Rust, jaq is divided into a core and a standard library.
//! The standard library provides a set of filters defined using core filters.
//! For example, the standard library provides the `map(f)` filter,
//! which is defined using the more elementary filter `[.[] | f]`.
//!
//! The time required to parse the standard library becomes evident
//! when the runtime of the jaq filter is small.
//! Therefore, when the "bincode" feature is enabled,
//! this crate precompiles the standard library,
//! in order to reduce startup time.
#![no_std]
#![warn(missing_docs)]

extern crate alloc;
use alloc::vec::Vec;

/// Return the standard library.
pub fn std(#[cfg(feature = "unstable-flag")] unstable: bool) -> Vec<jaq_syn::Def> {
    #[cfg(feature = "bincode")]
    {
        // use preparsed standard library
        let std = include_bytes!(concat!(env!("OUT_DIR"), "/std.bin"));
        #[cfg(feature = "unstable-flag")]
        let std_unstable = include_bytes!(concat!(env!("OUT_DIR"), "/std-unstable.bin"));
        #[cfg(feature = "unstable-flag")]
        let std = if unstable { std } else { std_unstable };
        bincode::deserialize(std).unwrap()
    }
    #[cfg(not(feature = "bincode"))]
    {
        let std = include_str!("std.jq");
        jaq_parse::parse(unstable, std, jaq_parse::defs(unstable))
            .0
            .unwrap()
    }
}
