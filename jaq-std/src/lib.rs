//! Standard library for a JSON query language.
//!
//! Just like Rust, jaq is divided into a core and a standard library.
//! The standard library provides a set of filters defined using core filters.
//! For example, the standard library provides the `map(f)` filter,
//! which is defined using the more elementary filter `[.[] | f]`.
#![no_std]
#![warn(missing_docs)]

extern crate alloc;
use jaq_interpret::load;

/// Return the standard library.
pub fn std() -> alloc::vec::Vec<load::parse::Def<&'static str>> {
    load::parse(include_str!("std.jq"), |p| p.defs()).unwrap()
}
