#![no_std]

extern crate alloc;
use alloc::vec::Vec;

#[cfg(feature = "bincode")]
pub fn std() -> Vec<jaq_parse::parse::Def> {
    // use preparsed standard library
    let std = include_bytes!(concat!(env!("OUT_DIR"), "/std.bin"));
    bincode::deserialize(std).unwrap()
}

#[cfg(not(feature = "bincode"))]
pub fn std() -> Vec<jaq_parse::parse::Def> {
    let std = include_str!("std.jq");
    jaq_parse::parse(std, jaq_parse::defs()).unwrap()
}
