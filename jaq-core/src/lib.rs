#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod error;
mod filter;
mod ops;
mod path;
mod recurse;
mod toplevel;
mod unparse;
mod val;

pub use jaq_parse as parse;

pub use error::Error;
pub use filter::Filter;
use path::Path;
use recurse::Recurse;
pub use toplevel::Definitions;
use unparse::unparse;
pub use val::Val;

use alloc::{boxed::Box, rc::Rc, vec::Vec};

/// A value result.
pub type ValR = Result<Val, Error>;

/// A reference-counted value result.
pub type RValR = Result<Rc<Val>, Error>;

/// A stream of reference-counted values.
pub type RVals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

/// A stream of reference-counted value results.
pub type RValRs<'a> = Box<dyn Iterator<Item = RValR> + 'a>;

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
