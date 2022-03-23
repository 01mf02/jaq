#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub mod error;
pub mod filter;
pub mod functions;
pub mod ops;
pub mod parse;
pub mod path;
pub mod preprocess;
pub mod recurse;
pub mod toplevel;
pub mod val;

pub use error::Error;
pub use filter::Filter;
pub use path::Path;
pub use preprocess::{ClosedFilter, OpenFilter, PreFilter};
pub use recurse::Recurse;
pub use toplevel::{Definition, Definitions, Main};
pub use val::Val;

use alloc::{boxed::Box, rc::Rc};

/// A value result.
pub type ValR = Result<Val, Error>;

/// A reference-counted value result.
pub type RValR = Result<Rc<Val>, Error>;

/// A stream of reference-counted values.
pub type RVals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

/// A stream of value results.
pub type ValRs<'a> = Box<dyn Iterator<Item = ValR> + 'a>;

/// A stream of reference-counted value results.
pub type RValRs<'a> = Box<dyn Iterator<Item = RValR> + 'a>;

#[cfg(feature = "bincode")]
fn parse_std() -> jaq_parse::parse::Defs {
    // use preparsed standard library
    let std = include_bytes!(concat!(env!("OUT_DIR"), "/std.bin"));
    bincode::deserialize(std).unwrap()
}

#[cfg(not(feature = "bincode"))]
fn parse_std() -> jaq_parse::parse::Defs {
    let std = include_str!("std.jq");
    jaq_parse::parse(std, jaq_parse::parse::parse_defs()).unwrap()
}

pub fn std() -> Definitions {
    Definitions::try_from(parse_std()).unwrap()
}
