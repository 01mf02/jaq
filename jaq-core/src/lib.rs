#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;
#[cfg(feature = "parse")]
extern crate pest;
#[cfg(feature = "parse")]
#[macro_use]
extern crate pest_derive;

pub mod error;
pub mod filter;
pub mod functions;
pub mod map;
pub mod num;
pub mod ops;
#[cfg(feature = "parse")]
pub mod parse;
pub mod path;
pub mod recurse;
pub mod val;

pub use error::Error;
pub use filter::Filter;
pub use map::Map;
pub use path::Path;
pub use recurse::Recurse;
pub use val::Val;
pub use num::Num;

use alloc::{boxed::Box, rc::Rc};

/// A reference-counted value result.
pub type RValR = Result<Rc<Val>, Error>;

/// A stream of reference-counted values.
pub type RVals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

/// A stream of value results.
pub type ValRs<'a> = Box<dyn Iterator<Item = Result<Val, Error>> + 'a>;

/// A stream of reference-counted value results.
pub type RValRs<'a> = Box<dyn Iterator<Item = RValR> + 'a>;
