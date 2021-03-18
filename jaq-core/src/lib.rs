#![cfg_attr(not(feature = "std"), no_std)]

pub mod error;
pub mod filter;
pub mod functions;
pub mod map;
pub mod number;
pub mod ops;
#[cfg(feature = "parse")]
pub mod parse;
pub mod path;
pub mod recurse;
pub mod val;

pub use error::Error;
pub use filter::Filter;
pub use path::Path;
pub use recurse::Recurse;
pub use val::Val;

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;
#[cfg(feature = "parse")]
extern crate pest;
#[cfg(feature = "parse")]
#[macro_use]
extern crate pest_derive;
