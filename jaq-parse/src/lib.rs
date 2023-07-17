//! JSON query language parser.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

#[cfg(feature = "chumsky")]
mod parse;

mod def;
pub mod filter;
mod ops;
pub mod path;
pub mod test;

pub use def::{Arg, Def, Main};
pub use ops::{MathOp, OrdOp};
#[cfg(feature = "chumsky")]
pub use parse::{defs, main, parse, Error};
use path::Path;

type Span = core::ops::Range<usize>;

/// An object with position information.
pub type Spanned<T> = (T, Span);
