//! JSON query language parser.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod parse;

mod def;
pub mod filter;
mod ops;
pub mod path;
pub mod test;
mod token;

pub use def::{Arg, Def, Main};
pub use ops::{MathOp, OrdOp};
pub use parse::{defs, main, parse, Error};
use path::Path;
use token::Token;

type Span = core::ops::Range<usize>;

/// An object with position information.
pub type Spanned<T> = (T, Span);
