//! JSON query language syntax.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod def;
pub mod filter;
mod ops;
pub mod path;
pub mod test;

pub use def::{Arg, Def, Main};
pub use ops::{MathOp, OrdOp};
use path::{Path, Str};

/// Position information.
pub type Span = core::ops::Range<usize>;

/// An object with position information.
pub type Spanned<T> = (T, Span);
