//! JSON query language syntax.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod def;
pub mod filter;
mod ops;
pub mod path;
pub mod string;
pub mod test;

pub use def::{Arg, Call, Def, Main};
pub use ops::{LogicOp, MathOp, OrdOp};
use path::Path;
pub use string::Str;

/// Position information.
pub type Span = core::ops::Range<usize>;

/// An object with position information.
pub type Spanned<T> = (T, Span);
