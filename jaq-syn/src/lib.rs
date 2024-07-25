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

mod convert;
mod graph;
pub mod lex;
pub mod parse;
mod prec_climb;

pub use def::{Arg, Call, Def, Main};
pub use lex::Lexer;
pub use ops::{MathOp, OrdOp};
pub use parse::Parser;
use path::Path;
pub use string::Str;

/// Position information.
pub type Span = core::ops::Range<usize>;

/// An object with position information.
pub type Spanned<T> = (T, Span);

/// Lex a string and parse resulting tokens, returning [`None`] if any error occurred.
///
/// Example:
///
/// ~~~
/// # use jaq_syn::parse;
/// let t = parse("[] | .[]", |p| p.term());
/// ~~~
pub fn parse<'s, T: Default, F>(s: &'s str, f: F) -> Option<T>
where
    F: for<'t> FnOnce(&mut Parser<'s, 't>) -> parse::Result<'s, 't, T>,
{
    Parser::new(&Lexer::new(s).lex().ok()?).parse(f).ok()
}
