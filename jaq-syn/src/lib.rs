//! JSON query language parser.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

pub mod lex;
pub mod load;
mod ops;
pub mod parse;
pub mod path;
mod prec_climb;
pub mod test;

pub use lex::Lexer;
pub use load::Loader;
pub use ops::{MathOp, OrdOp};
pub use parse::Parser;

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

/// Return the span of a string slice `part` relative to a string slice `whole`.
///
/// The caller must ensure that `part` is fully contained inside `whole`.
pub fn span(whole: &str, part: &str) -> core::ops::Range<usize> {
    let start = part.as_ptr() as usize - whole.as_ptr() as usize;
    start..start + part.len()
}
