#[cfg(feature = "ariadne")]
pub mod ariadne;
mod lex;
mod ops;
pub mod parse;

pub use lex::{lex, Token};
pub use ops::{MathOp, OrdOp};

pub type Span = std::ops::Range<usize>;

use chumsky::prelude::*;

pub fn parse<T, P>(src: &str, parser: P) -> Result<T, Vec<Simple<String>>>
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    let (tokens, lex_errs) = lex().parse_recovery(src);

    let (out, parse_errs) = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let stream = chumsky::Stream::from_iter(len..len + 1, tokens.into_iter());
        parser.parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    let lex_errs = lex_errs.into_iter().map(|e| e.map(|c| c.to_string()));
    let parse_errs = parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string()));
    let errs = lex_errs.chain(parse_errs);

    out.ok_or_else(|| errs.collect())
}
