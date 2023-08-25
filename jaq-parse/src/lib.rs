//! JSON query language parser.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod def;
mod filter;
mod path;
mod prec_climb;
mod token;
mod string;

use jaq_syn as syn;

pub use def::{defs, main};
use token::Token;

use alloc::{string::String, string::ToString, vec::Vec};
use chumsky::prelude::*;
use syn::Spanned;

/// Lex/parse error.
pub type Error = Simple<String>;

fn lex() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    recursive(token::tree)
        .map_with_span(|tree, span| tree.tokens(span))
        .repeated()
        .flatten()
        .collect()
}

/// Parse a string with a given parser.
///
/// May produce `Some` output even if there were errors.
pub fn parse<T, P>(src: &str, parser: P) -> (Option<T>, Vec<Error>)
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    let (tokens, lex_errs) = lex()
        .then_ignore(end())
        .recover_with(skip_then_retry_until([]))
        .parse_recovery(src);

    let (parsed, parse_errs) = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let stream = chumsky::Stream::from_iter(len..len + 1, tokens.into_iter());
        parser.then_ignore(end()).parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    let lex_errs = lex_errs.into_iter().map(|e| e.map(|c| c.to_string()));
    let parse_errs = parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string()));
    let errs: Vec<_> = lex_errs.chain(parse_errs).collect();

    (parsed, errs)
}
