//! JSON query language parser.
//#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod def;
mod filter;
mod lex;
mod path;
mod prec_climb;
mod string;
mod term;
mod token;

pub use def::{defs, main};
use token::{Delim, Token};

use alloc::{string::String, string::ToString, vec::Vec};
use chumsky::prelude::*;

/// Lex/parse error.
pub type Error = Simple<String>;

/// Parse a string with a given parser.
///
/// May produce `Some` output even if there were errors.
pub fn parse<T, P>(src: &str, parser: P) -> (Option<T>, Vec<Error>)
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    let (tokens, lex_errs) = crate::lex::Lex::new(src).lex();

    let mut new_parser = term::Parser::new(&tokens);
    std::println!("{:?}", new_parser.main());
    std::println!("{:?}", new_parser.e);

    let tokens: Vec<_> = tokens.into_iter().flat_map(|t| t.tokens(src)).collect();
    //std::println!("Tokens: {tokens:?}");

    let (parsed, parse_errs) = if lex_errs.is_empty() {
        let len = src.chars().count();
        let stream = chumsky::Stream::from_iter(len..len + 1, tokens.into_iter());
        parser.then_ignore(end()).parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    let lex_errs = lex_errs.iter().map(|(e, s)| {
        let (e, span) = e.to_simple_error(s, src);
        Simple::custom(span, e)
    });
    let parse_errs = parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string()));
    let errs: Vec<_> = lex_errs.chain(parse_errs).collect();

    (parsed, errs)
}
