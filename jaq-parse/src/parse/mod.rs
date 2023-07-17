mod def;
mod filter;
mod path;
mod prec_climb;
mod token;

pub use def::{defs, main};

use crate::{Spanned, Token};
use alloc::{string::String, string::ToString, vec::Vec};
use chumsky::prelude::*;

/// Lex/parse error.
pub type Error = Simple<String>;

/// A (potentially empty) parenthesised and `;`-separated sequence of arguments.
fn args<T, P>(arg: P) -> impl Parser<Token, Vec<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T> + Clone,
{
    arg.separated_by(just(Token::Ctrl(';')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .or_not()
        .map(Option::unwrap_or_default)
}

fn lex() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let comment = just("#").then(take_until(just('\n'))).padded();

    token::token()
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
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
