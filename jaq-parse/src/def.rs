use super::{filter::filter, Delim, Token};
use alloc::vec::Vec;
use chumsky::prelude::*;
use jaq_syn::{Arg, Call, Def, Main};

/// A (potentially empty) parenthesised and `;`-separated sequence of arguments.
fn args<T, P>(arg: P) -> impl Parser<Token, Vec<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T> + Clone,
{
    Delim::Paren
        .around(arg.separated_by(just(Token::Semicolon)))
        .or_not()
        .map(Option::unwrap_or_default)
}

pub fn call<T, P>(expr: P) -> impl Parser<Token, Call<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("filter name")
    .then(args(expr).labelled("filter args"))
    .map(|(name, args)| Call { name, args })
}

/// Parser for a single definition.
fn def<P>(def: P) -> impl Parser<Token, Def, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Def, Error = Simple<Token>> + Clone,
{
    let arg = select! {
        Token::Ident(name) => Arg::new_filter(name),
        Token::Var(name) => Arg::new_var(name),
    };

    let defs = def.repeated().collect();

    just(Token::Def)
        .ignore_then(call(arg))
        .then_ignore(just(Token::Colon))
        .then(defs.then(filter()).map(|(defs, body)| Main { defs, body }))
        .then_ignore(just(Token::Semicolon))
        .map(|(lhs, rhs)| Def { lhs, rhs })
        .labelled("definition")
}

/// Parser for a sequence of definitions.
pub fn defs() -> impl Parser<Token, Vec<Def>, Error = Simple<Token>> + Clone {
    recursive(def).repeated().collect()
}

/// Parser for a (potentially empty) sequence of definitions, followed by a filter.
pub fn main() -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    defs()
        .then(filter())
        .map(|(defs, body)| Main { defs, body })
}
