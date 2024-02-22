use super::{filter::filter, Delim, Token};
use alloc::vec::Vec;
use chumsky::prelude::*;
use jaq_syn::{Arg, Call, Def, Main};

/// A (potentially empty) parenthesised and `;`-separated sequence of arguments.
fn args<T, P>(
    #[allow(unused_variables)] unstable: bool,
    arg: P,
) -> impl Parser<Token, Vec<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T> + Clone,
{
    Delim::Paren
        .around(arg.separated_by(just(Token::Semicolon)))
        .or_not()
        .map(Option::unwrap_or_default)
}

pub fn call<T, P>(unstable: bool, expr: P) -> impl Parser<Token, Call<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("filter name")
    .then(args(unstable, expr).labelled("filter args"))
    .map(|(name, args)| Call::new(name, args))
}

/// Parser for a single definition.
fn def<P>(unstable: bool, def: P) -> impl Parser<Token, Def, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Def, Error = Simple<Token>> + Clone,
{
    let arg = select! {
        Token::Ident(name) => Arg::new_filter(name),
        Token::Var(name) => Arg::new_var(name),
    };

    let defs = def.repeated().collect();

    just(Token::Def)
        .ignore_then(call(unstable, arg))
        .then_ignore(just(Token::Colon))
        .then(
            defs.then(filter(
                #[cfg(feature = "unstable-flag")]
                unstable,
            ))
            .map(|(defs, body)| Main::new(defs, body)),
        )
        .then_ignore(just(Token::Semicolon))
        .map(|(lhs, rhs)| Def::new(lhs, rhs))
        .labelled("definition")
}

/// Parser for a sequence of definitions.
pub fn defs(
    #[cfg(feature = "unstable-flag")] unstable: bool,
) -> impl Parser<Token, Vec<Def>, Error = Simple<Token>> + Clone {
    #[cfg(not(feature = "unstable-flag"))]
    let unstable = false;
    recursive(|p| def(unstable, p)).repeated().collect()
}

/// Parser for a (potentially empty) sequence of definitions, followed by a filter.
pub fn main(
    #[cfg(feature = "unstable-flag")] unstable: bool,
) -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    defs(
        #[cfg(feature = "unstable-flag")]
        unstable,
    )
    .then(filter(
        #[cfg(feature = "unstable-flag")]
        unstable,
    ))
    .map(|(defs, body)| Main::new(defs, body))
}
