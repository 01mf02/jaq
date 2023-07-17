use crate::def::{Arg, Main};
use crate::filter::{args, filter};
use crate::{Def, Token};
use alloc::vec::Vec;
use chumsky::prelude::*;

/// Parser for a single definition.
fn def<P>(def: P) -> impl Parser<Token, Def, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Def, Error = Simple<Token>> + Clone,
{
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let arg = filter_map(|span, tok| match tok {
        Token::Ident(name) => Ok(Arg { name, var: false }),
        Token::Var(name) => Ok(Arg { name, var: true }),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    just(Token::Def)
        .ignore_then(ident.labelled("filter name"))
        .then(args(arg).labelled("filter args"))
        .then_ignore(just(Token::Ctrl(':')))
        .then(def.repeated().collect())
        .then(filter())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|(((name, args), defs), body)| Def {
            name,
            args,
            defs,
            body,
        })
        .labelled("definition")
}

/// Parser for a sequence of definitions.
pub fn defs() -> impl Parser<Token, Vec<Def>, Error = Simple<Token>> + Clone {
    recursive(def).repeated().collect()
}

/// Parser for a (potentially empty) sequence of definitions, followed by a filter.
pub fn main() -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    defs().then(filter())
}
