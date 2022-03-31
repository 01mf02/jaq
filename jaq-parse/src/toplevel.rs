use crate::{Spanned, Token};
use crate::filter::{Expr, expr, args};
use alloc::{string::String, vec::Vec};
use chumsky::prelude::*;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Def {
    pub name: String,
    pub args: Vec<String>,
    pub body: Spanned<Expr>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Main {
    pub defs: Vec<Def>,
    pub body: Spanned<Expr>,
}

fn def() -> impl Parser<Token, Def, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    just(Token::Def)
        .ignore_then(ident.labelled("filter name"))
        .then(args(ident).labelled("filter args"))
        .then_ignore(just(Token::Ctrl(':')))
        .then(expr())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|((name, args), body)| Def { name, args, body })
        .labelled("definition")
}

pub fn defs() -> impl Parser<Token, Vec<Def>, Error = Simple<Token>> + Clone {
    def().repeated().collect()
}

pub fn main() -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    defs().then(expr()).map(|(defs, body)| Main { defs, body })
}
