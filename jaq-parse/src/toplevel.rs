use crate::filter::{args, filter, Filter};
use crate::{Spanned, Token};
use alloc::{string::String, vec::Vec};
use chumsky::prelude::*;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A definition, such as `def map(f): [.[] | f];`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Def {
    /// Name of the filter, e.g. `map`
    pub name: String,
    /// Arguments of the filter, e.g. `["f"]`
    pub args: Vec<Arg>,
    /// Definitions at the top of the filter
    pub defs: Vec<Self>,
    /// Body of the filter, e.g. `[.[] | f`.
    pub body: Spanned<Filter>,
}

/// Argument of a definition, such as `$v` or `f` in `def foo($v; f): ...`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Arg {
    name: String,
    var: bool,
}

impl Arg {
    /// Create a variable argument with given name (without leading "$").
    pub fn new_var(name: String) -> Self {
        Self { name, var: true }
    }

    /// True if the argument is a variable.
    pub fn is_var(&self) -> bool {
        self.var
    }

    /// If the argument is a variable, return its name without leading "$", otherwise `None`.
    pub fn get_var(&self) -> Option<&str> {
        self.var.then_some(&*self.name)
    }

    /// If the argument is not a variable, return its name, otherwise `None`.
    pub fn get_nonvar(&self) -> Option<&str> {
        (!self.var).then_some(&*self.name)
    }
}

/// (Potentially empty) sequence of definitions, followed by a filter.
pub type Main = (Vec<Def>, Spanned<Filter>);

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
