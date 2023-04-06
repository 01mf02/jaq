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
    /// If the argument is a variable, return its name without leading "$", otherwise `None`.
    pub fn get_var(&self) -> Option<&str> {
        self.var.then(|| &*self.name)
    }

    pub fn get_arg(&self) -> Option<&str> {
        (!self.var).then(|| &*self.name)
    }

    // TODO: remove this?
    /// Return the full name of the argument, including leading "$" for variables.
    pub fn get_name(&self) -> String {
        use alloc::borrow::ToOwned;
        if self.var {
            "$".to_owned() + &self.name
        } else {
            self.name.clone()
        }
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
    recursive(|df| def(df)).repeated().collect()
}

/// Parser for a (potentially empty) sequence of definitions, followed by a filter.
pub fn main() -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    defs().then(filter())
}
