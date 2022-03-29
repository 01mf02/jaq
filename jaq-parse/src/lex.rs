use crate::Span;
use alloc::{string::String, vec::Vec};
use chumsky::prelude::*;
use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Str(String),
    Op(String),
    Ident(String),
    Ctrl(char),
    Dot(Option<String>),
    Def,
    If,
    Then,
    Else,
    End,
    Or,
    And,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Num(s) | Self::Str(s) | Self::Op(s) | Self::Ident(s) => s.fmt(f),
            Self::Ctrl(c) => c.fmt(f),
            Self::Dot(None) => ".".fmt(f),
            Self::Dot(Some(s)) => write!(f, r#".{}"#, s),
            Self::Def => "def".fmt(f),
            Self::If => "if".fmt(f),
            Self::Then => "then".fmt(f),
            Self::Else => "else".fmt(f),
            Self::End => "end".fmt(f),
            Self::Or => "or".fmt(f),
            Self::And => "and".fmt(f),
        }
    }
}

// A parser for numbers
fn num() -> impl Parser<char, String, Error = Simple<char>> {
    let comma = just('.').chain(text::digits(10).or_not().map(|d| d.unwrap_or_default()));
    let exp = one_of("eE").chain(text::digits(10));
    text::int(10)
        .chain::<char, _, _>(comma.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .collect()
}

// A parser for strings
fn str_() -> impl Parser<char, String, Error = Simple<char>> {
    just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect()
}

fn token() -> impl Parser<char, Token, Error = Simple<char>> {
    // A parser for operators
    let op = one_of("|=!<>+-*/%").chain(just('=').or_not()).collect();

    let dot_id = text::ident().or(text::whitespace().ignore_then(str_()));
    let dot = just('.').ignore_then(dot_id.or_not());

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("{}()[]:;,?");

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "def" => Token::Def,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "end" => Token::End,
        "or" => Token::Or,
        "and" => Token::And,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    ident
        .or(ctrl.map(Token::Ctrl))
        .or(op.map(Token::Op))
        .or(dot.map(Token::Dot))
        .or(num().map(Token::Num))
        .or(str_().map(Token::Str))
        .recover_with(skip_then_retry_until([]))
}

pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let comment = just("#").then(take_until(just('\n'))).padded();

    token()
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}
