use crate::Span;
use chumsky::prelude::*;
use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Op(String),
    Ident(String),
    Ctrl(char),
    Dot,
    DotId(String),
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
            Self::Null => "null".fmt(f),
            Self::Bool(x) => x.fmt(f),
            Self::Num(s) | Self::Str(s) | Self::Op(s) | Self::Ident(s) => s.fmt(f),
            Self::Ctrl(c) => c.fmt(f),
            Self::Dot => ".".fmt(f),
            Self::DotId(s) => write!(f, r#".{}"#, s),
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

pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect();

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect();

    // A parser for operators
    let op = one_of("|=!<>+-*/%").repeated().at_least(1).collect();

    let dot = just('.');
    let dot_id = just('.').ignore_then(text::ident().or(str_));

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
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = ident
        .or(num.map(Token::Num))
        .or(str_.map(Token::Str))
        .or(op.map(Token::Op))
        .or(ctrl.map(Token::Ctrl))
        .or(dot_id.map(Token::DotId))
        .or(dot.to(Token::Dot))
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}
