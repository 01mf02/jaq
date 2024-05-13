use alloc::{string::String};
use chumsky::prelude::*;
use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Paren, // ( ... )
    Brack, // [ ... ]
    Brace, // { ... }
}

impl Delim {
    fn open(self) -> char {
        match self {
            Self::Paren => '(',
            Self::Brack => '[',
            Self::Brace => '{',
        }
    }

    pub(crate) fn close(self) -> char {
        match self {
            Self::Paren => ')',
            Self::Brack => ']',
            Self::Brace => '}',
        }
    }

    /// Parse some tokens surrounded by the delimiters.
    pub fn around<T, P>(self, f: P) -> impl Parser<Token, T, Error = P::Error> + Clone
    where
        P: Parser<Token, T> + Clone,
    {
        f.delimited_by(just(Token::Open(self)), just(Token::Close(self)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Str(String),
    Op(String),
    Ident(String),
    Var(String),
    Open(Delim),
    Close(Delim),
    Quote,
    DotDot,
    Dot,
    Colon,
    Semicolon,
    Comma,
    Question,
    Def,
    If,
    Then,
    Elif,
    Else,
    End,
    Or,
    And,
    As,
    Reduce,
    For,
    Foreach,
    Try,
    Catch,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Num(s) | Self::Str(s) | Self::Op(s) | Self::Ident(s) => s.fmt(f),
            Self::Var(s) => write!(f, "${s}"),
            Self::Open(delim) => delim.open().fmt(f),
            Self::Close(delim) => delim.close().fmt(f),
            Self::Quote => '"'.fmt(f),
            Self::DotDot => "..".fmt(f),
            Self::Dot => '.'.fmt(f),
            Self::Colon => ':'.fmt(f),
            Self::Semicolon => ';'.fmt(f),
            Self::Comma => ','.fmt(f),
            Self::Question => '?'.fmt(f),
            Self::Def => "def".fmt(f),
            Self::If => "if".fmt(f),
            Self::Then => "then".fmt(f),
            Self::Elif => "elif".fmt(f),
            Self::Else => "else".fmt(f),
            Self::End => "end".fmt(f),
            Self::Or => "or".fmt(f),
            Self::And => "and".fmt(f),
            Self::As => "as".fmt(f),
            Self::Reduce => "reduce".fmt(f),
            Self::For => "for".fmt(f),
            Self::Foreach => "foreach".fmt(f),
            Self::Try => "try".fmt(f),
            Self::Catch => "catch".fmt(f),
        }
    }
}
