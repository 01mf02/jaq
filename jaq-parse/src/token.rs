use alloc::string::String;
use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Str(String),
    Op(String),
    Ident(String),
    Var(String),
    Ctrl(char),
    DotDot,
    Dot,
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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Num(s) | Self::Str(s) | Self::Op(s) | Self::Ident(s) => s.fmt(f),
            Self::Var(s) => write!(f, "${s}"),
            Self::Ctrl(c) => c.fmt(f),
            Self::DotDot => "..".fmt(f),
            Self::Dot => ".".fmt(f),
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
        }
    }
}
