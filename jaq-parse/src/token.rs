use alloc::{boxed::Box, string::String, vec::Vec};
use chumsky::prelude::*;
use core::fmt;
use jaq_syn::{Span, Spanned};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Paren, // ( ... )
    Brack, // [ ... ]
    Brace, // { ... }
}

impl Delim {
    fn open(&self) -> char {
        match self {
            Self::Paren => '(',
            Self::Brack => '[',
            Self::Brace => '{',
        }
    }

    fn close(&self) -> char {
        match self {
            Self::Paren => ')',
            Self::Brack => ']',
            Self::Brace => '}',
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Tree {
    Token(Token),
    Delim(Delim, Vec<Spanned<Self>>),
    String(Spanned<String>, Vec<(Vec<Spanned<Self>>, Spanned<String>)>),
}

impl Tree {
    pub fn tokens(self, span: Span) -> Box<dyn Iterator<Item = Spanned<Token>>> {
        use core::iter::once;
        match self {
            Self::Token(token) => Box::new(once((token, span))),
            Self::Delim(delim, tree) => {
                let s = (Token::Ctrl(delim.open()), span.start..span.start + 1);
                let e = (Token::Ctrl(delim.close()), span.end - 1..span.end);
                let tokens = tree.into_iter().flat_map(|(t, s)| t.tokens(s));
                Box::new(once(s).chain(tokens).chain(once(e)))
            }
            Self::String(s, _interpol) => {
                let s = (Token::Str(s.0), s.1);
                Box::new(once(s))
            }
        }
    }
}

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
    Try,
    Catch,
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
            Self::Try => "try".fmt(f),
            Self::Catch => "catch".fmt(f),
        }
    }
}

// A parser for numbers
fn num() -> impl Parser<char, String, Error = Simple<char>> {
    let comma = just('.').chain(text::digits(10).or_not());

    let exp = one_of("eE")
        .chain(one_of("+-").or_not())
        .chain::<char, _, _>(text::digits(10));

    text::int(10)
        .chain::<char, _, _>(comma.or_not())
        .chain::<char, _, _>(exp.or_not())
        .collect()
}

// A parser for strings; adapted from Chumsky's JSON example parser.
fn char_() -> impl Parser<char, char, Error = Simple<char>> {
    let unicode = filter(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(4)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let escape = just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('u').ignore_then(unicode),
    )));

    filter(|c| *c != '\\' && *c != '"').or(escape)
}

pub fn tree(
    tree: impl Parser<char, Tree, Error = Simple<char>> + Clone,
) -> impl Parser<char, Tree, Error = Simple<char>> {
    let trees = tree.map_with_span(|t, span| (t, span)).repeated().collect();
    let paren = trees.clone().delimited_by(just('('), just(')'));
    let brack = trees.clone().delimited_by(just('['), just(']'));
    let brace = trees.clone().delimited_by(just('{'), just('}'));

    let pair = |s, span| (s, span);
    let chars = || char_().repeated().collect().map_with_span(pair);
    let interpol = just('\\').ignore_then(paren.clone());
    let string = chars()
        .then(interpol.then(chars()).repeated().collect())
        .delimited_by(just('"'), just('"'))
        .labelled("string");

    let comment = just("#").then(take_until(just('\n'))).padded();

    choice((
        paren.map(|t| Tree::Delim(Delim::Paren, t)),
        brack.map(|t| Tree::Delim(Delim::Brack, t)),
        brace.map(|t| Tree::Delim(Delim::Brace, t)),
        string.map(|(s, interpol)| Tree::String(s, interpol)),
        token().map(Tree::Token),
    ))
    .padded_by(comment.repeated())
    .padded()
    //.recover_with(skip_then_retry_until([]))
}

pub fn token() -> impl Parser<char, Token, Error = Simple<char>> {
    // A parser for operators
    let op = one_of("|=!<>+-*/%").chain(one_of("=/").or_not()).collect();

    let var = just('$').ignore_then(text::ident());

    // A parser for control characters (colons, semicolons, etc.)
    let ctrl = one_of(":;,?");

    // A parser for identifiers and keywords
    let ident = just('@').or_not().chain::<char, _, _>(text::ident());
    let ident = ident.collect().map(|ident: String| match ident.as_str() {
        "def" => Token::Def,
        "if" => Token::If,
        "then" => Token::Then,
        "elif" => Token::Elif,
        "else" => Token::Else,
        "end" => Token::End,
        "or" => Token::Or,
        "and" => Token::And,
        "as" => Token::As,
        "reduce" => Token::Reduce,
        "for" => Token::For,
        "foreach" => Token::Foreach,
        "try" => Token::Try,
        "catch" => Token::Catch,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    ident
        .or(just("..").map(|_| Token::DotDot))
        .or(just('.').map(|_| Token::Dot))
        .or(ctrl.map(Token::Ctrl))
        .or(op.map(Token::Op))
        .or(var.map(Token::Var))
        .or(num().map(Token::Num))
}
