#![no_main]

use arbitrary::Arbitrary;
use jaq_core::load::{
    lex::{StrPart, Tok, Token},
    Parser,
};
use libfuzzer_sys::fuzz_target;

// Boilerplate copies a of types in order to be able to derive arbitrary on them.

#[derive(Arbitrary, Debug)]
struct FuzzToken<S>(pub S, pub FuzzTok<S>);

impl<S> From<FuzzToken<S>> for Token<S> {
    fn from(ft: FuzzToken<S>) -> Token<S> {
        Token(ft.0, Tok::from(ft.1))
    }
}

#[derive(Arbitrary, Debug)]
enum FuzzStrPart<S, T> {
    /// string without escape sequences
    Str(S),
    /// interpolated term (`\(...)`)
    Term(T),
    /// escaped character (e.g. `\n`, `t`, `\u0041`)
    Char(char),
}

impl<S, T, D> From<FuzzStrPart<S, T>> for StrPart<S, D>
where
    D: From<T>,
{
    fn from(fsp: FuzzStrPart<S, T>) -> StrPart<S, D> {
        match fsp {
            FuzzStrPart::Str(s) => StrPart::Str(s),
            FuzzStrPart::Term(t) => StrPart::Term(D::from(t)),
            FuzzStrPart::Char(c) => StrPart::Char(c),
        }
    }
}

#[derive(Arbitrary, Debug)]
enum FuzzTok<S> {
    /// keywords such as `def`, but also identifiers such as `map`, `f::g`
    Word,
    /// variables such as `$x`
    Var,
    /// formatters such as `@csv`
    Fmt,
    /// number
    Num,
    /// (interpolated) string, surrounded by opening and closing '"'
    Str(Vec<FuzzStrPart<S, FuzzToken<S>>>),
    /// symbol such as `.`, `;`, `-`, `|`, or `+=`
    Sym,
    /// delimited tokens, e.g. `(...)` or `[...]`
    Block(Vec<FuzzToken<S>>),
}

impl<S> From<FuzzTok<S>> for Tok<S> {
    fn from(ft: FuzzTok<S>) -> Tok<S> {
        match ft {
            FuzzTok::Word => Tok::Word,
            FuzzTok::Var => Tok::Var,
            FuzzTok::Fmt => Tok::Fmt,
            FuzzTok::Num => Tok::Num,
            FuzzTok::Str(mut v) => Tok::Str(v.drain(..).map(|fsp| StrPart::from(fsp)).collect()),
            FuzzTok::Sym => Tok::Sym,
            FuzzTok::Block(mut v) => Tok::Block(v.drain(..).map(|fb| Token::from(fb)).collect()),
        }
    }
}

fuzz_target!(|data: Vec<FuzzToken<&str>>| {
    // The libFuzzer interface doesn't let us have data mutable.
    let mut tokens: Vec<Token<&str>> = Vec::with_capacity(data.len());
    for t in data {
        // Filter out some shallow bugs
        if t.0.len() < 1 {
            return;
        }
        if let FuzzToken(_, FuzzTok::Block(_)) = t {
            return;
        }
        if let FuzzToken(_, FuzzTok::Str(_)) = t {
            return;
        }
        tokens.push(Token::from(t));
    }
    let parser = Parser::new(tokens.as_slice());
    let _ = parser.parse(|p| p.defs());
});
