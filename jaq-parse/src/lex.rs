use crate::token::{Delim, Token as OToken};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use jaq_syn::string::Part;
use jaq_syn::{Span, Spanned};

/// Token (tree) generic over string type `S`.
#[derive(Debug)]
pub enum Token<S> {
    /// keywords such as `def`, but also identifiers such as `map`, `$x`, or `@csv`
    Word(S),
    /// number
    Num(S),
    /// interpolated string
    Str(Vec<Part<Self>>),
    /// operator, such as `|` or `+=`
    Op(S),
    /// punctuation, such as `.` or `;`
    Punct(Punct, S),
    /// delimited tokens, e.g. `(...)` or `[...]`
    Delim(Delim, Vec<Self>),
}

/// Punctuation.
#[derive(Copy, Clone, Debug)]
pub enum Punct {
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `?`
    Question,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
}

impl Punct {
    fn as_str(self) -> &'static str {
        match self {
            Self::Dot => ".",
            Self::DotDot => "..",
            Self::Question => "?",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Semicolon => ";",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expect<'a> {
    Digit,
    Ident,
    Delim(&'a str),
    Escape,
    Unicode,
}

type Error<'a> = (Expect<'a>, &'a str);

pub struct Lex<'a> {
    i: &'a str,
    e: Vec<Error<'a>>,
}

impl<'a> Lex<'a> {
    pub fn new(i: &'a str) -> Self {
        let e = Vec::new();
        Self { i, e }
    }

    pub fn lex(&mut self) -> Vec<Token<&'a str>> {
        let tokens = self.tokens();
        self.space();
        tokens
    }

    pub fn input(&self) -> &'a str {
        self.i
    }

    pub fn errors(&self) -> &[Error<'a>] {
        &self.e
    }

    fn next(&mut self) -> Option<char> {
        let mut chars = self.i.chars();
        let c = chars.next()?;
        self.i = chars.as_str();
        Some(c)
    }

    fn trim(&mut self, f: impl FnMut(char) -> bool) {
        self.i = self.i.trim_start_matches(f);
    }

    fn consumed(&mut self, chars: core::str::Chars<'a>, f: impl FnOnce(&mut Self)) -> &'a str {
        let start = self.i;
        self.i = chars.as_str();
        f(self);
        &start[..start.len() - self.i.len()]
    }

    /// Whitespace and comments.
    fn space(&mut self) {
        self.i = self.i.trim_start();
        while let Some(comment) = self.i.strip_prefix('#') {
            self.i = comment.trim_start_matches(|c| c != '\n').trim_start();
        }
    }

    /// Lex a sequence matching `[a-zA-Z0-9_]*`.
    fn ident0(&mut self) {
        self.trim(|c: char| c.is_ascii_alphanumeric() || c == '_');
    }

    /// Lex a sequence matching `[a-zA-Z_][a-zA-Z0-9_]*`.
    fn ident1(&mut self) {
        let first = |c: char| c.is_ascii_alphabetic() || c == '_';
        if let Some(rest) = self.i.strip_prefix(first) {
            self.i = rest;
            self.ident0();
        } else {
            self.e.push((Expect::Ident, self.i));
        }
    }

    /// Lex a non-empty digit sequence.
    fn digits1(&mut self) {
        if let Some(rest) = self.i.strip_prefix(|c: char| c.is_numeric()) {
            self.i = rest.trim_start_matches(|c: char| c.is_numeric());
        } else {
            self.e.push((Expect::Digit, self.i));
        }
    }

    /// Decimal with optional exponent.
    fn num(&mut self) {
        self.trim(|c| c.is_numeric());
        if let Some(i) = self.i.strip_prefix('.') {
            self.i = i;
            self.digits1();
        }
        if let Some(i) = self.i.strip_prefix(['e', 'E']) {
            self.i = i.strip_prefix(['+', '-']).unwrap_or(i);
            self.digits1();
        }
    }

    /// Lex a (possibly interpolated) string.
    ///
    /// The input string has to start with '"'.
    fn str(&mut self) -> Vec<Part<Token<&'a str>>> {
        let start = self.i;
        assert_eq!(self.next(), Some('"'));
        let mut parts = Vec::new();

        loop {
            let s = self.consumed(self.i.chars(), |lex| lex.trim(|c| c != '\\' && c != '"'));
            //if !s.is_empty() {
            parts.push(Part::Str(s.to_string()));
            //}
            match self.next() {
                Some('"') => return parts,
                Some('\\') => {
                    let mut chars = self.i.chars();
                    let c = match chars.next() {
                        Some(c @ ('\\' | '/' | '"')) => c,
                        Some('b') => '\x08',
                        Some('f') => '\x0C',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('u') => unicode(&mut chars).unwrap_or_else(|| {
                            self.e.push((Expect::Unicode, self.i));
                            '\u{FFFD}' // Unicode replacement character
                        }),
                        Some('(') => {
                            parts.push(Part::Fun(self.delim()));
                            continue;
                        }
                        Some(_) | None => {
                            self.e.push((Expect::Escape, self.i));
                            '\0'
                        }
                    };

                    self.i = chars.as_str();
                    parts.push(Part::Str(c.into()));
                }
                // SAFETY: due to `lex.trim()`
                Some(_) => unreachable!(),
                None => {
                    self.e.push((Expect::Delim(start), self.i));
                    return parts;
                }
            };
        }
    }

    fn punct(&mut self, p: Punct) -> Token<&'a str> {
        let (s, after) = self.i.split_at(p.as_str().len());
        self.i = after;
        Token::Punct(p, s)
    }

    fn token(&mut self) -> Option<Token<&'a str>> {
        self.space();

        let is_op = |c| "|=!<>+-*/%".contains(c);

        let mut chars = self.i.chars();
        Some(match chars.next()? {
            'a'..='z' | 'A'..='Z' | '_' => Token::Word(self.consumed(chars, |lex| lex.ident0())),
            '$' | '@' => Token::Word(self.consumed(chars, |lex| lex.ident1())),
            '0'..='9' => Token::Num(self.consumed(chars, |lex| lex.num())),
            c if is_op(c) => Token::Op(self.consumed(chars, |lex| lex.trim(is_op))),
            '.' if chars.next() == Some('.') => self.punct(Punct::DotDot),
            '.' => self.punct(Punct::Dot),
            ':' => self.punct(Punct::Colon),
            ';' => self.punct(Punct::Semicolon),
            ',' => self.punct(Punct::Comma),
            '?' => self.punct(Punct::Question),
            '"' => Token::Str(self.str()),
            '(' | '[' | '{' => self.delim(),
            _ => return None,
        })
    }

    fn tokens(&mut self) -> Vec<Token<&'a str>> {
        core::iter::from_fn(|| self.token()).collect()
    }

    /// Lex a sequence of tokens that is surrounded by parentheses, curly braces, or brackets.
    ///
    /// The input string has to start with either '(', '[', or '{'.
    fn delim(&mut self) -> Token<&'a str> {
        let start = self.i;
        let delim = match self.next() {
            Some('(') => Delim::Paren,
            Some('[') => Delim::Brack,
            Some('{') => Delim::Brace,
            _ => panic!(),
        };
        let tokens = self.tokens();

        self.space();
        if let Some(rest) = self.i.strip_prefix(delim.close()) {
            self.i = rest
        } else {
            self.e.push((Expect::Delim(start), self.i));
        }
        Token::Delim(delim, tokens)
    }
}

fn unicode(chars: &mut core::str::Chars) -> Option<char> {
    let mut hex = String::with_capacity(4);
    for _ in 0..4 {
        hex.push(chars.next()?);
    }
    u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
}

fn span(whole_buffer: &str, part: &str) -> Span {
    let start = part.as_ptr() as usize - whole_buffer.as_ptr() as usize;
    let end = start + part.len();
    start..end
}

impl<'a> Token<&'a str> {
    pub fn tokens(self, i: &'a str) -> Box<dyn Iterator<Item = Spanned<OToken>> + 'a> {
        use core::iter::once;
        match self {
            Self::Word(w) => Box::new(once((
                match w {
                    "def" => OToken::Def,
                    "if" => OToken::If,
                    "then" => OToken::Then,
                    "elif" => OToken::Elif,
                    "else" => OToken::Else,
                    "end" => OToken::End,
                    "or" => OToken::Or,
                    "and" => OToken::And,
                    "as" => OToken::As,
                    "reduce" => OToken::Reduce,
                    "for" => OToken::For,
                    "foreach" => OToken::Foreach,
                    "try" => OToken::Try,
                    "catch" => OToken::Catch,
                    w if w.starts_with("$") => OToken::Var(w[1..].to_string()),
                    w => OToken::Ident(w.to_string()),
                },
                span(i, w),
            ))),
            Self::Num(n) => Box::new(once((OToken::Num(n.to_string()), span(i, n)))),
            Self::Op(o) => Box::new(once((OToken::Op(o.to_string()), span(i, o)))),
            Self::Punct(p, s) => Box::new(once((
                match p {
                    Punct::Dot => OToken::Dot,
                    Punct::DotDot => OToken::DotDot,
                    Punct::Question => OToken::Question,
                    Punct::Comma => OToken::Comma,
                    Punct::Colon => OToken::Colon,
                    Punct::Semicolon => OToken::Semicolon,
                },
                span(i, s),
            ))),
            Self::Delim(delim, tokens) => {
                let init = once((OToken::Open(delim), 0..0));
                let last = once((OToken::Close(delim), 0..0));
                Box::new(init.chain(tokens.into_iter().flat_map(|t| t.tokens(i)).chain(last)))
            }
            Self::Str(parts) => {
                let quote = once((OToken::Quote, 0..0));
                let f = |part: Part<Token<&'a str>>| match part {
                    Part::Fun(t) => t.tokens(i),
                    Part::Str(s) => Box::new(once((OToken::Str(s.to_string()), 0..0))),
                };
                Box::new(
                    quote
                        .clone()
                        .chain(parts.into_iter().flat_map(f))
                        .chain(quote),
                )
            }
        }
    }
}
