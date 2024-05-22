use crate::token::{Delim, Token as OToken};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use jaq_syn::string::Part;
use jaq_syn::{Span, Spanned};

#[derive(Debug)]
pub enum StrPart<S, F> {
    Str(S),
    Filter(F),
    Char(char),
    Unicode(u32),
}

/// Token (tree) generic over string type `S`.
#[derive(Debug)]
pub enum Token<S> {
    /// keywords such as `def`, but also identifiers such as `map`, `$x`, or `@csv`
    Word(S),
    /// number
    Num(S),
    /// interpolated string
    Str(Vec<StrPart<S, Self>>),
    /// operator, such as `|` or `+=`
    Op(S),
    /// punctuation, such as `.` or `;`
    Char(S),
    /// delimited tokens, e.g. `(...)` or `[...]`
    Block(S, Vec<Self>),
}

#[derive(Clone, Debug)]
pub enum Expect<'a> {
    Digit,
    Ident,
    Delim(&'a str),
    Escape,
    Unicode,
    Token,
}

impl<'a> Expect<'a> {
    pub fn to_simple_error(&self, pos: &'a str, full: &'a str) -> (&'static str, Span) {
        let mut pos = span(full, pos);
        pos.end = pos.start;
        let s = match self {
            Self::Digit => "expected digit",
            Self::Ident => "expected identifier",
            Self::Delim(start) => {
                let mut start = span(full, start);
                start.end = pos.start;
                return ("unclosed delimiter", start);
            }
            Self::Escape => "expected string escape sequence",
            Self::Unicode => "expected 4-digit hexadecimal UTF-8 code point",
            Self::Token => "expected token",
        };
        (s, pos)
    }
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

    pub fn lex(mut self) -> (Vec<Token<&'a str>>, Vec<Error<'a>>) {
        let tokens = self.tokens();
        self.space();
        if !self.i.is_empty() {
            self.e.push((Expect::Token, self.i));
        }
        (tokens, self.e)
    }

    fn next(&mut self) -> Option<char> {
        let mut chars = self.i.chars();
        let c = chars.next()?;
        self.i = chars.as_str();
        Some(c)
    }

    fn take(&mut self, len: usize) -> &'a str {
        let (head, tail) = self.i.split_at(len);
        self.i = tail;
        head
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
    fn str(&mut self) -> Vec<StrPart<&'a str, Token<&'a str>>> {
        let start = self.i;
        assert_eq!(self.next(), Some('"'));
        let mut parts = Vec::new();

        loop {
            let s = self.consumed(self.i.chars(), |lex| lex.trim(|c| c != '\\' && c != '"'));
            if !s.is_empty() {
                parts.push(StrPart::Str(s));
            }
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
                        Some('u') => {
                            let unicode = unicode(&mut chars).unwrap_or_else(|| {
                                self.e.push((Expect::Unicode, self.i));
                                0xFFFD // Unicode replacement character
                            });
                            parts.push(StrPart::Unicode(unicode));
                            continue;
                        }
                        Some('(') => {
                            parts.push(StrPart::Filter(self.delim()));
                            continue;
                        }
                        Some(_) | None => {
                            self.e.push((Expect::Escape, self.i));
                            '\0'
                        }
                    };

                    self.i = chars.as_str();
                    parts.push(StrPart::Char(c));
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

    fn token(&mut self) -> Option<Token<&'a str>> {
        self.space();

        let is_op = |c| "|=!<>+-*/%".contains(c);

        let mut chars = self.i.chars();
        Some(match chars.next()? {
            'a'..='z' | 'A'..='Z' | '_' => Token::Word(self.consumed(chars, |lex| lex.ident0())),
            '$' | '@' => Token::Word(self.consumed(chars, |lex| lex.ident1())),
            '0'..='9' => Token::Num(self.consumed(chars, |lex| lex.num())),
            c if is_op(c) => Token::Op(self.consumed(chars, |lex| lex.trim(is_op))),
            '?' if (chars.next(), chars.next()) == (Some('/'), Some('/')) => {
                Token::Op(self.take(3))
            }
            '.' if chars.next() == Some('.') => Token::Char(self.take(2)),
            '.' | ':' | ';' | ',' | '?' => Token::Char(self.take(1)),
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
        let open = &self.i[..1];
        let close = match self.next() {
            Some('(') => ')',
            Some('[') => ']',
            Some('{') => '}',
            _ => panic!(),
        };
        let mut tokens = self.tokens();

        self.space();
        if let Some(rest) = self.i.strip_prefix(close) {
            tokens.push(Token::Char(&self.i[..1]));
            self.i = rest
        } else {
            self.e.push((Expect::Delim(start), self.i));
        }
        Token::Block(open, tokens)
    }
}

fn unicode(chars: &mut core::str::Chars) -> Option<u32> {
    let mut hex = String::with_capacity(4);
    for _ in 0..4 {
        hex.push(chars.next()?);
    }
    u32::from_str_radix(&hex, 16).ok()
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
            Self::Char(c) => {
                let token = match c {
                    ".." => OToken::DotDot,
                    "." => OToken::Dot,
                    "?" => OToken::Question,
                    "," => OToken::Comma,
                    ":" => OToken::Colon,
                    ";" => OToken::Semicolon,
                    ")" => OToken::Close(Delim::Paren),
                    "]" => OToken::Close(Delim::Brack),
                    "}" => OToken::Close(Delim::Brace),
                    _ => panic!("{}", c),
                };
                Box::new(once((token, span(i, c))))
            }
            Self::Block(open, tokens) => {
                let delim = match open {
                    "(" => Delim::Paren,
                    "[" => Delim::Brack,
                    "{" => Delim::Brace,
                    _ => panic!(),
                };
                let init = once((OToken::Open(delim), span(i, open)));
                Box::new(init.chain(tokens.into_iter().flat_map(|t| t.tokens(i))))
            }
            Self::Str(parts) => {
                let quote = once((OToken::Quote, 0..0));
                let f = |part: StrPart<&'a str, Token<&'a str>>| match part {
                    StrPart::Filter(t) => t.tokens(i),
                    StrPart::Str(s) => Box::new(once((OToken::Str(s.to_string()), 0..0))),
                    StrPart::Char(c) => Box::new(once((OToken::Str(c.to_string()), 0..0))),
                    StrPart::Unicode(_) => todo!(),
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
