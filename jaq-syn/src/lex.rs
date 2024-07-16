//! Lexing.

use alloc::vec::Vec;

#[derive(Debug)]
pub enum StrPart<S, F> {
    Str(S),
    Filter(F),
    Char(char),
}

/// Token (tree) generic over string type `S`.
#[derive(Debug)]
pub enum Token<S> {
    /// keywords such as `def`, but also identifiers such as `map`, `$x`, or `@csv`
    Word(S),
    /// number
    Num(S),
    /// (interpolated) string, surrounded by opening and closing '"'
    Str(S, Vec<StrPart<S, Self>>, S),
    /// operator, such as `|` or `+=`
    Op(S),
    /// punctuation, such as `.` or `;`
    Char(S),
    /// delimited tokens, e.g. `(...)` or `[...]`
    Block(S, Vec<Self>),
}

#[derive(Clone, Debug)]
pub enum Expect<S> {
    Digit,
    Ident,
    Delim(S),
    Escape,
    Unicode,
    Token,
}

impl<'a> Expect<&'a str> {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Digit => "digit",
            Self::Ident => "identifier",
            Self::Delim("(") => "closing parenthesis",
            Self::Delim("[") => "closing bracket",
            Self::Delim("{") => "closing brace",
            Self::Delim("\"") => "closing quote",
            Self::Delim(_) => panic!(),
            Self::Escape => "string escape sequence",
            Self::Unicode => "4-digit hexadecimal UTF-8 code point",
            Self::Token => "token",
        }
    }
}

/// Lexer error, storing what we expected and what we got instead.
pub type Error<S> = (Expect<S>, S);

/// Lexer for jq files.
pub struct Lexer<S> {
    i: S,
    e: Vec<Error<S>>,
}

impl<'a> Lexer<&'a str> {
    /// Initialise a new lexer for the given input.
    #[must_use]
    pub fn new(i: &'a str) -> Self {
        let e = Vec::new();
        Self { i, e }
    }

    /// Lex, returning the resulting tokens and errors.
    #[must_use]
    pub fn lex(mut self) -> Result<Vec<Token<&'a str>>, Vec<Error<&'a str>>> {
        let tokens = self.tokens();
        self.space();
        if !self.i.is_empty() {
            self.e.push((Expect::Token, self.i));
        }

        if self.e.is_empty() {
            Ok(tokens)
        } else {
            Err(self.e)
        }
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

    fn consumed(&mut self, skip: usize, f: impl FnOnce(&mut Self)) -> &'a str {
        let start = self.i;
        self.i = &self.i[skip..];
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

    fn mod_then_ident(&mut self) {
        self.ident0();
        if let Some(rest) = self.i.strip_prefix("::") {
            self.i = rest.strip_prefix(['@', '$']).unwrap_or(rest);
            self.ident1();
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
        if let Some(rest) = self.i.strip_prefix(|c: char| c.is_ascii_digit()) {
            self.i = rest.trim_start_matches(|c: char| c.is_ascii_digit());
        } else {
            self.e.push((Expect::Digit, self.i));
        }
    }

    /// Decimal with optional exponent.
    fn num(&mut self) {
        self.trim(|c| c.is_ascii_digit());
        if let Some(i) = self.i.strip_prefix('.') {
            self.i = i;
            self.digits1();
        }
        if let Some(i) = self.i.strip_prefix(['e', 'E']) {
            self.i = i.strip_prefix(['+', '-']).unwrap_or(i);
            self.digits1();
        }
    }

    fn escape(&mut self) -> Option<StrPart<&'a str, Token<&'a str>>> {
        let mut chars = self.i.chars();
        let part = match chars.next() {
            Some(c @ ('\\' | '/' | '"')) => StrPart::Char(c),
            Some('b') => StrPart::Char('\x08'),
            Some('f') => StrPart::Char('\x0C'),
            Some('n') => StrPart::Char('\n'),
            Some('r') => StrPart::Char('\r'),
            Some('t') => StrPart::Char('\t'),
            Some('u') => {
                let mut hex = 0;
                for _ in 0..4 {
                    let i = chars.as_str();
                    match chars.next().and_then(|c| c.to_digit(16)) {
                        Some(digit) => hex = (hex << 4) + digit,
                        None => {
                            self.i = i;
                            self.e.push((Expect::Unicode, self.i));
                            return None;
                        }
                    }
                }
                StrPart::Char(char::from_u32(hex).unwrap())
            }
            Some('(') => return Some(StrPart::Filter(self.delim())),
            Some(_) | None => {
                self.e.push((Expect::Escape, self.i));
                return None;
            }
        };

        self.i = chars.as_str();
        Some(part)
    }

    /// Lex a (possibly interpolated) string.
    ///
    /// The input string has to start with '"'.
    fn str(&mut self) -> Token<&'a str> {
        let start = self.take(1);
        assert_eq!(start, "\"");
        let mut parts = Vec::new();

        loop {
            let s = self.consumed(0, |lex| lex.trim(|c| c != '\\' && c != '"'));
            if !s.is_empty() {
                parts.push(StrPart::Str(s));
            }
            let i = self.i;
            match self.next() {
                Some('"') => return Token::Str(start, parts, &i[..1]),
                Some('\\') => self.escape().map(|part| parts.push(part)),
                // SAFETY: due to `lex.trim()`
                Some(_) => unreachable!(),
                None => {
                    self.e.push((Expect::Delim(start), self.i));
                    return Token::Str(start, parts, &i[..0]);
                }
            };
        }
    }

    fn token(&mut self) -> Option<Token<&'a str>> {
        self.space();

        let is_op = |c| "|=!<>+-*/%".contains(c);

        let mut chars = self.i.chars();
        Some(match chars.next()? {
            'a'..='z' | 'A'..='Z' | '_' => Token::Word(self.consumed(1, Self::mod_then_ident)),
            '$' | '@' => Token::Word(self.consumed(1, Self::ident1)),
            '0'..='9' => Token::Num(self.consumed(1, Self::num)),
            c if is_op(c) => Token::Op(self.consumed(1, |lex| lex.trim(is_op))),
            '?' if (chars.next(), chars.next()) == (Some('/'), Some('/')) => {
                Token::Op(self.take(3))
            }
            '.' if chars.next() == Some('.') => Token::Char(self.take(2)),
            '.' | ':' | ';' | ',' | '?' => Token::Char(self.take(1)),
            '"' => self.str(),
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
        let open = self.take(1);
        let close = match open {
            "(" => ')',
            "[" => ']',
            "{" => '}',
            _ => panic!(),
        };
        let mut tokens = self.tokens();

        self.space();
        if let Some(rest) = self.i.strip_prefix(close) {
            tokens.push(Token::Char(&self.i[..1]));
            self.i = rest;
        } else {
            self.e.push((Expect::Delim(open), self.i));
        }
        Token::Block(open, tokens)
    }
}

impl<'a> Token<&'a str> {
    /// Return the span of a token that was lexed from some given input.
    pub fn span(&self, code: &str) -> crate::Span {
        match self {
            Self::Word(s) | Self::Char(s) | Self::Op(s) | Self::Num(s) => span(code, s),
            Self::Str(open, _, close) => span(code, open).start..span(code, close).end,
            Self::Block(open, block) => {
                span(code, open).start..block.last().unwrap().span(code).end
            }
        }
    }
}

/// Return the span of a string slice `part` relative to a string slice `whole`.
///
/// The caller must ensure that `part` is fully contained inside `whole`.
pub fn span(whole: &str, part: &str) -> crate::Span {
    let start = part.as_ptr() as usize - whole.as_ptr() as usize;
    start..start + part.len()
}
