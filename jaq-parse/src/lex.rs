use crate::token::Delim;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

/// Token (tree) generic over string type `S`.
#[derive(Debug)]
pub enum Token<S> {
    /// keywords such as `def`, but also identifiers such as `map`, `$x`, or `@csv`
    Word(S),
    /// number
    Num(S),
    /// interpolated string
    Str(Vec<jaq_syn::string::Part<Self>>),
    /// operator, such as `|` or `+=`
    Op(S),
    /// punctuation, such as `.` or `;`
    Punct(Punct, S),
    /// delimited tokens, e.g. `(...)` or `{...}`
    Delim(Delim, Vec<Self>),
}

/// Punctuation.
#[derive(Debug)]
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

fn strip_digits(i: &str) -> Option<&str> {
    i.strip_prefix(|c: char| c.is_numeric())
        .map(|i| i.trim_start_matches(|c: char| c.is_numeric()))
}

/// Decimal with optional exponent.
fn trim_num(i: &str) -> &str {
    let i = i.trim_start_matches(|c: char| c.is_numeric());
    let i = i.strip_prefix('.').map_or(i, |i| {
        strip_digits(i).unwrap_or_else(|| {
            // TODO: register error
            todo!();
            i
        })
    });
    let i = i.strip_prefix(['e', 'E']).map_or(i, |i| {
        let i = i.strip_prefix(['+', '-']).unwrap_or(i);
        strip_digits(i).unwrap_or_else(|| {
            // TODO: register error
            todo!();
            i
        })
    });
    i
}

fn trim_ident(i: &str) -> &str {
    i.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '_')
}

fn strip_ident(i: &str) -> Option<&str> {
    i.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_')
        .map(trim_ident)
}

fn token(i: &str) -> Option<(Token<&str>, &str)> {
    let i = trim_space(i);

    let is_op = |c| "|=!<>+-*/%".contains(c);
    let prefix = |rest: &str| &i[..i.len() - rest.len()];
    let punct = |len: usize, p: Punct| (Token::Punct(p, &i[..len]), &i[len..]);

    let mut chars = i.chars();
    Some(match chars.next()? {
        'a'..='z' | 'A'..='Z' | '_' => {
            let rest = trim_ident(chars.as_str());
            (Token::Word(prefix(rest)), rest)
        }
        '$' | '@' => {
            // TODO: handle error
            let rest = strip_ident(chars.as_str()).unwrap();
            (Token::Word(prefix(rest)), rest)
        }
        '0'..='9' => {
            let rest = trim_num(chars.as_str());
            (Token::Num(prefix(rest)), rest)
        }
        '.' if chars.next()? == '.' => punct(2, Punct::DotDot),
        '.' => punct(1, Punct::Dot),
        ':' => punct(1, Punct::Colon),
        ';' => punct(1, Punct::Semicolon),
        ',' => punct(1, Punct::Comma),
        '?' => punct(1, Punct::Question),
        c if is_op(c) => {
            let rest = chars.as_str().trim_start_matches(is_op);
            (Token::Op(prefix(rest)), rest)
        }
        '"' => {
            let (parts, rest) = string(chars.as_str())?;
            (Token::Str(parts), rest)
        }
        '(' => tokens_then(chars.as_str(), Delim::Paren),
        '[' => tokens_then(chars.as_str(), Delim::Brack),
        '{' => tokens_then(chars.as_str(), Delim::Brace),
        _ => return None,
    })
}

use jaq_syn::string::Part;

/// Returns `None` when an unexpected EOF was encountered.
fn string(mut i: &str) -> Option<(Vec<Part<Token<&str>>>, &str)> {
    let mut parts = Vec::new();

    loop {
        let rest = i.trim_start_matches(|c| c != '\\' && c != '"');
        let s = &i[..i.len() - rest.len()];
        if !s.is_empty() {
            parts.push(Part::Str(s.to_string()))
        }
        let mut chars = rest.chars();
        let c = match chars.next()? {
            '"' => return Some((parts, chars.as_str())),
            '\\' => match chars.next()? {
                c @ ('\\' | '/' | '"') => c,
                'b' => '\x08',
                'f' => '\x0C',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'u' => {
                    let mut hex = String::with_capacity(4);
                    (0..4).try_for_each(|_| Some(hex.push(chars.next()?)))?;
                    let num = u32::from_str_radix(&hex, 16).unwrap();
                    char::from_u32(num).unwrap_or_else(|| {
                        //emit(Simple::custom(span, "invalid unicode character"));
                        '\u{FFFD}' // unicode replacement character
                    })
                }
                '(' => {
                    let (trees, rest) = tokens_then(chars.as_str(), Delim::Paren);
                    parts.push(Part::Fun(trees));
                    i = rest;
                    continue;
                }
                _ => todo!("add error"),
            },
            _ => unreachable!(),
        };
        parts.push(Part::Str(c.into()));
        i = chars.as_str();
    }
}

/// Whitespace and comments.
fn trim_space(i: &str) -> &str {
    let mut i = i.trim_start();
    while let Some(comment) = i.strip_prefix('#') {
        i = comment.trim_start_matches(|c| c != '\n').trim_start();
    }
    i
}

/*
use jaq_syn::Spanned;
fn parts_to_interpol(
    parts: Vec<Part<Tree>>,
) -> (Spanned<String>, Vec<(Spanned<Tree>, Spanned<String>)>) {
    let mut init = (String::new(), 0..42);
    let mut tail = Vec::new();
    let mut parts = parts.into_iter();
    while let Some(part) = parts.next() {
        match part {
            Part::Str(s) => init.0.extend(s.chars()),
            Part::Fun(f) => {
                tail.push(((f, 0..42), (String::new(), 0..42)));
                while let Some(part) = parts.next() {
                    match part {
                        Part::Str(s) => tail.last_mut().unwrap().1 .0.extend(s.chars()),
                        Part::Fun(f) => tail.push(((f, 0..42), (String::new(), 0..42))),
                    }
                }
            }
        }
    }
    (init, tail)
}
*/

fn tokens(mut i: &str) -> (Vec<Token<&str>>, &str) {
    let mut tokens = Vec::new();
    while let Some((tk, rest)) = token(i) {
        tokens.push(tk);
        i = rest;
    }
    (tokens, i)
}

fn tokens_then(i: &str, delim: Delim) -> (Token<&str>, &str) {
    let (tokens, i) = tokens(i);
    let i = trim_space(i);
    let i = i.strip_prefix(delim.close()).unwrap_or_else(|| {
        todo!("add error");
        i
    });
    (Token::Delim(delim, tokens), i)
}

pub fn lex_(i: &str) -> (Vec<Token<&str>>, &str) {
    let (tokens, i) = tokens(i);
    let i = trim_space(i);
    (tokens, i)
}
