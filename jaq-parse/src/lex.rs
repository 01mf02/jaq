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
    /// delimited tokens, e.g. `(...)` or `[...]`
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

#[derive(Debug)]
pub enum Expect<'a> {
    Digit,
    Ident,
    Delim(&'a str),
    Escape,
    Unicode,
}

type Errors<'a> = Vec<(Expect<'a>, &'a str)>;

fn fail<'a>(e: Expect<'a>, i: &'a str, errs: &mut Errors<'a>) -> &'a str {
    errs.push((e, i));
    i
}

fn strip_digits(i: &str) -> Option<&str> {
    i.strip_prefix(|c: char| c.is_numeric())
        .map(|i| i.trim_start_matches(|c: char| c.is_numeric()))
}

/// Decimal with optional exponent.
fn trim_num<'a>(i: &'a str, e: &mut Errors<'a>) -> &'a str {
    let i = i.trim_start_matches(|c: char| c.is_numeric());
    let i = i.strip_prefix('.').map_or(i, |i| {
        strip_digits(i).unwrap_or_else(|| fail(Expect::Digit, i, e))
    });
    let i = i.strip_prefix(['e', 'E']).map_or(i, |i| {
        let i = i.strip_prefix(['+', '-']).unwrap_or(i);
        strip_digits(i).unwrap_or_else(|| fail(Expect::Digit, i, e))
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

fn token<'a>(i: &'a str, e: &mut Errors<'a>) -> Option<(Token<&'a str>, &'a str)> {
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
            let rest = strip_ident(chars.as_str())
                .unwrap_or_else(|| fail(Expect::Ident, chars.as_str(), e));
            (Token::Word(prefix(rest)), rest)
        }
        '0'..='9' => {
            let rest = trim_num(chars.as_str(), e);
            (Token::Num(prefix(rest)), rest)
        }
        '.' if chars.next() == Some('.') => punct(2, Punct::DotDot),
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
            let (parts, rest) = string(chars.as_str(), e)?;
            (Token::Str(parts), rest)
        }
        '(' | '[' | '{' => delim(i, e),
        _ => return None,
    })
}

use jaq_syn::string::Part;

/// Returns `None` when an unexpected EOF was encountered.
fn string<'a>(mut i: &'a str, e: &mut Errors<'a>) -> Option<(Vec<Part<Token<&'a str>>>, &'a str)> {
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
                    let c = u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32);
                    c.unwrap_or_else(|| {
                        e.push((Expect::Unicode, &rest[2..]));
                        '\u{FFFD}' // Unicode replacement character
                    })
                }
                '(' => {
                    let (trees, rest) = delim(&rest[1..], e);
                    parts.push(Part::Fun(trees));
                    i = rest;
                    continue;
                }
                _ => {
                    e.push((Expect::Escape, &rest[1..]));
                    continue;
                }
            },
            // SAFETY: due to `trim_start_matches`
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

fn tokens<'a>(mut i: &'a str, e: &mut Errors<'a>) -> (Vec<Token<&'a str>>, &'a str) {
    let mut tokens = Vec::new();
    while let Some((tk, rest)) = token(i, e) {
        tokens.push(tk);
        i = rest;
    }
    (tokens, i)
}

/// Parse a delimited sequence of tokens.
///
/// The input string has to start with either '(', '[', or '{'.
fn delim<'a>(i: &'a str, e: &mut Errors<'a>) -> (Token<&'a str>, &'a str) {
    let mut chars = i.chars();
    let delim = match chars.next().unwrap() {
        '(' => Delim::Paren,
        '[' => Delim::Brack,
        '{' => Delim::Brace,
        _ => panic!(),
    };
    let (tokens, rest) = tokens(chars.as_str(), e);
    let rest = trim_space(rest);
    let rest = rest
        .strip_prefix(delim.close())
        .unwrap_or_else(|| fail(Expect::Delim(i), rest, e));
    (Token::Delim(delim, tokens), rest)
}

/*
fn tokens_then<'a>(i: &'a str, e: &mut Errors<'a>, delim: Delim) -> (Token<&'a str>, &'a str) {
    let (tokens, i) = tokens(i, e);
    let i = trim_space(i);
    let i = i
        .strip_prefix(delim.close())
        .unwrap_or_else(|| fail(Expect::Delim(delim), i, e));
    (Token::Delim(delim, tokens), i)
}
*/

pub fn lex<'a>(i: &'a str, e: &mut Errors<'a>) -> (Vec<Token<&'a str>>, &'a str) {
    let (tokens, i) = tokens(i, e);
    let i = trim_space(i);
    (tokens, i)
}
