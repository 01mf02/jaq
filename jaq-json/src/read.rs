//! JSON support.
use crate::{Map, Num, Val};
use alloc::{string::ToString, vec::Vec};
use core::fmt::{self, Formatter};
use hifijson::token::{Expect, Lex};
use hifijson::{LexAlloc, SliceLexer};
#[cfg(feature = "std")]
use std::io;

// ---------------------------------------------------------------------------
// Plain JSON: uses hifijson native methods, only `#` comments (upstream default)
// ---------------------------------------------------------------------------

/// Eat whitespace and `#` comments, then peek at next character.
fn ws_tk<L: Lex>(lexer: &mut L) -> Option<u8> {
    loop {
        lexer.eat_whitespace();
        match lexer.peek_next() {
            Some(b'#') => lexer.skip_until(|c| c == b'\n'),
            next => return next,
        }
    }
}

/// Parse a JSON value (plain).
fn parse<L: LexAlloc>(next: u8, lexer: &mut L) -> Result<Val, hifijson::Error> {
    Ok(match next {
        b'n' if lexer.strip_prefix(b"null") => Val::Null,
        b't' if lexer.strip_prefix(b"true") => Val::Bool(true),
        b'f' if lexer.strip_prefix(b"false") => Val::Bool(false),
        b'b' if lexer.strip_prefix(b"b\"") => Val::byte_str(parse_string(lexer, true)?),
        b'N' if lexer.strip_prefix(b"NaN") => Val::Num(Num::Float(f64::NAN)),
        b'I' if lexer.strip_prefix(b"Infinity") => Val::Num(Num::Float(f64::INFINITY)),
        b'0'..=b'9' | b'+' | b'-' => Val::Num(parse_num(lexer)?),
        b'"' => Val::utf8_str(parse_string(lexer.discarded(), false)?),
        b'[' => Val::Arr({
            let mut arr = Vec::new();
            lexer.discarded().seq(b']', ws_tk, |next, lexer| {
                arr.push(parse(next, lexer)?);
                Ok::<_, hifijson::Error>(())
            })?;
            arr.into()
        }),
        b'{' => Val::obj({
            let mut obj = Map::default();
            lexer.discarded().seq(b'}', ws_tk, |next, lexer| {
                let key = parse(next, lexer)?;
                lexer.expect(ws_tk, b':').ok_or(Expect::Colon)?;
                let value = parse(ws_tk(lexer).ok_or(Expect::Value)?, lexer)?;
                obj.insert(key, value);
                Ok::<_, hifijson::Error>(())
            })?;
            obj
        }),
        _ => Err(Expect::Value)?,
    })
}

// ---------------------------------------------------------------------------
// JSONC: supports `#`, `//`, and `/* */` comments with error reporting
// ---------------------------------------------------------------------------

/// Eat whitespace/comments, then peek at next character.
///
/// Supports `#` (hash), `//` (single-line), and `/* */` (block) comments.
/// Returns `Err` for incomplete comments (bare `/` or unterminated `/* ...`).
fn ws_tk_jsonc<L: Lex>(lexer: &mut L) -> Result<Option<u8>, Cause> {
    loop {
        lexer.eat_whitespace();
        match lexer.peek_next() {
            Some(b'#') => lexer.skip_until(|c| c == b'\n'),
            Some(b'/') => {
                lexer.take_next();
                match lexer.peek_next() {
                    Some(b'/') => {
                        lexer.skip_until(|c| c == b'\n');
                    }
                    Some(b'*') => {
                        lexer.take_next();
                        let mut prev = 0u8;
                        lexer.skip_until(|c| {
                            let found = prev == b'*' && c == b'/';
                            prev = c;
                            found
                        });
                        if lexer.take_next().is_none() {
                            return Err(Cause::UnterminatedBlockComment);
                        }
                    }
                    _ => return Err(Cause::BareSlash),
                }
            }
            next => return Ok(next),
        }
    }
}

/// Like [`Lex::exactly_one`], but accepts a peek function returning `Result`.
fn exactly_one_ws<L: Lex, T, E: From<Expect>, PF, F>(
    lexer: &mut L,
    mut pf: PF,
    f: F,
) -> Result<T, E>
where
    PF: FnMut(&mut L) -> Result<Option<u8>, E>,
    F: FnOnce(u8, &mut L) -> Result<T, E>,
{
    let next = pf(lexer)?.ok_or(Expect::Value)?;
    let v = f(next, lexer)?;
    match pf(lexer)? {
        None => Ok(v),
        Some(_) => Err(Expect::Eof)?,
    }
}

/// Like [`Lex::seq`], but accepts a peek function returning `Result`.
fn seq_ws<L: Lex, E: From<Expect>, PF, F>(
    lexer: &mut L,
    end: u8,
    mut pf: PF,
    mut f: F,
) -> Result<(), E>
where
    PF: FnMut(&mut L) -> Result<Option<u8>, E>,
    F: FnMut(u8, &mut L) -> Result<(), E>,
{
    let mut next = pf(lexer)?.ok_or(Expect::ValueOrEnd)?;
    if next == end {
        lexer.take_next();
        return Ok(());
    }

    loop {
        f(next, lexer)?;
        next = pf(lexer)?.ok_or(Expect::CommaOrEnd)?;
        if next == end {
            lexer.take_next();
            return Ok(());
        } else if next == b',' {
            lexer.take_next();
            next = pf(lexer)?.ok_or(Expect::Value)?;
        } else {
            return Err(Expect::CommaOrEnd)?;
        }
    }
}

/// Like [`Lex::expect`], but accepts a peek function returning `Result`.
fn expect_ws<L: Lex, E>(
    lexer: &mut L,
    pf: impl FnOnce(&mut L) -> Result<Option<u8>, E>,
    expect: u8,
) -> Result<Option<()>, E> {
    if pf(lexer)? == Some(expect) {
        Ok(lexer.take_next().map(|_| ()))
    } else {
        Ok(None)
    }
}

/// Parse a JSON value with JSONC comment support.
fn parse_jsonc<L: LexAlloc>(next: u8, lexer: &mut L) -> Result<Val, Cause> {
    Ok(match next {
        b'n' if lexer.strip_prefix(b"null") => Val::Null,
        b't' if lexer.strip_prefix(b"true") => Val::Bool(true),
        b'f' if lexer.strip_prefix(b"false") => Val::Bool(false),
        b'b' if lexer.strip_prefix(b"b\"") => Val::byte_str(parse_string(lexer, true)?),
        b'N' if lexer.strip_prefix(b"NaN") => Val::Num(Num::Float(f64::NAN)),
        b'I' if lexer.strip_prefix(b"Infinity") => Val::Num(Num::Float(f64::INFINITY)),
        b'0'..=b'9' | b'+' | b'-' => Val::Num(parse_num(lexer)?),
        b'"' => Val::utf8_str(parse_string(lexer.discarded(), false)?),
        b'[' => Val::Arr({
            let mut arr = Vec::new();
            seq_ws(lexer.discarded(), b']', ws_tk_jsonc, |next, lexer| {
                arr.push(parse_jsonc(next, lexer)?);
                Ok::<_, Cause>(())
            })?;
            arr.into()
        }),
        b'{' => Val::obj({
            let mut obj = Map::default();
            seq_ws(lexer.discarded(), b'}', ws_tk_jsonc, |next, lexer| {
                let key = parse_jsonc(next, lexer)?;
                expect_ws(lexer, ws_tk_jsonc, b':')?.ok_or(Expect::Colon)?;
                let value = parse_jsonc(ws_tk_jsonc(lexer)?.ok_or(Expect::Value)?, lexer)?;
                obj.insert(key, value);
                Ok::<_, Cause>(())
            })?;
            obj
        }),
        _ => Err(Expect::Value)?,
    })
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// Internal cause of a parse error.
#[derive(Debug)]
enum Cause {
    /// A standard hifijson parse error.
    Hifi(hifijson::Error),
    /// A bare `/` not followed by `/` or `*`.
    BareSlash,
    /// An unterminated `/* */` block comment.
    UnterminatedBlockComment,
}

impl From<hifijson::Error> for Cause {
    fn from(e: hifijson::Error) -> Self {
        Self::Hifi(e)
    }
}

impl From<Expect> for Cause {
    fn from(e: Expect) -> Self {
        Self::Hifi(hifijson::Error::Token(e))
    }
}

impl core::fmt::Display for Cause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Hifi(e) => e.fmt(f),
            Self::BareSlash => f.write_str("bare `/` is not a valid comment"),
            Self::UnterminatedBlockComment => f.write_str("unterminated block comment"),
        }
    }
}

/// Parse error.
#[derive(Debug)]
pub struct Error(usize, Cause);

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "byte offset {}: {}", self.0, self.1)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

#[cfg(feature = "std")]
impl std::error::Error for Cause {}

/// Parse a JSON string as byte or text string, preserving invalid UTF-8 as-is.
fn parse_string<L: LexAlloc>(lexer: &mut L, bytes: bool) -> Result<Vec<u8>, hifijson::Error> {
    let on_string = |bytes: &mut L::Bytes, out: &mut Vec<u8>| {
        out.extend(bytes.as_ref());
        Ok(())
    };
    let s = lexer.str_fold(Vec::new(), on_string, |lexer, out| {
        use hifijson::escape::Error;
        match lexer.take_next().ok_or(Error::Eof)? {
            b'u' if bytes => Err(Error::InvalidKind(b'u'))?,
            b'x' if bytes => out.push(lexer.hex()?),
            c => out.extend(lexer.escape(c)?.encode_utf8(&mut [0; 4]).as_bytes()),
        }
        Ok(())
    });
    s.map_err(hifijson::Error::Str)
}

fn parse_num<L: LexAlloc>(lexer: &mut L) -> Result<Num, hifijson::Error> {
    let num = hifijson::num::Num::signed_digits();
    let (num, parts) = lexer.num_string_with(num).unvalidated();
    let num = num.as_ref();
    Ok(match num {
        "+" if lexer.strip_prefix(b"Infinity") => Num::Float(f64::INFINITY),
        "-" if lexer.strip_prefix(b"Infinity") => Num::Float(f64::NEG_INFINITY),
        _ if num.ends_with(|c: char| c.is_ascii_digit()) => {
            if parts.is_int() {
                Num::from_str_radix(num, 10).unwrap()
            } else {
                Num::Dec(num.to_string().into())
            }
        }
        _ => Err(hifijson::num::Error::ExpectedDigit)?,
    })
}

// ---------------------------------------------------------------------------
// Public API: plain JSON
// ---------------------------------------------------------------------------

/// Parse exactly one JSON value.
pub fn parse_single(slice: &[u8]) -> Result<Val, Error> {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    lexer
        .exactly_one(ws_tk, parse)
        .map_err(|e| Error(offset(lexer.as_slice()), Cause::Hifi(e)))
}

/// Parse a sequence of JSON values.
pub fn parse_many(slice: &[u8]) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || {
        Some(
            parse(ws_tk(&mut lexer)?, &mut lexer)
                .map_err(|e| Error(offset(lexer.as_slice()), Cause::Hifi(e))),
        )
    })
}

#[cfg(feature = "std")]
/// Read a sequence of JSON values.
pub fn read_many<'a>(read: impl io::BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let invalid_data = |e: hifijson::Error| io::Error::new(io::ErrorKind::InvalidData, e);
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = ws_tk(&mut lexer).map(|next| parse(next, &mut lexer).map_err(invalid_data));
        // always return I/O error if present, regardless of the output value!
        lexer.error.take().map(Err).or(v)
    })
}

// ---------------------------------------------------------------------------
// Public API: JSONC (JSON with Comments)
// ---------------------------------------------------------------------------

/// Parse exactly one JSONC value.
pub fn parse_single_jsonc(slice: &[u8]) -> Result<Val, Error> {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    exactly_one_ws(&mut lexer, ws_tk_jsonc, parse_jsonc)
        .map_err(|e| Error(offset(lexer.as_slice()), e))
}

/// Parse a sequence of JSONC values.
pub fn parse_many_jsonc(slice: &[u8]) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || {
        let next = match ws_tk_jsonc(&mut lexer) {
            Ok(Some(next)) => next,
            Ok(None) => return None,
            Err(e) => return Some(Err(Error(offset(lexer.as_slice()), e))),
        };
        Some(parse_jsonc(next, &mut lexer).map_err(|e| Error(offset(lexer.as_slice()), e)))
    })
}

#[cfg(feature = "std")]
/// Read a sequence of JSONC values.
pub fn read_many_jsonc<'a>(
    read: impl io::BufRead + 'a,
) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let invalid_data = |e: Cause| io::Error::new(io::ErrorKind::InvalidData, e);
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = match ws_tk_jsonc(&mut lexer) {
            Ok(Some(next)) => Some(parse_jsonc(next, &mut lexer).map_err(invalid_data)),
            Ok(None) => None,
            Err(e) => Some(Err(invalid_data(e))),
        };
        // always return I/O error if present, regardless of the output value!
        lexer.error.take().map(Err).or(v)
    })
}
