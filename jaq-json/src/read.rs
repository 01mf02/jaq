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

/// Like [`seq_ws`], but allows a trailing comma before the closing delimiter.
fn seq_ws_trailing<L: Lex, E: From<Expect>, PF, F>(
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
            next = match pf(lexer)? {
                Some(n) if n == end => {
                    lexer.take_next();
                    return Ok(());
                }
                Some(n) => n,
                None => return Err(Expect::Value.into()),
            };
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
// JSON5: supports all JSONC features plus trailing commas, single-quoted
// strings, unquoted keys, hex numbers, and leading/trailing decimal points
// ---------------------------------------------------------------------------

/// Parse a single-quoted string (JSON5 extension).
///
/// Handles the same escape sequences as double-quoted strings.
/// Terminates on unescaped `'`. Allows unescaped `"` inside.
fn parse_string_sq<L: LexAlloc>(lexer: &mut L) -> Result<Vec<u8>, Cause> {
    use hifijson::escape;
    let mut out = Vec::new();
    loop {
        match lexer.take_next() {
            None => return Err(Cause::UnterminatedString),
            Some(b'\'') => return Ok(out),
            Some(b'\\') => {
                let c = lexer.take_next().ok_or(Cause::UnterminatedString)?;
                match c {
                    b'\'' => out.push(b'\''),
                    b'"' => out.push(b'"'),
                    _ => {
                        let ch = escape::Lex::escape(lexer, c)
                            .map_err(|e| hifijson::Error::Str(hifijson::str::Error::Escape(e)))?;
                        out.extend(ch.encode_utf8(&mut [0; 4]).as_bytes());
                    }
                }
            }
            Some(c) => out.push(c),
        }
    }
}

/// Parse an unquoted identifier (JSON5 object keys).
///
/// Accepts ASCII identifiers: `[a-zA-Z_$][a-zA-Z0-9_$]*`.
fn parse_identifier<L: hifijson::Lex>(first: u8, lexer: &mut L) -> Result<Vec<u8>, Cause> {
    if !matches!(first, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$') {
        return Err(Cause::InvalidIdentifier);
    }
    lexer.take_next(); // consume the peeked first byte
    let mut ident = Vec::new();
    ident.push(first);
    while let Some(c) = lexer.peek_next() {
        if matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'$') {
            lexer.take_next();
            ident.push(c);
        } else {
            break;
        }
    }
    Ok(ident)
}

/// Parse a hex number literal after the `0x` / `0X` prefix has been consumed.
fn parse_hex<L: hifijson::Lex>(lexer: &mut L) -> Result<Num, Cause> {
    let mut digits = Vec::new();
    while let Some(c) = lexer.peek_next() {
        if c.is_ascii_hexdigit() {
            lexer.take_next();
            digits.push(c);
        } else {
            break;
        }
    }
    if digits.is_empty() {
        return Err(Cause::InvalidHexNumber);
    }
    let s = core::str::from_utf8(&digits).unwrap();
    Num::from_str_radix(s, 16).ok_or(Cause::InvalidHexNumber)
}

/// Parse a leading-decimal number (e.g. `.5`, `.5e2`).
///
/// The leading `.` has already been consumed by the caller.
fn parse_leading_decimal<L: LexAlloc>(lexer: &mut L) -> Result<Num, Cause> {
    let mut s = alloc::string::String::from("0.");
    let mut has_digit = false;
    while let Some(c) = lexer.peek_next() {
        if c.is_ascii_digit() {
            lexer.take_next();
            s.push(c as char);
            has_digit = true;
        } else {
            break;
        }
    }
    if !has_digit {
        return Err(Cause::Hifi(hifijson::Error::Num(
            hifijson::num::Error::ExpectedDigit,
        )));
    }
    // Handle optional exponent: e/E followed by optional +/- and digits
    if let Some(c) = lexer.peek_next() {
        if c == b'e' || c == b'E' {
            lexer.take_next();
            s.push(c as char);
            if let Some(sign) = lexer.peek_next() {
                if sign == b'+' || sign == b'-' {
                    lexer.take_next();
                    s.push(sign as char);
                }
            }
            let mut exp_digits = false;
            while let Some(d) = lexer.peek_next() {
                if d.is_ascii_digit() {
                    lexer.take_next();
                    s.push(d as char);
                    exp_digits = true;
                } else {
                    break;
                }
            }
            if !exp_digits {
                return Err(Cause::Hifi(hifijson::Error::Num(
                    hifijson::num::Error::ExpectedDigit,
                )));
            }
        }
    }
    Ok(Num::Dec(s.into()))
}

/// Parse a number in JSON5 mode.
///
/// Extends the standard parser with hex literal support (`0x` / `0X`).
fn parse_num_json5<L: LexAlloc>(lexer: &mut L) -> Result<Num, Cause> {
    // Use hifijson's num_string_with to get the raw number string
    let num = hifijson::num::Num::signed_digits();
    let (num, parts) = lexer.num_string_with(num).unvalidated();
    let num = num.as_ref();
    Ok(match num {
        "+" if lexer.strip_prefix(b"Infinity") => Num::Float(f64::INFINITY),
        "-" if lexer.strip_prefix(b"Infinity") => Num::Float(f64::NEG_INFINITY),
        // Detect hex prefix: hifijson will have consumed "0" then stopped at "x"
        "0" | "+0" | "-0" if lexer.strip_prefix(b"x") || lexer.strip_prefix(b"X") => {
            let hex = parse_hex(lexer)?;
            if num == "-0" {
                // Negate the parsed hex value
                match hex {
                    Num::Int(n) => Num::Int(-n),
                    other => other,
                }
            } else {
                hex
            }
        }
        // Trailing decimal: hifijson returns e.g. "5." — accept it
        _ if num.ends_with('.') && num.len() > 1 => {
            let trimmed = &num[..num.len() - 1];
            if trimmed.bytes().all(|c| c.is_ascii_digit() || c == b'+' || c == b'-') {
                Num::Dec(num.to_string().into())
            } else {
                Err(Cause::Hifi(hifijson::Error::Num(hifijson::num::Error::ExpectedDigit)))?
            }
        }
        _ if num.ends_with(|c: char| c.is_ascii_digit()) => {
            if parts.is_int() {
                Num::from_str_radix(num, 10).unwrap()
            } else {
                Num::Dec(num.to_string().into())
            }
        }
        _ => Err(Cause::Hifi(hifijson::Error::Num(hifijson::num::Error::ExpectedDigit)))?,
    })
}

/// Parse a JSON5 value.
fn parse_json5<L: LexAlloc>(next: u8, lexer: &mut L) -> Result<Val, Cause> {
    Ok(match next {
        b'n' if lexer.strip_prefix(b"null") => Val::Null,
        b't' if lexer.strip_prefix(b"true") => Val::Bool(true),
        b'f' if lexer.strip_prefix(b"false") => Val::Bool(false),
        b'b' if lexer.strip_prefix(b"b\"") => Val::byte_str(parse_string(lexer, true)?),
        b'N' if lexer.strip_prefix(b"NaN") => Val::Num(Num::Float(f64::NAN)),
        b'I' if lexer.strip_prefix(b"Infinity") => Val::Num(Num::Float(f64::INFINITY)),
        b'0'..=b'9' | b'+' | b'-' => Val::Num(parse_num_json5(lexer)?),
        b'.' => {
            // Leading decimal: .5, .5e2
            lexer.take_next(); // consume the peeked '.'
            Val::Num(parse_leading_decimal(lexer)?)
        }
        b'"' => Val::utf8_str(parse_string(lexer.discarded(), false)?),
        b'\'' => {
            lexer.take_next(); // consume the peeked '\''
            Val::utf8_str(parse_string_sq(lexer)?)
        }
        b'[' => Val::Arr({
            let mut arr = Vec::new();
            seq_ws_trailing(lexer.discarded(), b']', ws_tk_jsonc, |next, lexer| {
                arr.push(parse_json5(next, lexer)?);
                Ok::<_, Cause>(())
            })?;
            arr.into()
        }),
        b'{' => Val::obj({
            let mut obj = Map::default();
            seq_ws_trailing(lexer.discarded(), b'}', ws_tk_jsonc, |next, lexer| {
                // Key: double-quoted string, single-quoted string, or unquoted identifier
                let key = match next {
                    b'"' => Val::utf8_str(parse_string(lexer.discarded(), false)?),
                    b'\'' => {
                        lexer.take_next(); // consume the peeked '\''
                        Val::utf8_str(parse_string_sq(lexer)?)
                    }
                    c => Val::utf8_str(parse_identifier(c, lexer)?),
                };
                expect_ws(lexer, ws_tk_jsonc, b':')?.ok_or(Expect::Colon)?;
                let value = parse_json5(ws_tk_jsonc(lexer)?.ok_or(Expect::Value)?, lexer)?;
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
    /// An unterminated single-quoted string.
    UnterminatedString,
    /// A malformed unquoted identifier key.
    InvalidIdentifier,
    /// A malformed hex number literal (e.g. `0x` with no digits).
    InvalidHexNumber,
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
            Self::UnterminatedString => f.write_str("unterminated single-quoted string"),
            Self::InvalidIdentifier => f.write_str("invalid unquoted identifier"),
            Self::InvalidHexNumber => f.write_str("invalid hex number literal"),
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

// ---------------------------------------------------------------------------
// Public API: JSON5
// ---------------------------------------------------------------------------

/// Parse exactly one JSON5 value.
pub fn parse_single_json5(slice: &[u8]) -> Result<Val, Error> {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    exactly_one_ws(&mut lexer, ws_tk_jsonc, parse_json5)
        .map_err(|e| Error(offset(lexer.as_slice()), e))
}

/// Parse a sequence of JSON5 values.
pub fn parse_many_json5(slice: &[u8]) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || {
        let next = match ws_tk_jsonc(&mut lexer) {
            Ok(Some(next)) => next,
            Ok(None) => return None,
            Err(e) => return Some(Err(Error(offset(lexer.as_slice()), e))),
        };
        Some(parse_json5(next, &mut lexer).map_err(|e| Error(offset(lexer.as_slice()), e)))
    })
}

#[cfg(feature = "std")]
/// Read a sequence of JSON5 values.
pub fn read_many_json5<'a>(
    read: impl io::BufRead + 'a,
) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let invalid_data = |e: Cause| io::Error::new(io::ErrorKind::InvalidData, e);
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = match ws_tk_jsonc(&mut lexer) {
            Ok(Some(next)) => Some(parse_json5(next, &mut lexer).map_err(invalid_data)),
            Ok(None) => None,
            Err(e) => Some(Err(invalid_data(e))),
        };
        // always return I/O error if present, regardless of the output value!
        lexer.error.take().map(Err).or(v)
    })
}
