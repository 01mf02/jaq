//! JSON support.
use crate::{Map, Num, Val};
use alloc::{string::ToString, vec::Vec};
use core::fmt::{self, Formatter};
use hifijson::token::{Expect, Lex};
use hifijson::{LexAlloc, SliceLexer};
#[cfg(feature = "std")]
use std::io;

/// Eat whitespace/comments, then peek at next character.
fn ws_tk<L: Lex>(lexer: &mut L) -> Option<u8> {
    loop {
        lexer.eat_whitespace();
        match lexer.peek_next() {
            Some(b'#') => lexer.skip_until(|c| c == b'\n'),
            next => return next,
        }
    }
}

/// Parse error.
#[derive(Debug)]
pub struct Error(usize, hifijson::Error);

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "byte offset {}: {}", self.0, self.1)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

/// Parse exactly one JSON value.
pub fn parse_single(slice: &[u8]) -> Result<Val, Error> {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    lexer
        .exactly_one(ws_tk, parse)
        .map_err(|e| Error(offset(lexer.as_slice()), e))
}

/// Parse a sequence of JSON values.
pub fn parse_many(slice: &[u8]) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || {
        Some(parse(ws_tk(&mut lexer)?, &mut lexer).map_err(|e| Error(offset(lexer.as_slice()), e)))
    })
}

#[cfg(feature = "std")]
/// Read a sequence of JSON values.
pub fn read_many<'a>(read: impl io::BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let invalid_data = |e| io::Error::new(io::ErrorKind::InvalidData, e);
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = ws_tk(&mut lexer).map(|next| parse(next, &mut lexer).map_err(invalid_data));
        // always return I/O error if present, regardless of the output value!
        lexer.error.take().map(Err).or(v)
    })
}

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

/// Parse a JSON value, given an initial non-whitespace character and a lexer.
///
/// If the underlying lexer reads input fallibly (for example [`hifijson::IterLexer`]),
/// the error returned by this function might be misleading.
/// In that case, always check whether the lexer contains an error.
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
