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

/// Parse error with line and column information.
#[derive(Debug)]
pub struct Error {
    line: usize,
    column: usize,
    inner: hifijson::Error,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "at line {}, column {}: {}",
            self.line, self.column, self.inner
        )
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

/// Convert a byte offset in a slice to a (line, column) pair (both 1-based).
/// Column counts Unicode characters, not bytes.
fn byte_offset_to_position(slice: &[u8], offset: usize) -> (usize, usize) {
    let before = &slice[..offset];
    let line = before.iter().filter(|&&byte| byte == b'\n').count() + 1;
    let last_newline = before.iter().rposition(|&byte| byte == b'\n');
    let line_start = last_newline.map_or(before, |pos| &before[pos + 1..]);
    // TODO: replace with `!byte.is_utf8_continuation()` once stable.
    let column = line_start
        .iter()
        .filter(|&&byte| byte & 0xC0 != 0x80)
        .count()
        + 1;
    (line, column)
}

/// Parse exactly one JSON value.
pub fn parse_single(slice: &[u8]) -> Result<Val, Error> {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    lexer.exactly_one(ws_tk, parse).map_err(|inner| {
        let (line, column) = byte_offset_to_position(slice, offset(lexer.as_slice()));
        Error {
            line,
            column,
            inner,
        }
    })
}

/// Parse a sequence of JSON values.
pub fn parse_many(slice: &[u8]) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let offset = |rest: &[u8]| rest.as_ptr() as usize - slice.as_ptr() as usize;
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || {
        Some(
            parse(ws_tk(&mut lexer)?, &mut lexer).map_err(|inner| {
                let (line, column) = byte_offset_to_position(slice, offset(lexer.as_slice()));
                Error {
                    line,
                    column,
                    inner,
                }
            }),
        )
    })
}

#[cfg(feature = "std")]
/// Wrapper iterator that tracks line and column position as bytes flow through.
struct PositionTracker<I> {
    inner: I,
    position: alloc::rc::Rc<core::cell::Cell<(usize, usize)>>,
}

#[cfg(feature = "std")]
impl<I: Iterator<Item = io::Result<u8>>> Iterator for PositionTracker<I> {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.inner.next()?;
        if let Ok(byte) = &result {
            let (mut line, mut column) = self.position.get();
            if *byte == b'\n' {
                line += 1;
                column = 0;
            } else if *byte & 0xC0 != 0x80 {
                // TODO: replace with `!byte.is_utf8_continuation()` once stable.
                column += 1;
            }
            self.position.set((line, column));
        }
        Some(result)
    }
}

#[cfg(feature = "std")]
/// Read a sequence of JSON values.
pub fn read_many<'a>(read: impl io::BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let invalid_data = |error| io::Error::new(io::ErrorKind::InvalidData, error);
    let position = alloc::rc::Rc::new(core::cell::Cell::new((1usize, 0usize)));
    let tracker = PositionTracker {
        inner: read.bytes(),
        position: alloc::rc::Rc::clone(&position),
    };
    let mut lexer = hifijson::IterLexer::new(tracker);
    core::iter::from_fn(move || {
        let value = ws_tk(&mut lexer).map(|next| {
            parse(next, &mut lexer).map_err(|inner| {
                let (line, column) = position.get();
                invalid_data(Error {
                    line,
                    column,
                    inner,
                })
            })
        });
        // Always return I/O error if present, regardless of the output value.
        lexer.error.take().map(Err).or(value)
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
