//! JSON support.
use crate::{Map, Num, Tag, Val};
use alloc::{string::ToString, vec::Vec};
use hifijson::token::{Expect, Lex};
use hifijson::{IterLexer, LexAlloc, SliceLexer};
use std::io;

pub use crate::write::write;

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

/// Parse a sequence of JSON values.
pub fn parse_many(slice: &[u8]) -> impl Iterator<Item = Result<Val, hifijson::Error>> + '_ {
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || Some(parse(ws_tk(&mut lexer)?, &mut lexer)))
}

/// Read a sequence of JSON values.
pub fn read_many<'a>(read: impl io::BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    use crate::invalid_data;
    let mut lexer = IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = ws_tk(&mut lexer).map(|next| parse(next, &mut lexer).map_err(invalid_data));
        // always return I/O error if present, regardless of the output value!
        lexer.error.take().map(Err).or(v)
    })
}

/// Parse exactly one JSON value.
pub fn parse_single(s: &[u8]) -> Result<Val, hifijson::Error> {
    SliceLexer::new(s).exactly_one(ws_tk, parse)
}

/// Parse a JSON string as byte string, preserving invalid UTF-8 as-is.
fn parse_string<L: LexAlloc>(lexer: &mut L, tag: Tag) -> Result<Vec<u8>, hifijson::Error> {
    let on_string = |bytes: &mut L::Bytes, out: &mut Vec<u8>| {
        out.extend(bytes.iter());
        Ok(())
    };
    let s = lexer.str_fold(Vec::new(), on_string, |lexer, out| {
        use hifijson::escape::Error;
        match (tag, lexer.take_next().ok_or(Error::Eof)?) {
            (Tag::Bytes, b'u') => Err(Error::InvalidKind(b'u'))?,
            (Tag::Bytes, b'x') => out.push(lexer.hex()?),
            (_, c) => out.extend(lexer.escape(c)?.encode_utf8(&mut [0; 4]).as_bytes()),
        }
        Ok(())
    });
    s.map_err(hifijson::Error::Str)
}

fn parse_num<L: LexAlloc>(lexer: &mut L, prefix: &str) -> Result<Num, hifijson::Error> {
    let (num, parts) = lexer.num_string(prefix)?;
    // if we are dealing with an integer ...
    Ok(if parts.dot.is_none() && parts.exp.is_none() {
        Num::try_from_int_str(&num, 10).unwrap()
    } else {
        Num::Dec(num.to_string().into())
    })
}

fn parse_signed<L: LexAlloc>(sign: u8, lexer: &mut L) -> Result<Num, hifijson::Error> {
    Ok(match (sign, lexer.discarded().peek_next()) {
        (b'+', Some(b'I')) if lexer.strip_prefix(b"Infinity") => Num::Float(f64::INFINITY),
        (b'-', Some(b'I')) if lexer.strip_prefix(b"Infinity") => Num::Float(f64::NEG_INFINITY),
        (b'+', _) => parse_num(lexer, "")?,
        (b'-', _) => parse_num(lexer, "-")?,
        _ => Err(Expect::Value)?,
    })
}

/// Parse a JSON value, given an initial non-whitespace character and a lexer.
///
/// If the underlying lexer reads input fallibly (for example `IterLexer`),
/// the error returned by this function might be misleading.
/// In that case, always check whether the lexer contains an error.
fn parse<L: LexAlloc>(next: u8, lexer: &mut L) -> Result<Val, hifijson::Error> {
    Ok(match next {
        b'n' if lexer.strip_prefix(b"null") => Val::Null,
        b't' if lexer.strip_prefix(b"true") => Val::Bool(true),
        b'f' if lexer.strip_prefix(b"false") => Val::Bool(false),
        b'b' if lexer.strip_prefix(b"b\"") => Val::byte_str(parse_string(lexer, Tag::Bytes)?),
        b'N' if lexer.strip_prefix(b"NaN") => Val::Num(Num::Float(f64::NAN)),
        b'I' if lexer.strip_prefix(b"Infinity") => Val::Num(Num::Float(f64::INFINITY)),
        sign @ (b'+' | b'-') => Val::Num(parse_signed(sign, lexer)?),
        b'0'..=b'9' => Val::Num(parse_num(lexer, "")?),
        b'"' => Val::utf8_str(parse_string(lexer.discarded(), Tag::Utf8)?),
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
