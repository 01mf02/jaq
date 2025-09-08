//! JSON support.
use crate::{Map, Num, Tag, Val};
use alloc::{string::ToString, vec::Vec};
use core::fmt::{self, Formatter};
use hifijson::token::{Expect, Lex, Token};
use hifijson::{str, IterLexer, LexAlloc, SliceLexer};
#[cfg(feature = "std")]
use std::io;

/// Parse a sequence of JSON values.
pub fn parse_many(slice: &[u8]) -> impl Iterator<Item = Result<Val, hifijson::Error>> + '_ {
    let mut lexer = SliceLexer::new(slice);
    core::iter::from_fn(move || Some(parse(lexer.ws_token()?, &mut lexer)))
}

#[cfg(feature = "std")]
/// Read a sequence of JSON values.
pub fn read_many<'a>(read: impl io::BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    use crate::invalid_data;
    let mut lexer = IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        let v = parse(lexer.ws_token()?, &mut lexer);
        Some(v.map_err(|e| core::mem::take(&mut lexer.error).unwrap_or_else(|| invalid_data(e))))
    })
}

/// Parse exactly one JSON value.
pub fn parse_single(s: &[u8]) -> Result<Val, hifijson::Error> {
    SliceLexer::new(s).exactly_one(parse)
}

/// Parse a JSON string as byte string, preserving invalid UTF-8 as-is.
fn parse_string<L: LexAlloc>(lexer: &mut L) -> Result<Vec<u8>, hifijson::Error> {
    let on_string = |bytes: &mut L::Bytes, out: &mut Vec<u8>| {
        out.extend(bytes.iter());
        Ok(())
    };
    lexer.str_fold(Vec::new(), on_string, |lexer, escape, out| {
        let c = lexer.escape_char(escape).map_err(str::Error::Escape)?;
        out.extend(c.encode_utf8(&mut [0; 4]).as_bytes());
        Ok(())
    })
}

/// Parse at least one JSON value, given an initial token and a lexer.
///
/// If the underlying lexer reads input fallibly (for example `IterLexer`),
/// the error returned by this function might be misleading.
/// In that case, always check whether the lexer contains an error.
fn parse(token: Token, lexer: &mut impl LexAlloc) -> Result<Val, hifijson::Error> {
    match token {
        Token::Null => Ok(Val::Null),
        Token::True => Ok(Val::Bool(true)),
        Token::False => Ok(Val::Bool(false)),
        Token::DigitOrMinus => Ok(Val::Num({
            let (num, parts) = lexer.num_string()?;
            // if we are dealing with an integer ...
            if parts.dot.is_none() && parts.exp.is_none() {
                Num::try_from_int_str(&num, 10).unwrap()
            } else {
                Num::Dec(num.to_string().into())
            }
        })),
        Token::Quote => Ok(Val::utf8_str(parse_string(lexer)?)),
        Token::LSquare => Ok(Val::Arr({
            let mut arr = Vec::new();
            lexer.seq(Token::RSquare, |token, lexer| {
                arr.push(parse(token, lexer)?);
                Ok::<_, hifijson::Error>(())
            })?;
            arr.into()
        })),
        Token::LCurly => Ok(Val::obj({
            let mut obj = Map::default();
            lexer.seq(Token::RCurly, |token, lexer| {
                let is_colon = |t: &Token| *t == Token::Colon;
                let key = parse(token, lexer)?;
                lexer.ws_token().filter(is_colon).ok_or(Expect::Colon)?;
                let value = parse(lexer.ws_token().ok_or(Expect::Value)?, lexer)?;
                obj.insert(key, value);
                Ok::<_, hifijson::Error>(())
            })?;
            obj
        })),
        _ => Err(Expect::Value)?,
    }
}

type WriteFn<T> = fn(&mut dyn std::io::Write, &T) -> std::io::Result<()>;
type FormatFn<T> = fn(&mut Formatter, &T) -> fmt::Result;

pub(crate) fn write_with(w: &mut dyn io::Write, v: &Val, f: WriteFn<Val>) -> io::Result<()> {
    match v {
        Val::Str(s, Tag::Raw) => w.write_all(s),
        Val::Str(b, Tag::Bytes) => write_bytes!(w, b),
        Val::Str(s, Tag::Utf8) => write_utf8!(w, s, |part| w.write_all(part)),
        _ => write_val!(w, v, |v: &Val| f(w, v)),
    }
}

/// Write a value as JSON.
///
/// Note that unlike jq, this may actually produce invalid JSON.
/// In particular, this may yield:
///
/// - literals for special floating-point values (NaN, Infinity, -Infinity)
/// - invalid UTF-8 characters
/// - byte strings with `\xXX` sequences
/// - objects with non-string keys
///
/// The key principles behind this behaviour are:
///
/// 1. Printing a value should always succeed.
///    (Otherwise, there would exist values that we could not even inspect.)
/// 2. Printing a value should yield valid JSON if and only if
///    the value can be represented by an equivalent JSON value.
///    (To give users a chance to find non-JSON values and to take appropriate action.)
///
/// jq and jaq agree on principle 1, but disagree on principle 2.
/// In particular, this shows by the fact that `jq -n 'nan'` yields `null`.
/// That means that jq maps values that cannot be represented by JSON
/// to different values that can be represented by JSON.
///
/// In summary,
/// jq may cause silent information loss, whereas
/// jaq may yield invalid JSON values.
/// Choose your poison.
pub fn write(w: &mut dyn io::Write, v: &Val) -> io::Result<()> {
    write_with(w, v, |w, v| write(w, v))
}

/// Format a value as compact JSON, using a custom function to format child values.
///
/// This is useful to override how certain values are printed, e.g. for YAML.
pub(crate) fn format_with(w: &mut Formatter, v: &Val, f: FormatFn<Val>) -> fmt::Result {
    write_val!(w, v, |v: &Val| f(w, v))
}
