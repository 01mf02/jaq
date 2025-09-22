//! Functions and macros for writing (parts of) values.
//!
//! We use macros so that we can create both
//! formatters ([core::fmt::Formatter]) and
//! writers ([std::io::Write]) from the same code.

use crate::{Tag, Val};
use core::fmt::{self, Formatter};
use std::io::{self, Write};

/// Write a byte.
///
/// This uses `$f` to write bytes not corresponding to normal ASCII characters.
///
/// This is especially useful to pretty-print control characters, such as
/// `'\n'` (U+000A), but also all other control characters.
#[macro_export]
macro_rules! write_byte {
    ($w:ident, $c:expr, $f:expr) => {{
        match $c {
            // Rust does not recognise the following two character escapes
            0x08 => write!($w, "\\b"),
            0x0c => write!($w, "\\f"),
            c @ (b'\t' | b'\n' | b'\r' | b'\\' | b'"') => {
                write!($w, "{}", char::from(c).escape_default())
            }
            0x00..=0x1F | 0x7F..=0xFF => $f,
            c => write!($w, "{}", char::from(c)),
        }
    }};
}

/// Write a UTF-8 string as JSON string, including leading and trailing quotes.
///
/// This uses `$f` to format byte slices that do not need to be escaped.
#[macro_export]
macro_rules! write_utf8 {
    ($w:ident, $s:ident, $f:expr) => {{
        write!($w, "\"")?;
        let is_special = |c| matches!(c, 0x00..=0x1F | b'\\' | b'"' | 0x7F);
        for s in $s.split_inclusive(|c| is_special(*c)) {
            match s.split_last() {
                Some((last, init)) if is_special(*last) => {
                    $f(init)?;
                    write_byte!($w, *last, write!($w, "\\u{last:04x}"))?
                }
                _ => $f(s)?,
            }
        }
        write!($w, "\"")
    }};
}

/// Write a byte string, including leading and trailing quotes.
///
/// This maps all non-ASCII `u8`s to `\xXX`.
#[macro_export]
macro_rules! write_bytes {
    ($w:ident, $s: ident) => {{
        write!($w, "\"")?;
        $s.iter()
            .try_for_each(|c| write_byte!($w, *c, write!($w, "\\x{c:02x}")))?;
        write!($w, "\"")
    }};
}

macro_rules! write_seq {
    ($w:ident, $iter:ident, $f:expr) => {{
        if let Some(x) = $iter.next() {
            $f(x)?;
        }
        $iter.try_for_each(|x| {
            write!($w, ",")?;
            $f(x)
        })
    }};
}

/// Write a value as JSON superset, using a function `$f` to write sub-values.
///
/// This macro writes strings by replacing invalid UTF-8 characters with the
/// Unicode replacement character.
/// That way, this macro can be used not only for writers, but also for
/// formatters, which require all output to be valid UTF-8.
/// However, the JSON/YAML writers usually override this behaviour,
/// yielding invalid UTF-8 characters as-is.
#[macro_export]
macro_rules! write_val {
    ($w:ident, $v:ident, $f:expr) => {{
        use $crate::{bstr, Tag, Val};
        match $v {
            Val::Null => write!($w, "null"),
            Val::Bool(b) => write!($w, "{b}"),
            Val::Num(n) => write!($w, "{n}"),
            Val::Str(b, Tag::Bytes) => write_bytes!($w, b),
            Val::Str(s, Tag::Utf8) => write_utf8!($w, s, |part| write!($w, "{}", bstr(part))),
            Val::Arr(a) => {
                write!($w, "[")?;
                let mut iter = a.iter();
                write_seq!($w, iter, $f)?;
                write!($w, "]")
            }
            Val::Obj(o) => {
                write!($w, "{{")?;
                let mut iter = o.iter();
                write_seq!($w, iter, |(k, v)| {
                    use jaq_std::ValT;
                    $f(k)?;
                    // YAML interprets {1:2}  as {"1:2": null}, whereas
                    // it   interprets {1: 2} as {1: 2}
                    // in order to keep compatibility with jq,
                    // we add a space between ':' and the value
                    // only if the key is a UTF-8 string
                    write!($w, ":{}", if k.is_utf8_str() { "" } else { " " })?;
                    $f(v)
                })?;
                write!($w, "}}")
            }
        }
    }};
}

type WriteFn<T> = fn(&mut dyn Write, &T) -> io::Result<()>;
type FormatFn<T> = fn(&mut Formatter, &T) -> fmt::Result;

pub(crate) fn write_with(w: &mut dyn Write, v: &Val, f: WriteFn<Val>) -> io::Result<()> {
    match v {
        Val::Str(b, Tag::Bytes) => write_bytes!(w, b),
        Val::Str(s, Tag::Utf8) => write_utf8!(w, s, |part| w.write_all(part)),
        _ => write_val!(w, v, |v: &Val| f(w, v)),
    }
}

/// Format a value as compact JSON, using a custom function to format child values.
///
/// This is useful to override how certain values are printed, e.g. for YAML.
pub(crate) fn format_with(w: &mut Formatter, v: &Val, f: FormatFn<Val>) -> fmt::Result {
    write_val!(w, v, |v: &Val| f(w, v))
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
