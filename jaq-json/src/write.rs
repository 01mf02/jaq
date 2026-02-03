//! Functions and macros for writing (parts of) values.
//!
//! We use macros so that we can create both
//! formatters ([core::fmt::Formatter]) and
//! writers ([std::io::Write]) from the same code.

use crate::Val;
use alloc::string::String;
use core::fmt::{self, Formatter};
#[cfg(feature = "std")]
use std::io;

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
                    $crate::write_byte!($w, *last, write!($w, "\\u{last:04x}"))?
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
    ($w:ident, $s:ident) => {{
        write!($w, "b\"")?;
        $s.iter()
            .try_for_each(|c| $crate::write_byte!($w, *c, write!($w, "\\x{c:02x}")))?;
        write!($w, "\"")
    }};
}

/// Write a comma-separated sequence of values `$xs` with `$f`.
#[macro_export]
macro_rules! write_seq {
    ($w:ident, $pp:expr, $level:expr, $xs:expr, $f:expr) => {{
        let indent = &$pp.indent;
        if indent.is_some() {
            writeln!($w)?;
        }
        let mut iter = $xs.into_iter().peekable();
        while let Some(x) = iter.next() {
            if let Some(indent) = indent {
                write!($w, "{}", indent.repeat($level + 1))?;
            }
            $f(x)?;
            if iter.peek().is_some() {
                write!($w, ",")?;
                if $pp.sep_space && indent.is_none() {
                    write!($w, " ")?
                }
            }
            if indent.is_some() {
                writeln!($w)?
            }
        }
        if let Some(indent) = indent {
            write!($w, "{}", indent.repeat($level))
        } else {
            Ok(())
        }
    }};
}

/// Styles used to pretty-print values.
#[derive(Clone, Default)]
pub struct Styles<S = String> {
    /// null
    pub null: S,
    /// false
    pub r#false: S,
    /// true
    pub r#true: S,
    /// numbers
    pub num: S,
    /// strings
    pub str: S,
    /// arrays
    pub arr: S,
    /// objects
    pub obj: S,
    /// object keys
    pub key: S,

    /// byte strings
    pub bstr: S,

    /// reset pretty printer
    pub reset: S,
}

impl Styles {
    /// Default ANSI styles
    pub fn ansi() -> Self {
        let mut cols = Styles::default().parse("90:39:39:39:32:1;39:1;39:1;34");
        cols.bstr = "\x1b[31m".into();
        cols.reset = "\x1b[0m".into();
        cols
    }

    /// Overwrite styles with those present in `JQ_COLORS` environment variable.
    pub fn parse(mut self, s: &str) -> Self {
        let fields = [
            &mut self.null,
            &mut self.r#false,
            &mut self.r#true,
            &mut self.num,
            &mut self.str,
            &mut self.arr,
            &mut self.obj,
            &mut self.key,
        ];
        for (style, field) in s.split(':').zip(fields) {
            *field = if style.is_empty() {
                "".into()
            } else {
                alloc::format!("\x1b[{style}m")
            }
        }
        self
    }
}

/// Pretty printer.
#[derive(Clone, Default)]
pub struct Pp<S = String> {
    /// indent by repeating given string `n` times
    pub indent: Option<S>,
    /// sort objects by keys
    pub sort_keys: bool,
    /// styles for different types of values
    pub styles: Styles<S>,
    /// put a space after ':'
    ///
    /// This is necessary for YAML, which interprets
    /// {1:2}  as {"1:2": null}, whereas it interprets
    /// {1: 2} as {1: 2}.
    pub sep_space: bool,
}

/// Apply a style `$style` to the output of `$f`.
#[macro_export]
macro_rules! style {
    ($w:ident, $pp:ident, $style:ident, $f:expr) => {{
        let style = &$pp.styles.$style;
        let reset = if style.is_empty() {
            ""
        } else {
            &*$pp.styles.reset
        };
        write!($w, "{style}")?;
        $f?;
        write!($w, "{reset}")
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
macro_rules! format_val {
    ($w:ident, $pp:ident, $level:expr, $v:ident, $f:expr) => {{
        macro_rules! color {
            ($style:ident, $g:expr) => {{
                $crate::style!($w, $pp, $style, $g)
            }};
        }
        match $v {
            Val::Null => color!(null, write!($w, "null")),
            Val::Bool(true) => color!(r#true, write!($w, "true")),
            Val::Bool(false) => color!(r#false, write!($w, "false")),
            Val::Num(n) => color!(num, write!($w, "{n}")),
            Val::Str(b, $crate::Tag::Bytes) => color!(bstr, $crate::write_bytes!($w, b)),
            Val::Str(s, $crate::Tag::Utf8) => color!(
                str,
                $crate::write_utf8!($w, s, |part| write!($w, "{}", $crate::bstr(part)))
            ),
            Val::Arr(a) => {
                color!(arr, write!($w, "["))?;
                if !a.is_empty() {
                    $crate::write_seq!($w, $pp, $level, &**a, |x| $f($w, $pp, $level + 1, x))?;
                }
                color!(arr, write!($w, "]"))
            }
            Val::Obj(o) => {
                color!(obj, write!($w, "{{"))?;
                macro_rules! kv {
                    ($kv:expr) => {{
                        let (k, v) = $kv;
                        if $pp.styles.key.is_empty() {
                            $f($w, $pp, $level + 1, k)?
                        } else {
                            let unstyled = $crate::write::Pp {
                                styles: Default::default(),
                                ..$pp.clone()
                            };
                            color!(key, $f($w, &unstyled, $level + 1, k))?;
                        }
                        color!(obj, write!($w, ":"))?;
                        if $pp.sep_space {
                            write!($w, " ")?;
                        }
                        $f($w, $pp, $level + 1, v)
                    }};
                }
                if !o.is_empty() {
                    if $pp.sort_keys {
                        let mut o: alloc::vec::Vec<_> = o.iter().collect();
                        o.sort_by_key(|(k, _v)| *k);
                        $crate::write_seq!($w, $pp, $level, o, |x| kv!(x))
                    } else {
                        $crate::write_seq!($w, $pp, $level, &**o, |x| kv!(x))
                    }?
                }
                color!(obj, write!($w, "}}"))
            }
        }
    }};
}

/// Write a value as JSON, using a custom function for child values.
#[macro_export]
macro_rules! write_val {
    ($w:ident, $pp:ident, $level:expr, $v:ident, $f:expr) => {{
        use $crate::{Tag::Utf8, Val::Str};
        match $v {
            Str(s, Utf8) => style!($w, $pp, str, write_utf8!($w, s, |part| $w.write_all(part))),
            _ => format_val!($w, $pp, $level, $v, $f),
        }
    }};
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
#[cfg(feature = "std")]
pub fn write(w: &mut dyn io::Write, pp: &Pp, level: usize, v: &Val) -> io::Result<()> {
    write_val!(w, pp, level, v, write)
}

pub(crate) struct Buf(pub(crate) alloc::vec::Vec<u8>);

impl Buf {
    fn write_all(&mut self, bytes: &[u8]) -> fmt::Result {
        self.0.extend_from_slice(bytes);
        Ok(())
    }
}

impl fmt::Write for Buf {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.write_all(s.as_bytes())
    }
}

pub(crate) fn write_buf(w: &mut Buf, pp: &Pp, level: usize, v: &Val) -> fmt::Result {
    use core::fmt::Write;
    write_val!(w, pp, level, v, write_buf)
}

pub(crate) fn format(w: &mut Formatter, pp: &Pp, level: usize, v: &Val) -> fmt::Result {
    format_val!(w, pp, level, v, format)
}
