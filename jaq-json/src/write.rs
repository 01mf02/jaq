//! Functions and macros for writing (parts of) values.
//!
//! We use macros so that we can create both
//! formatters ([core::fmt::Formatter]) and
//! writers ([std::io::Write]) from the same code.

use crate::{Tag, Val};
use alloc::string::String;
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

macro_rules! write_seq {
    ($w:ident, $indent:expr, $level:expr, $xs:expr, $f:expr) => {{
        if $indent.is_some() {
            writeln!($w)?;
        }
        let mut iter = $xs.into_iter().peekable();
        while let Some(x) = iter.next() {
            if let Some(indent) = $indent {
                write!($w, "{}", indent.repeat($level + 1))?;
            }
            $f(x)?;
            if iter.peek().is_some() {
                write!($w, ",")?;
            }
            if $indent.is_some() {
                writeln!($w)?;
            }
        }
        if let Some(indent) = $indent {
            write!($w, "{}", indent.repeat($level))
        } else {
            Ok(())
        }
    }};
}

/// Colors used to pretty-print values.
#[derive(Clone, Default)]
pub struct Colors<S = String> {
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

    /// byte strings
    pub bstr: S,

    /// reset pretty printer
    pub reset: S,
}

impl Colors {
    /// Default ANSI colors
    pub fn ansi() -> Self {
        let mut cols = Colors::default().parse("90:39:39:39:32:1;39:1;39");
        cols.bstr = "\x1b[31m".into();
        cols.reset = "\x1b[0m".into();
        cols
    }

    /// Overwrite colors with those present in `JQ_COLORS` environment variable.
    pub fn parse(mut self, s: &str) -> Self {
        let fields = [
            &mut self.null,
            &mut self.r#false,
            &mut self.r#true,
            &mut self.num,
            &mut self.str,
            &mut self.arr,
            &mut self.obj,
        ];
        for (color, field) in s.split(':').zip(fields) {
            use alloc::format;
            *field = color.split(';').map(|s| format!("\x1b[{s}m")).collect();
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
    /// colors for different types of values
    pub colors: Colors<S>,
}

impl Pp {
    /// Write a string with color.
    pub fn write_str(
        &self,
        w: &mut dyn Write,
        f: impl FnOnce(&mut dyn Write) -> io::Result<()>,
    ) -> io::Result<()> {
        write!(w, "{}", self.colors.str)?;
        f(w)?;
        write!(w, "{}", self.colors.reset)
    }
}

/// Write a value as JSON superset, using a function `$f` to write sub-values.
///
/// This macro writes strings by replacing invalid UTF-8 characters with the
/// Unicode replacement character.
/// That way, this macro can be used not only for writers, but also for
/// formatters, which require all output to be valid UTF-8.
/// However, the JSON/YAML writers usually override this behaviour,
/// yielding invalid UTF-8 characters as-is.
macro_rules! write_val {
    ($w:ident, $pp:ident, $level:expr, $v:ident, $f:expr) => {{
        macro_rules! color {
            ($style:ident, $g:expr) => {{
                write!($w, "{}", $pp.colors.$style)?;
                $g?;
                write!($w, "{}", $pp.colors.reset)
            }};
        }
        let indent = &$pp.indent;
        match $v {
            Val::Null => color!(null, write!($w, "null")),
            Val::Bool(true) => color!(r#true, write!($w, "true")),
            Val::Bool(false) => color!(r#false, write!($w, "false")),
            Val::Num(n) => color!(num, write!($w, "{n}")),
            Val::Str(b, $crate::Tag::Bytes) => color!(bstr, $crate::write_bytes!($w, b)),
            Val::Str(s, $crate::Tag::Utf8) => color!(
                str,
                write_utf8!($w, s, |part| write!($w, "{}", $crate::bstr(part)))
            ),
            Val::Arr(a) => {
                color!(arr, write!($w, "["))?;
                if !a.is_empty() {
                    write_seq!($w, indent, $level, &**a, |x| $f($level + 1, x))?;
                }
                color!(arr, write!($w, "]"))
            }
            Val::Obj(o) => {
                color!(obj, write!($w, "{{"))?;
                macro_rules! kv {
                    ($kv:expr) => {{
                        let (k, v) = $kv;
                        use jaq_std::ValT;
                        $f($level + 1, k)?;
                        color!(obj, write!($w, ":"))?;
                        // YAML interprets {1:2}  as {"1:2": null}, whereas
                        // it   interprets {1: 2} as {1: 2}
                        // in order to keep compatibility with jq,
                        // we add a space between ':' and the value
                        // only if the key is a UTF-8 string
                        if indent.is_some() || !k.is_utf8_str() {
                            write!($w, " ")?;
                        }
                        $f($level + 1, v)
                    }};
                }
                if !o.is_empty() {
                    if $pp.sort_keys {
                        let mut o: alloc::vec::Vec<_> = o.iter().collect();
                        o.sort_by_key(|(k, _v)| *k);
                        write_seq!($w, indent, $level, o, |x| kv!(x))
                    } else {
                        write_seq!($w, indent, $level, &**o, |x| kv!(x))
                    }?
                }
                color!(obj, write!($w, "}}"))
            }
        }
    }};
}

type WriteFn = fn(&mut dyn Write, &Pp, usize, &Val) -> io::Result<()>;
type FormatFn = fn(&mut Formatter, &Pp, usize, &Val) -> fmt::Result;

/// Write a value as JSON, using a custom function for child values.
pub fn write_with(w: &mut dyn Write, pp: &Pp, level: usize, v: &Val, f: WriteFn) -> io::Result<()> {
    match v {
        Val::Str(s, Tag::Utf8) => pp.write_str(w, |w| write_utf8!(w, s, |part| w.write_all(part))),
        _ => write_val!(w, pp, level, v, |level, x| f(w, pp, level, x)),
    }
}

/// Format a value as JSON, using a custom function for child values.
///
/// This is useful to override how certain values are printed, e.g. for YAML.
pub fn format_with(w: &mut Formatter, pp: &Pp, level: usize, v: &Val, f: FormatFn) -> fmt::Result {
    write_val!(w, pp, level, v, |level, x| f(w, pp, level, x))
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
pub fn write(w: &mut dyn io::Write, pp: &Pp, level: usize, v: &Val) -> io::Result<()> {
    write_with(w, pp, level, v, write)
}

pub(crate) fn format(w: &mut Formatter, pp: &Pp, level: usize, v: &Val) -> fmt::Result {
    format_with(w, pp, level, v, format)
}
