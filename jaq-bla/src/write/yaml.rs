//! YAML support.
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use core::fmt::{self, Formatter};
use jaq_json::{write, Num, Val};
use std::io;

macro_rules! write_yaml {
    ($w:ident, $pp:ident, $level:ident, $v:ident, $f:expr, $g:expr) => {{
        macro_rules! color {
            ($style:ident, $h:expr) => {{
                write!($w, "{}", $pp.colors.$style)?;
                $h?;
                write!($w, "{}", $pp.colors.reset)
            }};
        }
        let indent = $pp.indent.as_ref();
        macro_rules! nested {
            ($x:ident, $iter:ident, $pre:expr) => {{
                write!($w, "{}", indent.unwrap().repeat($level))?;
                $pre?;
                if match $x {
                    Val::Arr(a) => !a.is_empty(),
                    Val::Obj(o) => !o.is_empty(),
                    _ => false,
                } {
                    writeln!($w)?
                } else if $pp.sep_space {
                    write!($w, " ")?;
                };
                $f($w, $pp, $level + 1, $x)?;
                if $iter.peek().is_some() {
                    writeln!($w)
                } else {
                    Ok(())
                }
            }};
        }
        match $v {
            Val::Str(b, jaq_json::Tag::Utf8) if !must_quote(b) => {
                color!(str, write!($w, "{}", jaq_json::bstr(b)))
            }
            Val::Str(b, jaq_json::Tag::Bytes) => {
                color!(bstr, write!($w, "!!binary {}", BASE64.encode(b)))
            }
            Val::Arr(a) if !a.is_empty() && indent.is_some() => {
                let mut iter = a.iter().peekable();
                while let Some(v) = iter.next() {
                    nested!(v, iter, color!(arr, write!($w, "-")))?
                }
                Ok(())
            }
            Val::Obj(o) if !o.is_empty() && indent.is_some() => {
                let mut unindented = $pp.clone();
                unindented.indent = None;
                let mut iter = o.iter().peekable();
                while let Some((k, v)) = iter.next() {
                    nested!(v, iter, {
                        $f($w, &unindented, $level, k)?;
                        color!(obj, write!($w, ":"))
                    })?
                }
                Ok(())
            }
            Val::Num(Num::Float(f64::INFINITY)) => color!(num, write!($w, ".inf")),
            Val::Num(Num::Float(f64::NEG_INFINITY)) => color!(num, write!($w, "-.inf")),
            Val::Num(Num::Float(fl)) if fl.is_nan() => color!(num, write!($w, ".nan")),
            _ => $g,
        }
    }};
}

fn must_quote(s: &[u8]) -> bool {
    if s.last().filter(|c| c.is_ascii_whitespace()).is_some() {
        return true;
    }

    let null = ["null", "Null", "NULL"];
    let on = ["on", "On", "ON"];
    let off = ["off", "Off", "OFF"];
    let yes = ["yes", "Yes", "YES"];
    let no = ["no", "No", "NO"];
    let true_ = ["True", "TRUE", "true"];
    let false_ = ["False", "FALSE", "false"];
    let kws = [&null, &on, &off, &yes, &no, &true_, &false_].map(|a| a.map(str::as_bytes));
    if s == b"~" || kws.iter().any(|ss| ss.contains(&s)) {
        return true;
    }

    let good_head = |c| b"$()./;^_~".contains(c);
    let good_tail = |c| b" !%&*+-<=>?@|".contains(c) || good_head(c);

    let mut iter = s.iter();
    match iter.next() {
        None => return true,
        Some(c) if c.is_ascii() && !c.is_ascii_alphabetic() && !good_head(c) => return true,
        _ => (),
    }

    iter.any(|c| c.is_ascii() && !c.is_ascii_alphanumeric() && !good_tail(c))
}

/// Format a value as YAML document, without explicit document start/end markers.
pub fn format(w: &mut Formatter, pp: &write::Pp, lvl: usize, v: &Val) -> fmt::Result {
    use write::format_with;
    write_yaml!(w, pp, lvl, v, format, format_with(w, pp, lvl, v, format))
}

/// Write a value as YAML document, without explicit document start/end markers.
pub fn write(w: &mut dyn io::Write, pp: &write::Pp, level: usize, v: &Val) -> io::Result<()> {
    use write::write_with;
    match v {
        Val::Str(s, jaq_json::Tag::Utf8) if !must_quote(s) => pp.write_str(w, |w| w.write_all(s)),
        _ => write_yaml!(w, pp, level, v, write, write_with(w, pp, level, v, write)),
    }
}
