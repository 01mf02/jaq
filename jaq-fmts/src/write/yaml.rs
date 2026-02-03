//! YAML support.
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use jaq_json::{write, write_utf8, Num, Tag, Val};
use std::io;

macro_rules! format_yaml {
    ($w:ident, $pp:ident, $level:ident, $v:ident, $f:expr) => {{
        macro_rules! style {
            ($style:ident, $h:expr) => {
                jaq_json::style!($w, $pp, $style, $h)
            };
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
            Val::Str(b, Tag::Utf8) if !must_quote(b) => {
                style!(str, write!($w, "{}", jaq_json::bstr(b)))
            }
            Val::Str(b, Tag::Bytes) => {
                style!(bstr, write!($w, "!!binary {}", BASE64.encode(b)))
            }
            Val::Arr(a) if !a.is_empty() && indent.is_some() => {
                let mut iter = a.iter().peekable();
                while let Some(v) = iter.next() {
                    nested!(v, iter, style!(arr, write!($w, "-")))?
                }
                Ok(())
            }
            Val::Obj(o) if !o.is_empty() && indent.is_some() => {
                let mut unindented = write::Pp {
                    indent: None,
                    ..$pp.clone()
                };
                if !$pp.styles.key.is_empty() {
                    unindented.styles = Default::default();
                }
                let mut iter = o.iter().peekable();
                while let Some((k, v)) = iter.next() {
                    nested!(v, iter, {
                        style!(key, $f($w, &unindented, $level, k))?;
                        style!(obj, write!($w, ":"))
                    })?
                }
                Ok(())
            }
            Val::Num(Num::Float(f64::INFINITY)) => style!(num, write!($w, ".inf")),
            Val::Num(Num::Float(f64::NEG_INFINITY)) => style!(num, write!($w, "-.inf")),
            Val::Num(Num::Float(fl)) if fl.is_nan() => style!(num, write!($w, ".nan")),
            _ => jaq_json::format_val!($w, $pp, $level, $v, $f),
        }
    }};
}

macro_rules! write_yaml {
    ($w:ident, $pp:ident, $level:ident, $v:ident, $f:expr) => {{
        macro_rules! style {
            ($style:ident, $g:expr) => {{
                jaq_json::style!($w, $pp, $style, $g)
            }};
        }
        match $v {
            Val::Str(b, Tag::Utf8) if !must_quote(b) => style!(str, $w.write_all(b)),
            Val::Str(s, Tag::Utf8) => style!(str, write_utf8!($w, s, |part| $w.write_all(part))),
            _ => format_yaml!($w, $pp, $level, $v, $f),
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

/// Write a value as YAML document, without explicit document start/end markers.
pub fn write(w: &mut dyn io::Write, pp: &write::Pp, level: usize, v: &Val) -> io::Result<()> {
    write_yaml!(w, pp, level, v, write)
}
