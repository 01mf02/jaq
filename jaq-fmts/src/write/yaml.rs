//! YAML support.
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use jaq_json::{write, write_utf8, Num, Val};
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
            Val::TStr(b) if !must_quote(b) => {
                style!(str, write!($w, "{}", jaq_json::bstr(&**b)))
            }
            Val::BStr(b) => {
                style!(bstr, write!($w, "!!binary {}", BASE64.encode(&**b)))
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
            Val::TStr(b) if !must_quote(b) => style!(str, $w.write_all(b)),
            Val::TStr(s) => style!(str, write_utf8!($w, s, |part| $w.write_all(part))),
            _ => format_yaml!($w, $pp, $level, $v, $f),
        }
    }};
}

// https://yaml.org/spec/1.2.2/#rule-ns-plain-one-line
fn ns_plain_one_line(s: &[u8]) -> bool {
    // naming:
    // - ns = no-space
    // - nb = no-break
    // -  b =    break
    // -  c = character
    // -  s = space

    // https://yaml.org/spec/1.2.2/#rule-c-indicator
    let c_indicator = |c: &u8| br#"-?:,[]{}#&*!|>'"%@`"#.contains(c);
    // https://yaml.org/spec/1.2.2/#rule-b-char
    let b_char = |c: &u8| b"\n\r".contains(c);
    // https://yaml.org/spec/1.2.2/#rule-c-printable
    // we omit the check for valid UTF-8 here
    let c_printable = |c: &u8| c.is_ascii_control() == b"\t\n\r".contains(c);
    // https://yaml.org/spec/1.2.2/#rule-nb-char
    // we omit the check for c-byte-order-mark here
    let nb_char = |c: &u8| c_printable(c) && !b_char(c);
    // https://yaml.org/spec/1.2.2/#rule-s-white
    let s_white = |c: &u8| b" \t".contains(c);
    // https://yaml.org/spec/1.2.2/#rule-ns-char
    let ns_char = |c: &u8| nb_char(c) && !s_white(c);
    // https://yaml.org/spec/1.2.2/#rule-c-flow-indicator
    let c_flow_indicator = |c: &u8| b",[]{}".contains(c);
    // https://yaml.org/spec/1.2.2/#rule-ns-plain-safe-in
    let ns_plain_safe_in = |c: &u8| ns_char(c) && !c_flow_indicator(c);
    // we choose safe-in because it is more restrictive than safe-out
    let ns_plain_safe = ns_plain_safe_in;
    // https://yaml.org/spec/1.2.2/#rule-ns-plain-first
    let ns_plain_first = |c: &u8, next: Option<&u8>| {
        (ns_char(c) && !c_indicator(c)) || (b"?:-".contains(c) && next.is_some_and(ns_plain_safe))
    };
    // https://yaml.org/spec/1.2.2/#rule-ns-plain-char
    let ns_plain_char = |prev: &u8, c: &u8, next: Option<&u8>| {
        (ns_plain_safe(c) && !b":#".contains(c))
            || (ns_char(prev) && *c == b'#')
            || (*c == b':' && next.is_some_and(ns_plain_safe))
    };

    let mut iter = s.iter();
    let Some(c) = iter.next() else {
        return false;
    };
    let mut next = iter.next();
    if !ns_plain_first(c, next) {
        return false;
    }
    let mut prev = c;
    while let Some(c) = next.take() {
        next = iter.next();
        if !s_white(c) && !ns_plain_char(prev, c, next) {
            return false;
        }
        prev = c;
    }
    true
}

fn must_quote(s: &[u8]) -> bool {
    let null = ["null", "Null", "NULL"];
    let on = ["on", "On", "ON"];
    let off = ["off", "Off", "OFF"];
    let yes = ["yes", "Yes", "YES"];
    let no = ["no", "No", "NO"];
    let true_ = ["True", "TRUE", "true"];
    let false_ = ["False", "FALSE", "false"];
    let inf = [".inf", ".Inf", ".INF"];
    let nan = [".nan", ".NaN", ".NAN"];
    let kws = [&null, &on, &off, &yes, &no, &true_, &false_, &inf, &nan];
    let kws = kws.map(|a| a.map(str::as_bytes));

    // https://yaml.org/spec/1.2.2/#912-document-markers
    let is_doc_marker = |s: &[u8]| matches!(s, b"---" | b"...");

    // number overapproximation
    let is_pos_num = |s: &[u8]| s.first().is_some_and(u8::is_ascii_digit);
    let is_num = |s: &[u8]| is_pos_num(s.strip_prefix(b"-").unwrap_or(s));

    s == b"~"
        || is_doc_marker(s)
        || is_num(s)
        || kws.iter().any(|ss| ss.contains(&s))
        || !ns_plain_one_line(s)
}

#[test]
fn must_quote_test() {
    // "~" is YAML shorthand for null
    assert!(must_quote(b"~"));

    assert!(must_quote(b"null"));
    assert!(must_quote(b"on"));
    assert!(must_quote(b"off"));
    assert!(must_quote(b"true"));
    assert!(must_quote(b"false"));

    assert!(!must_quote(b"nul"));
    assert!(!must_quote(b"trues"));

    assert!(!must_quote(b"_foo"));
    assert!(!must_quote(b"foo-bar"));
    assert!(!must_quote(b"foo_bar"));
    assert!(must_quote(b"- a"));
    assert!(must_quote(b"-,a"));

    assert!(!must_quote(b"What hath God wrought"));
    assert!(!must_quote(b"What hath God wrought?"));

    assert!(!must_quote(b"a# bla"));
    assert!(must_quote(b"a # bla"));
    assert!(!must_quote(b"a:1"));
    assert!(must_quote(b"a: 1"));

    assert!(!must_quote(b"(a)"));
    assert!(!must_quote(b"_a_"));
    assert!(!must_quote(b"(1 + 2 * 3 / 4 % 5 <= 6"));
    assert!(!must_quote(b"a > b || c && d"));
    assert!(!must_quote(b"a > b || c && d"));
    assert!(!must_quote(b"Beer is good! Are you sure? Hmm ... Yes."));
    assert!(!must_quote(b"Sure?"));

    assert!(must_quote(b"-1"));
    assert!(!must_quote(b"-a1"));
}

/// Write a value as YAML document, without explicit document start/end markers.
pub fn write(w: &mut dyn io::Write, pp: &write::Pp, level: usize, v: &Val) -> io::Result<()> {
    write_yaml!(w, pp, level, v, write)
}
