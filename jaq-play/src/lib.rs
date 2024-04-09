use core::fmt::{self, Debug, Display, Formatter};
use jaq_interpret::{Ctx, FilterT, ParseCtx, RcIter, Val};
use wasm_bindgen::prelude::*;

struct Pp<'a> {
    val: &'a Val,
    level: usize,
    indent_start: bool,
}

impl<'a> Pp<'a> {
    fn new(val: &'a Val) -> Self {
        Self {
            val,
            level: 0,
            indent_start: false,
        }
    }
}

fn span(f: &mut Formatter, cls: &str, el: impl Display) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el}</span>")
}

fn span_dbg(f: &mut Formatter, cls: &str, el: impl Debug) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el:?}</span>")
}

fn indent(f: &mut Formatter, level: usize) -> fmt::Result {
    write!(f, "{}", "    ".repeat(level))
}

fn escape(s: &str) -> String {
    use aho_corasick::AhoCorasick;
    let patterns = &["&", "<", ">"];
    let replaces = &["&amp;", "&lt;", "&gt;"];
    let ac = AhoCorasick::new(patterns).unwrap();
    ac.replace_all(s, replaces)
}

impl<'a> Display for Pp<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.indent_start {
            indent(f, self.level)?;
        }

        match self.val {
            Val::Null => span(f, "null", "null"),
            Val::Bool(b) => span(f, "boolean", b),
            Val::Int(i) => span(f, "number", i),
            Val::Float(x) if x.is_finite() => span_dbg(f, "number", x),
            Val::Float(_) => span(f, "null", "null"),
            Val::Num(n) => span(f, "number", n),
            Val::Str(s) => span_dbg(f, "string", escape(s)),
            Val::Arr(a) if a.is_empty() => write!(f, "[]"),
            Val::Arr(a) => {
                write!(f, "[")?;
                writeln!(f)?;
                let mut iter = a.iter().peekable();
                while let Some(val) = iter.next() {
                    Pp {
                        val,
                        level: self.level + 1,
                        indent_start: true,
                    }
                    .fmt(f)?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                    writeln!(f)?;
                }
                indent(f, self.level)?;
                write!(f, "]")
            }
            Val::Obj(o) if o.is_empty() => write!(f, "{{}}"),
            Val::Obj(o) => {
                write!(f, "{{")?;
                writeln!(f)?;
                let mut iter = o.iter().peekable();
                while let Some((k, val)) = iter.next() {
                    indent(f, self.level + 1)?;
                    span_dbg(f, "key", escape(k))?;
                    write!(f, ": ")?;
                    Pp {
                        val,
                        level: self.level + 1,
                        indent_start: false,
                    }
                    .fmt(f)?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                    writeln!(f)?;
                }
                indent(f, self.level)?;
                write!(f, "}}")
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct Settings {
    raw_input: bool,
    slurp: bool,
    null_input: bool,
    raw_output: bool,
    compact: bool,
    join_output: bool,
    indent: usize,
    tab: bool,
}

impl Settings {
    fn try_from(v: &JsValue) -> Option<Self> {
        let get = |key: &str| js_sys::Reflect::get(v, &key.into()).ok();
        let get_bool = |key: &str| get(key).and_then(|v| v.as_bool());
        let as_usize = |v: JsValue| v.as_string()?.parse().ok();
        Some(Self {
            raw_input: get_bool("raw-input")?,
            slurp: get_bool("slurp")?,
            null_input: get_bool("null-input")?,
            raw_output: get_bool("raw-output")?,
            compact: get_bool("compact")?,
            join_output: get_bool("join-output")?,
            indent: get("indent").and_then(as_usize)?,
            tab: get_bool("tab")?,
        })
    }
}

use web_sys::DedicatedWorkerGlobalScope as Scope;

#[wasm_bindgen]
pub fn run(filter: &str, input: &str, settings: &JsValue, scope: Scope) {
    let _ = console_log::init();
    log::debug!("Starting run in Rust ...");

    let settings = Settings::try_from(settings).unwrap();
    log::debug!("{settings:?}");

    let mut lexer = hifijson::SliceLexer::new(input.as_bytes());
    let inputs = core::iter::from_fn(move || {
        use hifijson::token::Lex;
        Some(Val::parse(lexer.ws_token()?, &mut lexer).map_err(|e| e.to_string()))
    });

    let mut defs = ParseCtx::new(Vec::new());
    defs.insert_natives(jaq_core::core());
    defs.insert_defs(jaq_std::std());

    // parse the filter
    let (f, errs) = jaq_parse::parse(filter, jaq_parse::main());
    if !errs.is_empty() {
        for e in errs {
            let mut buf = Vec::new();
            let cache = ariadne::Source::from(filter);
            report(e).write(cache, &mut buf).unwrap();
            let s = String::from_utf8(buf).unwrap();
            scope.post_message(&format!("⚠️ Parse {s}").into()).unwrap();
        }
        scope.post_message(&JsValue::NULL).unwrap();
        return;
    }

    // compile the filter in the context of the given definitions
    let f = defs.compile(f.unwrap());
    assert!(defs.errs.is_empty());

    let inputs = RcIter::new(inputs);

    for x in &inputs {
        for y in f.run((Ctx::new([], &inputs), x.unwrap())) {
            match y {
                Ok(y) => scope.post_message(&Pp::new(&y).to_string().into()).unwrap(),
                Err(e) => {
                    scope.post_message(&format!("⚠️ Error: {e}").into()).unwrap();
                    break;
                }
            }
        }
    }
    // signal that we are done
    scope.post_message(&JsValue::NULL).unwrap();
}

fn report<'a>(e: chumsky::error::Simple<String>) -> ariadne::Report<'a> {
    use ariadne::{Color, Fmt, Label, Report, ReportKind};
    use chumsky::error::SimpleReason;

    let (red, yellow) = (Color::Unset, Color::Unset);
    let config = ariadne::Config::default().with_color(false);

    let msg = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        let found = if e.found().is_some() {
            "Unexpected token"
        } else {
            "Unexpected end of input"
        };
        let when = if let Some(label) = e.label() {
            format!(" while parsing {label}")
        } else {
            String::new()
        };
        let expected = if e.expected().len() == 0 {
            "something else".to_string()
        } else {
            e.expected()
                .map(|expected| match expected {
                    Some(expected) => expected.to_string(),
                    None => "end of input".to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ")
        };
        format!("{found}{when}, expected {expected}",)
    };

    let label = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        format!(
            "Unexpected {}",
            e.found()
                .map(|c| format!("token {}", c.fg(red)))
                .unwrap_or_else(|| "end of input".to_string())
        )
    };

    let report = Report::build(ReportKind::Error, (), e.span().start)
        .with_message(msg)
        .with_label(Label::new(e.span()).with_message(label).with_color(red));

    let report = match e.reason() {
        SimpleReason::Unclosed { span, delimiter } => report.with_label(
            Label::new(span.clone())
                .with_message(format!("Unclosed delimiter {}", delimiter.fg(yellow)))
                .with_color(yellow),
        ),
        SimpleReason::Unexpected => report,
        SimpleReason::Custom(_) => report,
    };

    report.with_config(config).finish()
}
