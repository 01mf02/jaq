#![no_std]
extern crate alloc;

use alloc::{borrow::ToOwned, format, string::ToString};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{self, Debug, Display, Formatter};
use jaq_bla::data::{self, Runner, Writer};
use jaq_bla::{compile, load::Color, read};
use jaq_core::Vars;
use jaq_json::write::{Colors, Pp};
use jaq_json::{bstr, write_bytes, write_utf8, Tag, Val};
use wasm_bindgen::prelude::*;
use web_sys::DedicatedWorkerGlobalScope as Scope;

struct FormatterFn<F>(F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatterFn<F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0(f)
    }
}

fn fmt_json(w: &mut Formatter, pp: &Pp, level: usize, v: &Val) -> fmt::Result {
    macro_rules! color {
        ($style:ident, $g:expr) => {{
            write!(w, "{}", pp.colors.$style)?;
            $g?;
            write!(w, "{}", pp.colors.reset)
        }};
    }

    match v {
        Val::Str(b, Tag::Bytes) => {
            let fun = FormatterFn(move |f: &mut Formatter| write_bytes!(f, b));
            color!(bstr, write!(w, "{}", escape_str(&fun.to_string())))
        }
        Val::Str(s, Tag::Utf8) => {
            let fun = FormatterFn(move |f: &mut Formatter| {
                write_utf8!(f, s, |part| bstr(&escape_bytes(part)).fmt(f))
            });
            color!(str, write!(w, "{}", fun))
        }
        _ => jaq_json::write::format_with(w, pp, level, v, fmt_json),
    }
}

fn html_colors() -> Colors {
    let span = |cls| format!(r#"<span class="{cls}">"#);
    Colors {
        null: span("null"),
        r#true: span("boolean"),
        r#false: span("boolean"),
        num: span("number"),
        str: span("string"),
        arr: span("array"),
        obj: span("object"),

        bstr: span("bytes"),

        reset: "</span>".into(),
    }
}

const AC_PATTERNS: &[&str] = &["&", "<", ">"];
const AC_REPLACES: &[&str] = &["&amp;", "&lt;", "&gt;"];

fn escape_bytes(s: &[u8]) -> Vec<u8> {
    let ac = aho_corasick::AhoCorasick::new(AC_PATTERNS).unwrap();
    ac.replace_all_bytes(s, AC_REPLACES)
}
fn escape_str(s: &str) -> String {
    let ac = aho_corasick::AhoCorasick::new(AC_PATTERNS).unwrap();
    ac.replace_all(s, AC_REPLACES)
}

#[allow(dead_code)]
#[derive(Debug)]
struct Settings {
    raw_input: bool,
    slurp: bool,
    null_input: bool,
    raw_output: bool,
    compact: bool,
    //join_output: bool,
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
            //join_output: get_bool("join-output")?,
            indent: get("indent").and_then(as_usize)?,
            tab: get_bool("tab")?,
        })
    }

    fn indent(&self) -> String {
        if self.tab {
            "\t".to_string()
        } else {
            " ".repeat(self.indent)
        }
    }

    fn pp(&self) -> Pp {
        Pp {
            indent: (!self.compact).then(|| self.indent()),
            sep_space: !self.compact,
            colors: html_colors(),
            sort_keys: false,
        }
    }

    fn runner(&self) -> Runner {
        Runner {
            color_err: true,
            null_input: self.null_input,
            writer: Writer {
                pp: self.pp(),
                ..Default::default()
            },
        }
    }
}

enum Error {
    Hifijson(String),
    Jaq(jaq_core::Error<Val>),
}

#[wasm_bindgen]
pub fn run(filter: &str, input: &str, settings: &JsValue, scope: &Scope) {
    let _ = console_log::init_with_level(log::Level::Debug);
    log::trace!("Starting run in Rust ...");

    let settings = Settings::try_from(settings).unwrap();
    log::trace!("{settings:?}");
    let runner = &settings.runner();

    let post = |s: String| scope.post_message(&s.into()).unwrap();
    let post_value = |y: jaq_json::ValR| {
        let y = y.map_err(Error::Jaq)?;
        let s = FormatterFn(|f: &mut Formatter| match &y {
            Val::Str(s, _) if settings.raw_output => bstr(&escape_bytes(s)).fmt(f),
            y => fmt_json(f, &runner.writer.pp, 0, y),
        });
        post(s.to_string());
        Ok(())
    };
    let vars = Vars::new([]);
    let inputs = read_str(&settings, input);

    match compile::<()>(filter, &[]) {
        Err(file_reports) => {
            for (file, reports) in file_reports {
                let idx = codesnake::LineIndex::new(&file.code);
                for e in reports {
                    let error = format!("⚠️ Error: {}", e.message);
                    post(error);

                    let block = e.to_block(&idx, color);
                    let block = format!("{}\n{}{}", block.prologue(), block, block.epilogue());
                    post(block);
                }
            }
        }
        Ok(filter) => match data::run(runner, &filter, vars, inputs, Error::Hifijson, post_value) {
            Ok(()) => (),
            Err(Error::Hifijson(e)) => post(format!("⚠️ Parse error: {e}")),
            Err(Error::Jaq(e)) => post(format!("⚠️ Error: {e}")),
        },
    }

    // signal that we are done
    scope.post_message(&JsValue::NULL).unwrap();
}

fn read_str<'a>(
    settings: &Settings,
    input: &'a str,
) -> Box<dyn Iterator<Item = Result<Val, String>> + 'a> {
    if settings.raw_input {
        Box::new(raw_input(settings.slurp, input).map(|s| Ok(Val::from(s.to_owned()))))
    } else {
        let vals =
            jaq_json::read::parse_many(input.as_bytes()).map(|r| r.map_err(|e| e.to_string()));
        Box::new(read::collect_if(settings.slurp, vals))
    }
}

fn raw_input(slurp: bool, input: &str) -> impl Iterator<Item = &str> {
    if slurp {
        Box::new(core::iter::once(input))
    } else {
        Box::new(input.lines()) as Box<dyn Iterator<Item = _>>
    }
}

fn color(color: Color, text: String) -> String {
    let mut color = format!("{color:?}");
    color.make_ascii_lowercase();
    format!("<span class={color}>{text}</span>",)
}
