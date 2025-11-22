#![no_std]
extern crate alloc;

use alloc::{borrow::ToOwned, format, string::ToString};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{self, Debug, Display, Formatter};
use jaq_bla::{compile_errors, load_errors, write_seq, Color};
use jaq_core::{compile, data, unwrap_valr, Ctx, DataT, Lut, Native, Vars};
use jaq_json::{bstr, json, write_byte, write_bytes, write_utf8, Tag, Val};
use jaq_std::input::{self, HasInputs, Inputs, RcIter};
use wasm_bindgen::prelude::*;

struct DataKind;

impl DataT for DataKind {
    type V<'a> = Val;
    type Data<'a> = Data<'a>;
}

#[derive(Clone)]
struct Data<'a> {
    lut: &'a Lut<DataKind>,
    inputs: Inputs<'a, Val>,
}

impl<'a> data::HasLut<'a, DataKind> for Data<'a> {
    fn lut(&self) -> &'a Lut<DataKind> {
        self.lut
    }
}
impl<'a> HasInputs<'a, Val> for Data<'a> {
    fn inputs(&self) -> Inputs<'a, Val> {
        self.inputs
    }
}

type Filter = jaq_core::Filter<DataKind>;

struct FormatterFn<F>(F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatterFn<F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0(f)
    }
}

struct PpOpts {
    compact: bool,
    indent: String,
}

fn fmt_val(f: &mut Formatter, opts: &PpOpts, level: usize, v: &Val) -> fmt::Result {
    match v {
        Val::Null => span(f, "null", "null"),
        Val::Bool(b) => span(f, "boolean", b),
        Val::Num(n) => span(f, "number", n),
        Val::Str(b, Tag::Bytes) => {
            let fun = FormatterFn(move |f: &mut Formatter| write_bytes!(f, b));
            span(f, "bytes", escape_str(&fun.to_string()))
        }
        Val::Str(s, Tag::Utf8) => {
            let fun = FormatterFn(move |f: &mut Formatter| {
                write_utf8!(f, s, |part| bstr(&escape_bytes(part)).fmt(f))
            });
            span(f, "string", fun)
        }
        Val::Arr(a) if a.is_empty() => write!(f, "[]"),
        Val::Arr(a) => {
            write!(f, "[")?;
            write_seq!(f, opts, level, &**a, |f, x| fmt_val(f, opts, level + 1, x))?;
            write!(f, "]")
        }
        Val::Obj(o) if o.is_empty() => write!(f, "{{}}"),
        Val::Obj(o) => {
            write!(f, "{{")?;
            write_seq!(f, opts, level, &**o, |f: &mut _, (k, v)| {
                use jaq_std::ValT;
                fmt_val(f, opts, level + 1, k)?;
                write!(f, ":")?;
                if !opts.compact || !k.is_utf8_str() {
                    write!(f, " ")?;
                }
                fmt_val(f, opts, level + 1, v)
            })?;
            write!(f, "}}")
        }
    }
}

fn span(f: &mut Formatter, cls: &str, el: impl Display) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el}</span>")
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
}

use web_sys::DedicatedWorkerGlobalScope as Scope;

type FileReports = jaq_bla::FileReports<()>;

enum Error {
    Report(Vec<FileReports>),
    Hifijson(String),
    Jaq(jaq_core::Error<Val>),
}

#[wasm_bindgen]
pub fn run(filter: &str, input: &str, settings: &JsValue, scope: &Scope) {
    let _ = console_log::init_with_level(log::Level::Debug);
    log::trace!("Starting run in Rust ...");

    let settings = Settings::try_from(settings).unwrap();
    log::trace!("{settings:?}");

    let indent = if settings.tab {
        "\t".to_string()
    } else {
        " ".repeat(settings.indent)
    };

    let pp_opts = PpOpts {
        compact: settings.compact,
        indent,
    };

    let post_value = |y| {
        let s = FormatterFn(|f: &mut Formatter| match &y {
            Val::Str(s, _) if settings.raw_output => span(f, "string", bstr(&escape_bytes(s))),
            y => fmt_val(f, &pp_opts, 0, y),
        });
        scope.post_message(&s.to_string().into()).unwrap();
    };
    match process(filter, input, &settings, post_value) {
        Ok(()) => (),
        Err(Error::Report(file_reports)) => {
            for (file, reports) in file_reports {
                let idx = codesnake::LineIndex::new(&file.code);
                for e in reports {
                    let error = format!("⚠️ Error: {}", e.message);
                    scope.post_message(&error.into()).unwrap();

                    let block = e.to_block(&idx, color);
                    let block = format!("{}\n{}{}", block.prologue(), block, block.epilogue());
                    scope.post_message(&block.into()).unwrap();
                }
            }
        }
        Err(Error::Hifijson(e)) => {
            scope
                .post_message(&format!("⚠️ Parse error: {e}").into())
                .unwrap();
        }
        Err(Error::Jaq(e)) => {
            scope
                .post_message(&format!("⚠️ Error: {e}").into())
                .unwrap();
        }
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
        let vals = json::parse_many(input.as_bytes()).map(|r| r.map_err(|e| e.to_string()));
        Box::new(collect_if(settings.slurp, vals))
    }
}

fn raw_input(slurp: bool, input: &str) -> impl Iterator<Item = &str> {
    if slurp {
        Box::new(core::iter::once(input))
    } else {
        Box::new(input.lines()) as Box<dyn Iterator<Item = _>>
    }
}

fn collect_if<'a, T: 'a + FromIterator<T>, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> {
    if slurp {
        Box::new(core::iter::once(iter.collect()))
    } else {
        Box::new(iter)
    }
}

fn process(filter: &str, input: &str, settings: &Settings, f: impl Fn(Val)) -> Result<(), Error> {
    let (vals, filter) = parse(filter, &[]).map_err(Error::Report)?;

    let inputs = read_str(settings, input);

    let iter = Box::new(inputs) as Box<dyn Iterator<Item = Result<_, _>>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let iter: Inputs<_> = &RcIter::new(iter);
    let null: Inputs<_> = &RcIter::new(null);

    let data = Data {
        inputs: iter,
        lut: &filter.lut,
    };
    let vars = Vars::new(vals);
    let ctx = Ctx::<DataKind>::new(data, vars);

    for x in if settings.null_input { null } else { iter } {
        let x = x.map_err(Error::Hifijson)?;
        for y in filter.id.run((ctx.clone(), x)) {
            f(unwrap_valr(y).map_err(Error::Jaq)?);
        }
    }
    Ok(())
}

fn funs() -> impl Iterator<Item = jaq_std::Filter<Native<DataKind>>> {
    let run = jaq_std::run::<DataKind>;
    let std = jaq_std::funs::<DataKind>();
    let input = input::funs::<DataKind>().into_vec().into_iter().map(run);
    std.chain(jaq_json::funs()).chain(input)
}

fn parse(code: &str, vars: &[String]) -> Result<(Vec<Val>, Filter), Vec<FileReports>> {
    use compile::Compiler;
    use jaq_core::load::{import, Arena, File, Loader};

    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let loader = Loader::new(jaq_std::defs().chain(jaq_json::defs()));
    let modules = loader
        .load(&arena, File { path: (), code })
        .map_err(load_errors)?;

    let vals = Vec::new();
    import(&modules, |_path| Err("file loading not supported".into())).map_err(load_errors)?;

    let compiler = Compiler::default()
        .with_funs(funs())
        .with_global_vars(vars.iter().map(|v| &**v));
    let filter = compiler.compile(modules).map_err(compile_errors)?;
    Ok((vals, filter))
}

fn color(color: Color, text: String) -> String {
    let mut color = format!("{color:?}");
    color.make_ascii_lowercase();
    format!("<span class={color}>{text}</span>",)
}
