#![no_std]
extern crate alloc;

use alloc::{borrow::ToOwned, format, string::ToString};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{self, Debug, Display, Formatter};
use jaq_core::{compile, data, load, unwrap_valr, Ctx, DataT, Lut, Vars};
use jaq_json::{fmt_str, Val};
use jaq_std::input::{HasInputs, Inputs, RcIter};
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

impl PpOpts {
    fn indent(&self, f: &mut Formatter, level: usize) -> fmt::Result {
        if !self.compact {
            write!(f, "{}", self.indent.repeat(level))?;
        }
        Ok(())
    }

    fn newline(&self, f: &mut Formatter) -> fmt::Result {
        if !self.compact {
            writeln!(f)?;
        }
        Ok(())
    }
}

fn fmt_seq<T, I, F>(fmt: &mut Formatter, opts: &PpOpts, level: usize, xs: I, f: F) -> fmt::Result
where
    I: IntoIterator<Item = T>,
    F: Fn(&mut Formatter, T) -> fmt::Result,
{
    opts.newline(fmt)?;
    let mut iter = xs.into_iter().peekable();
    while let Some(x) = iter.next() {
        opts.indent(fmt, level + 1)?;
        f(fmt, x)?;
        if iter.peek().is_some() {
            write!(fmt, ",")?;
        }
        opts.newline(fmt)?;
    }
    opts.indent(fmt, level)
}

fn fmt_val(f: &mut Formatter, opts: &PpOpts, level: usize, v: &Val) -> fmt::Result {
    let display = |s| FormatterFn(move |f: &mut Formatter| fmt_str(f, &escape(s)));
    match v {
        Val::Null => span(f, "null", "null"),
        Val::Bool(b) => span(f, "boolean", b),
        Val::Num(n) => span(f, "number", n),
        Val::Str(s) => span(f, "string", display(s)),
        Val::Arr(a) if a.is_empty() => write!(f, "[]"),
        Val::Arr(a) => {
            write!(f, "[")?;
            fmt_seq(f, opts, level, &**a, |f, x| fmt_val(f, opts, level + 1, x))?;
            write!(f, "]")
        }
        Val::Obj(o) if o.is_empty() => write!(f, "{{}}"),
        Val::Obj(o) => {
            write!(f, "{{")?;
            fmt_seq(f, opts, level, &**o, |f, (k, val)| {
                span(f, "key", display(k))?;
                write!(f, ":")?;
                if !opts.compact {
                    write!(f, " ")?;
                }
                fmt_val(f, opts, level + 1, val)
            })?;
            write!(f, "}}")
        }
    }
}

fn span(f: &mut Formatter, cls: &str, el: impl Display) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el}</span>")
}

fn escape(s: &str) -> String {
    use aho_corasick::AhoCorasick;
    let patterns = &["&", "<", ">"];
    let replaces = &["&amp;", "&lt;", "&gt;"];
    let ac = AhoCorasick::new(patterns).unwrap();
    ac.replace_all(s, replaces)
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

type FileReports = (load::File<String, ()>, Vec<Report>);

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
            Val::Str(s) if settings.raw_output => span(f, "string", escape(s)),
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

                    let block = e.into_block(&idx);
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
        let vals = json_slice(input.as_bytes());
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

fn json_slice(slice: &[u8]) -> impl Iterator<Item = Result<Val, String>> + '_ {
    let mut lexer = hifijson::SliceLexer::new(slice);
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        Some(Val::parse(lexer.ws_token()?, &mut lexer).map_err(|e| e.to_string()))
    })
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

    let input_funs = IntoIterator::into_iter(jaq_std::input::funs()).map(jaq_std::run);
    let compiler = Compiler::default()
        .with_funs(jaq_std::funs().chain(jaq_json::funs()).chain(input_funs))
        .with_global_vars(vars.iter().map(|v| &**v));
    let filter = compiler.compile(modules).map_err(compile_errors)?;
    Ok((vals, filter))
}

fn load_errors(errs: load::Errors<&str, ()>) -> Vec<FileReports> {
    use load::Error;

    let errs = errs.into_iter().map(|(file, err)| {
        let code = file.code;
        let err = match err {
            Error::Io(errs) => errs.into_iter().map(|e| report_io(code, e)).collect(),
            Error::Lex(errs) => errs.into_iter().map(|e| report_lex(code, e)).collect(),
            Error::Parse(errs) => errs.into_iter().map(|e| report_parse(code, e)).collect(),
        };
        (file.map_code(|s| s.into()), err)
    });
    errs.collect()
}

fn compile_errors(errs: compile::Errors<&str, ()>) -> Vec<FileReports> {
    let errs = errs.into_iter().map(|(file, errs)| {
        let code = file.code;
        let errs = errs.into_iter().map(|e| report_compile(code, e)).collect();
        (file.map_code(|s| s.into()), errs)
    });
    errs.collect()
}

type StringColors = Vec<(String, Option<Color>)>;

#[derive(Debug)]
struct Report {
    message: String,
    labels: Vec<(core::ops::Range<usize>, StringColors, Color)>,
}

#[derive(Clone, Debug)]
enum Color {
    Yellow,
    Red,
}

impl Color {
    fn apply(&self, d: impl Display) -> String {
        let mut color = format!("{self:?}");
        color.make_ascii_lowercase();
        format!("<span class={color}>{d}</span>",)
    }
}

fn report_io(code: &str, (path, error): (&str, String)) -> Report {
    let path_range = load::span(code, path);
    Report {
        message: format!("could not load file {path}: {error}"),
        labels: [(path_range, [(error, None)].into(), Color::Red)].into(),
    }
}

fn report_lex(code: &str, (expected, found): load::lex::Error<&str>) -> Report {
    use load::span;
    // truncate found string to its first character
    let found = &found[..found.char_indices().nth(1).map_or(found.len(), |(i, _)| i)];

    let found_range = span(code, found);
    let found = match found {
        "" => [("unexpected end of input".to_string(), None)].into(),
        c => [("unexpected character ", None), (c, Some(Color::Red))]
            .map(|(s, c)| (s.into(), c))
            .into(),
    };
    let label = (found_range, found, Color::Red);

    let labels = match expected {
        load::lex::Expect::Delim(open) => {
            let text = [("unclosed delimiter ", None), (open, Some(Color::Yellow))]
                .map(|(s, c)| (s.into(), c));
            Vec::from([(span(code, open), text.into(), Color::Yellow), label])
        }
        _ => Vec::from([label]),
    };

    Report {
        message: format!("expected {}", expected.as_str()),
        labels,
    }
}

fn report_parse(code: &str, (expected, found): load::parse::Error<&str>) -> Report {
    let found_range = load::span(code, found);

    let found = if found.is_empty() {
        "unexpected end of input"
    } else {
        "unexpected token"
    };
    let found = [(found.to_string(), None)].into();

    Report {
        message: format!("expected {}", expected.as_str()),
        labels: Vec::from([(found_range, found, Color::Red)]),
    }
}

fn report_compile(code: &str, (found, undefined): compile::Error<&str>) -> Report {
    use compile::Undefined::Filter;
    let found_range = load::span(code, found);
    let wnoa = |exp, got| format!("wrong number of arguments (expected {exp}, found {got})");
    let message = match (found, undefined) {
        ("reduce", Filter(arity)) => wnoa("2", arity),
        ("foreach", Filter(arity)) => wnoa("2 or 3", arity),
        (_, undefined) => format!("undefined {}", undefined.as_str()),
    };
    let found = [(message.clone(), None)].into();

    Report {
        message,
        labels: Vec::from([(found_range, found, Color::Red)]),
    }
}

type CodeBlock = codesnake::Block<codesnake::CodeWidth<String>, String>;

impl Report {
    fn into_block(self, idx: &codesnake::LineIndex) -> CodeBlock {
        use codesnake::{Block, CodeWidth, Label};
        let color_maybe = |(text, color): (_, Option<Color>)| match color {
            None => text,
            Some(color) => color.apply(text).to_string(),
        };
        let labels = self.labels.into_iter().map(|(range, text, color)| {
            let text = text.into_iter().map(color_maybe).collect::<Vec<_>>();
            Label::new(range)
                .with_text(text.join(""))
                .with_style(move |s| color.apply(s).to_string())
        });
        Block::new(idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        })
    }
}
