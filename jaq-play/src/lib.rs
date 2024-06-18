use core::fmt::{self, Debug, Display, Formatter};
use jaq_interpret::{Ctx, Filter, FilterT, ParseCtx, RcIter, Val};
use wasm_bindgen::prelude::*;

struct Pp<'a> {
    val: &'a Val,
    opts: &'a PpOpts,
    level: usize,
    indent_start: bool,
}

struct PpOpts {
    raw: bool,
    compact: bool,
    indent: String,
}

impl<'a> Pp<'a> {
    fn new(val: &'a Val, opts: &'a PpOpts) -> Self {
        Self {
            val,
            opts,
            level: 0,
            indent_start: false,
        }
    }
}

impl PpOpts {
    fn indent(&self, f: &mut Formatter, level: usize) -> fmt::Result {
        if !self.compact {
            write!(f, "{}", self.indent.repeat(level))?
        }
        Ok(())
    }

    fn newline(&self, f: &mut Formatter) -> fmt::Result {
        if !self.compact {
            writeln!(f)?
        }
        Ok(())
    }
}

fn span(f: &mut Formatter, cls: &str, el: impl Display) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el}</span>")
}

fn span_dbg(f: &mut Formatter, cls: &str, el: impl Debug) -> fmt::Result {
    write!(f, "<span class=\"{cls}\">{el:?}</span>")
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
            self.opts.indent(f, self.level)?;
        }

        match self.val {
            Val::Null => span(f, "null", "null"),
            Val::Bool(b) => span(f, "boolean", b),
            Val::Int(i) => span(f, "number", i),
            Val::Float(x) if x.is_finite() => span_dbg(f, "number", x),
            Val::Float(_) => span(f, "null", "null"),
            Val::Num(n) => span(f, "number", n),
            Val::Str(s) if self.opts.raw => span(f, "string", escape(s)),
            Val::Str(s) => span_dbg(f, "string", escape(s)),
            Val::Arr(a) if a.is_empty() => write!(f, "[]"),
            Val::Arr(a) => {
                write!(f, "[")?;
                self.opts.newline(f)?;
                let mut iter = a.iter().peekable();
                while let Some(val) = iter.next() {
                    Pp {
                        val,
                        opts: self.opts,
                        level: self.level + 1,
                        indent_start: true,
                    }
                    .fmt(f)?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                    self.opts.newline(f)?;
                }
                self.opts.indent(f, self.level)?;
                write!(f, "]")
            }
            Val::Obj(o) if o.is_empty() => write!(f, "{{}}"),
            Val::Obj(o) => {
                write!(f, "{{")?;
                self.opts.newline(f)?;
                let mut iter = o.iter().peekable();
                while let Some((k, val)) = iter.next() {
                    self.opts.indent(f, self.level + 1)?;
                    span_dbg(f, "key", escape(k))?;
                    write!(f, ":")?;
                    if !self.opts.compact {
                        write!(f, " ")?;
                    }
                    Pp {
                        val,
                        opts: self.opts,
                        level: self.level + 1,
                        indent_start: false,
                    }
                    .fmt(f)?;
                    if iter.peek().is_some() {
                        write!(f, ",")?;
                    }
                    self.opts.newline(f)?;
                }
                self.opts.indent(f, self.level)?;
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

enum Error {
    Chumsky(Vec<ChumskyError>),
    Hifijson(String),
    Jaq(jaq_interpret::Error),
}

#[wasm_bindgen]
pub fn run(filter: &str, input: &str, settings: &JsValue, scope: &Scope) {
    let _ = console_log::init();
    log::debug!("Starting run in Rust ...");

    let settings = Settings::try_from(settings).unwrap();
    log::debug!("{settings:?}");

    let indent = if settings.tab {
        "\t".to_string()
    } else {
        " ".repeat(settings.indent)
    };

    let pp_opts = PpOpts {
        raw: settings.raw_output,
        compact: settings.compact,
        indent,
    };

    let post_value = |y| {
        scope
            .post_message(&Pp::new(&y, &pp_opts).to_string().into())
            .unwrap()
    };
    match process(filter, input, &settings, post_value) {
        Ok(()) => (),
        Err(Error::Chumsky(errs)) => {
            for e in errs {
                scope
                    .post_message(&format!("⚠️ Parse error: {}", report(&filter, &e)).into())
                    .unwrap();
            }
        }
        Err(Error::Hifijson(e)) => {
            scope
                .post_message(&format!("⚠️ Parse error: {e}").into())
                .unwrap();
        }
        Err(Error::Jaq(e)) => {
            scope.post_message(&format!("⚠️ Error: {e}").into()).unwrap();
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
        Box::new(std::iter::once(input))
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
    let filter = parse(filter, Vec::new()).map_err(Error::Chumsky)?;

    let inputs = read_str(settings, input);

    let inputs = Box::new(inputs) as Box<dyn Iterator<Item = Result<_, _>>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let inputs = RcIter::new(inputs);
    let null = RcIter::new(null);

    for x in if settings.null_input { &null } else { &inputs } {
        let x = x.map_err(Error::Hifijson)?;
        for y in filter.run((Ctx::new([], &inputs), x)) {
            f(y.map_err(Error::Jaq)?)
        }
    }
    Ok(())
}

type ChumskyError = chumsky::error::Simple<String>;

fn parse(filter_str: &str, vars: Vec<String>) -> Result<Filter, Vec<ChumskyError>> {
    let mut defs = ParseCtx::new(vars);
    defs.insert_natives(jaq_core::core());
    defs.insert_defs(jaq_std::std());
    assert!(defs.errs.is_empty());
    let (filter, errs) = jaq_parse::parse(filter_str, jaq_parse::main());
    if !errs.is_empty() {
        return Err(errs);
    }
    let filter = defs.compile(filter.unwrap());
    if defs.errs.is_empty() {
        Ok(filter)
    } else {
        Err(defs
            .errs
            .into_iter()
            .map(|error| ChumskyError::custom(error.1, error.0.to_string()))
            .collect())
    }
}

#[derive(Debug)]
struct Report<'a> {
    code: &'a str,
    message: String,
    labels: Vec<(core::ops::Range<usize>, String, Color)>,
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

fn report<'a>(code: &'a str, e: &chumsky::error::Simple<String>) -> Report<'a> {
    use chumsky::error::SimpleReason;

    let eof = || "end of input".to_string();

    let message = if let SimpleReason::Custom(msg) = e.reason() {
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
            let f = |e: &Option<String>| e.as_ref().map_or_else(eof, |e| e.to_string());
            e.expected().map(f).collect::<Vec<_>>().join(", ")
        };
        format!("{found}{when}, expected {expected}",)
    };

    let label = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        let token = |c: &String| format!("token {}", Color::Red.apply(c));
        format!("Unexpected {}", e.found().map_or_else(eof, token))
    };
    // convert character indices to byte offsets
    let char_to_byte = |i| {
        code.char_indices()
            .map(|(i, _c)| i)
            .chain([code.len(), code.len()])
            .nth(i)
            .unwrap()
    };
    let conv = |span: &core::ops::Range<_>| char_to_byte(span.start)..char_to_byte(span.end);
    let mut labels = Vec::from([(conv(&e.span()), label, Color::Red)]);

    if let SimpleReason::Unclosed { span, delimiter } = e.reason() {
        let text = format!("Unclosed delimiter {}", Color::Yellow.apply(delimiter));
        labels.insert(0, (conv(span), text, Color::Yellow));
    }
    Report {
        code,
        message,
        labels,
    }
}

impl Display for Report<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use codesnake::{Block, CodeWidth, Label, LineIndex};
        let idx = LineIndex::new(self.code);
        let labels = self.labels.clone().into_iter().map(|(range, text, color)| {
            Label::new(range, text).with_style(move |s| color.apply(s).to_string())
        });
        let block = Block::new(&idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        });
        writeln!(f, "{}", self.message)?;
        write!(f, "{}\n{}{}", block.prologue(), block, block.epilogue())
    }
}
