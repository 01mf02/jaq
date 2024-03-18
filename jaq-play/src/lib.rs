use jaq_interpret::{Ctx, FilterT, ParseCtx, RcIter, Val};
use wasm_bindgen::prelude::*;

/*
fn indent(level: usize) {
    if level > 0 {
        output_tag(&"    ".repeat(level), None);
    }
}

fn print_val(v: &Val, level: usize, indent_start: bool) {
    if indent_start {
        indent(level);
    }
    match v {
        Val::Null => output_tag("null", Some("null")),
        Val::Bool(true) => output_tag("true", Some("bool")),
        Val::Bool(false) => output_tag("false", Some("bool")),
        Val::Int(i) => output_tag(&i.to_string(), Some("number")),
        Val::Float(x) if x.is_finite() => output_tag(&format!("{x:?}"), Some("number")),
        Val::Float(_) => output_tag("null", Some("null")),
        Val::Num(n) => output_tag(n, Some("number")),
        Val::Str(s) => output_tag(&format!("{s:?}"), Some("string")),
        Val::Arr(a) if a.is_empty() => output_tag("[]", Some("array")),
        Val::Arr(a) => {
            output_tag("[", Some("array"));
            output_newline();
            let mut iter = a.iter().peekable();
            while let Some(x) = iter.next() {
                print_val(x, level + 1, true);
                if iter.peek().is_some() {
                    output_tag(",", None);
                }
                output_newline();
            }
            indent(level);
            output_tag("]", Some("array"));
        }
        Val::Obj(o) if o.is_empty() => output_tag("{}", Some("object")),
        Val::Obj(o) => {
            output_tag("{", Some("object"));
            output_newline();
            let mut iter = o.iter().peekable();
            while let Some((k, v)) = iter.next() {
                indent(level + 1);
                output_tag(&format!("{k:?}"), Some("key"));
                output_tag(": ", None);
                print_val(v, level + 1, false);
                if iter.peek().is_some() {
                    output_tag(",", None);
                }
                output_newline();
            }
            indent(level);
            output_tag("}", Some("object"));
        }
    }
}
*/

#[wasm_bindgen]
pub fn run(filter: &str, input: &str, scope: web_sys::DedicatedWorkerGlobalScope) {
    let _ = console_log::init();
    log::info!("Starting run in Rust ...");

    let mut lexer = hifijson::SliceLexer::new(input.as_bytes());
    let inputs = core::iter::from_fn(move || {
        use hifijson::token::Lex;
        Some(Val::parse(lexer.ws_token()?, &mut lexer).map_err(|_e| todo!()))
    });

    // start out only from core filters,
    // which do not include filters in the standard library
    // such as `map`, `select` etc.
    let mut defs = ParseCtx::new(Vec::new());

    // parse the filter
    let (f, errs) = jaq_parse::parse(filter, jaq_parse::main());
    assert_eq!(errs, Vec::new());

    // compile the filter in the context of the given definitions
    let f = defs.compile(f.unwrap());
    assert!(defs.errs.is_empty());

    let inputs = RcIter::new(inputs);

    for x in &inputs {
        for y in f.run((Ctx::new([], &inputs), x.unwrap())) {
            match y {
                Ok(y) => scope.post_message(&y.to_string().into()).unwrap(),
                Err(e) => {
                    scope.post_message(&format!("⚠️ Error: {e}").into()).unwrap();
                    break;
                }
            }
        }
    }
    scope.post_message(&"".into()).unwrap();
}
