//! Tests for custom filters.

use std::{iter, rc::Rc};

use jaq_core::{parse, Ctx, Definitions, Error, RcIter, Val, CustomFilter};
use serde_json::{json, Value};

pub fn yields<const N: usize>(defs: &Definitions, x: Value, f: &str, ys: [Value; N], err: Option<Error>) {
    let f = parse::parse(&f, parse::main()).0.unwrap();
    
    let mut errs = Vec::new();
    let f = defs.clone().finish(f, Vec::new(), &mut errs);
    assert_eq!(errs, Vec::new());

    let to = |v| Val::from(v);

    let expected = ys.into_iter().map(|y| Ok(to(y)));
    let expected: Vec<_> = expected.chain(err.into_iter().map(Err)).collect();

    let inputs = RcIter::new(core::iter::empty());
    let out: Vec<_> = f.run(Ctx::new([], &inputs), to(x)).collect();
    assert_eq!(out, expected);
}

#[test]
fn arity0() {
    let mut defs = Definitions::core();
    defs.insert_custom("natzero", 0, CustomFilter::new(
        |_cv| {
            Box::new(iter::once(Ok(Val::Int(0))))
        },
    ));
    defs.insert_custom("nattwenty", 0, CustomFilter::new(
        |_cv| {
            Box::new(iter::once(Ok(Val::Int(20))))
        },
    ));
    defs.insert_custom("hello", 0, CustomFilter::new(
        |_cv| {
            Box::new(iter::once(Ok(Val::Str(Rc::new("world".into())))))
        },
    ));

    yields(&defs, Value::Null, "natzero", [json!(0)], None);
    yields(&defs, Value::Null, "nattwenty", [json!(20)], None);
    yields(&defs, Value::Null, "hello", [json!("world")], None);
}
