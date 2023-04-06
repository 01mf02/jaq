//! Tests for custom filters.

use std::{iter::once, rc::Rc};

use itertools::Itertools;
use jaq_core::{parse, Ctx, CustomFilter, Definitions, Error, RcIter, Val};
use serde_json::{json, Value};

pub fn yields<const N: usize>(
    defs: &Definitions,
    x: Value,
    f: &str,
    ys: [Value; N],
    err: Option<Error>,
) {
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
fn arity0_source_only() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "natzero",
        0,
        CustomFilter::new(|_cv| Box::new(once(Ok(Val::Int(0))))),
    );
    defs.insert_custom(
        "nattwenty",
        0,
        CustomFilter::new(|_cv| Box::new(once(Ok(Val::Int(20))))),
    );
    defs.insert_custom(
        "hello",
        0,
        CustomFilter::new(|_cv| Box::new(once(Ok(Val::Str(Rc::new("world".into())))))),
    );

    yields(&defs, Value::Null, "natzero", [json!(0)], None);
    yields(&defs, Value::Null, "nattwenty", [json!(20)], None);
    yields(&defs, Value::Null, "hello", [json!("world")], None);
}

#[test]
fn arity0_and_sink() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "str_rev",
        0,
        CustomFilter::new(|(_, val)| {
            Box::new(once(
                val.to_str().map(|s| Val::str(s.chars().rev().join(""))),
            ))
        }),
    );

    yields(&defs, json!("hello"), "str_rev", [json!("olleh")], None);
    yields(&defs, json!(""), "str_rev", [json!("")], None);
}
