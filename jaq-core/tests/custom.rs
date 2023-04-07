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
        CustomFilter::new(0, |_, _cv| Box::new(once(Ok(Val::Int(0))))),
    );
    defs.insert_custom(
        "nattwenty",
        CustomFilter::new(0, |_, _cv| Box::new(once(Ok(Val::Int(20))))),
    );
    defs.insert_custom(
        "hello",
        CustomFilter::new(0, |_, _cv| {
            Box::new(once(Ok(Val::Str(Rc::new("world".into())))))
        }),
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
        CustomFilter::new(0, |_, (_, val)| {
            Box::new(once(
                val.to_str().map(|s| Val::str(s.chars().rev().join(""))),
            ))
        }),
    );

    yields(&defs, json!("hello"), "str_rev", [json!("olleh")], None);
    yields(&defs, json!(""), "str_rev", [json!("")], None);
}

#[test]
fn non_updatable() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "nupd",
        CustomFilter::new(0, |_, (_, val)| Box::new(once(Ok(val)))),
    );

    yields(&defs, json!("hello"), "nupd", [json!("hello")], None);
    yields(
        &defs,
        json!("hello"),
        "nupd |= .",
        [],
        Some(Error::NonUpdatable),
    );
}

#[test]
fn arity0_and_update() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "with_length",
        CustomFilter::with_update(
            0,
            |_, (_, val)| Box::new(once(val.to_str().map(|s| Val::from(json!(s.len()))))),
            |_, (_, val), _| Box::new(once(val.to_str().map(|s| Val::from(json!(s.len()))))),
        ),
    );

    yields(&defs, json!("hello"), "with_length", [json!(5)], None);
    yields(&defs, json!("hello"), "with_length |= .", [json!(5)], None);
}

#[test]
fn arity1() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "iflonger",
        CustomFilter::new(1, |args, (ctx, val)| {
            let arg = match args[0].run((ctx.clone(), val.clone())).next() {
                Some(Ok(v)) => v,
                Some(Err(e)) => return Box::new(once(Err(e))),
                None => {
                    return Box::new(once(Err(Error::Custom(
                        "value expected but none found".into(),
                    ))))
                }
            };

            Box::new(once(
                val.to_str()
                    .and_then(|s| arg.as_int().map(|n| (s, n as usize)))
                    .map(|(s, n)| if s.len() > n { Val::Str(s) } else { Val::Null }),
            ))
        }),
    );

    yields(&defs, json!("hello"), "iflonger(6)", [Value::Null], None);
    yields(&defs, json!("hello"), "iflonger(3)", [json!("hello")], None);
}

#[test]
fn arity2() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "ifwithin",
        CustomFilter::new(2, |args, (ctx, val)| {
            let min = match args[0].run((ctx.clone(), val.clone())).next() {
                Some(Ok(v)) => match v.as_int() {
                    Ok(n) => n as usize,
                    Err(e) => return Box::new(once(Err(e))),
                },
                Some(Err(e)) => return Box::new(once(Err(e))),
                None => {
                    return Box::new(once(Err(Error::Custom(
                        "value expected but none found".into(),
                    ))))
                }
            };
            let max = match args[1].run((ctx.clone(), val.clone())).next() {
                Some(Ok(v)) => match v.as_int() {
                    Ok(n) => n as usize,
                    Err(e) => return Box::new(once(Err(e))),
                },
                Some(Err(e)) => return Box::new(once(Err(e))),
                None => {
                    return Box::new(once(Err(Error::Custom(
                        "value expected but none found".into(),
                    ))))
                }
            };

            Box::new(once(val.to_str().map(|s| {
                if s.len() >= min && s.len() <= max {
                    Val::Str(s)
                } else {
                    Val::Null
                }
            })))
        }),
    );

    yields(
        &defs,
        json!("hello"),
        "ifwithin(7; 11)",
        [Value::Null],
        None,
    );
    yields(
        &defs,
        json!("hello"),
        "ifwithin(3; 8)",
        [json!("hello")],
        None,
    );
}

#[test]
fn arity12() {
    let mut defs = Definitions::core();
    defs.insert_custom(
        "sillysum",
        CustomFilter::new(12, |args, (ctx, val)| {
            let mut nums = Vec::with_capacity(args.len());
            for arg in args {
                match arg.run((ctx.clone(), val.clone())).next() {
                    Some(Ok(v)) => match v.as_int() {
                        Ok(n) => nums.push(n),
                        Err(e) => return Box::new(once(Err(e))),
                    },
                    Some(Err(e)) => return Box::new(once(Err(e))),
                    None => {
                        return Box::new(once(Err(Error::Custom(
                            "value expected but none found".into(),
                        ))))
                    }
                };
            }

            Box::new(once(Ok(Val::Int(nums.into_iter().sum()))))
        }),
    );

    yields(
        &defs,
        Value::Null,
        "sillysum(6; 13; 15; 8; 10; 12; 20; 16; 20; 3; 17; 8)",
        [json!(148)],
        None,
    );
}
