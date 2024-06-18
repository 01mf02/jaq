// SPDX-FileCopyrightText: 2021 Michael Färber
//
// SPDX-License-Identifier: MIT

use serde_json::Value;

fn yields(x: jaq_interpret::Val, f: &str, ys: impl Iterator<Item = jaq_interpret::ValR>) {
    let mut ctx = jaq_interpret::ParseCtx::new(Vec::new());
    ctx.insert_natives(jaq_core::core());

    let (f, errs) = jaq_parse::parse(f, jaq_parse::main());
    assert!(errs.is_empty());
    ctx.yields(x, f.unwrap(), ys)
}

pub fn fail(x: Value, f: &str, err: jaq_interpret::Error) {
    yields(x.into(), f, core::iter::once(Err(err)))
}

pub fn give(x: Value, f: &str, y: Value) {
    yields(x.into(), f, core::iter::once(Ok(y.into())))
}

pub fn gives<const N: usize>(x: Value, f: &str, ys: [Value; N]) {
    yields(x.into(), f, ys.into_iter().map(|y| Ok(y.into())))
}

#[macro_export]
macro_rules! yields {
    ($func_name:ident, $filter:expr, $output: expr) => {
        #[test]
        fn $func_name() {
            give(json!(null), $filter, json!($output))
        }
    };
}
