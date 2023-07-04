use jaq_core::{Ctx, Error, FilterT, ParseCtx, RcIter, Val};
use serde_json::Value;

pub fn give(x: Value, f: &str, y: Value) {
    gives(x, f, [y])
}

pub fn gives<const N: usize>(x: Value, f: &str, ys: [Value; N]) {
    yields(x, f, ys, None)
}

pub fn fail(x: Value, f: &str, err: Error) {
    fails(x, f, [], err)
}

pub fn fails<const N: usize>(x: Value, f: &str, ys: [Value; N], err: Error) {
    yields(x, f, ys, Some(err))
}

pub fn yields<const N: usize>(x: Value, f: &str, ys: [Value; N], err: Option<Error>) {
    let mut defs = ParseCtx::new(Vec::new());
    defs.insert_core();
    let f = defs.parse_filter(f);
    assert_eq!(defs.errs, Vec::new());

    let to = |v| Val::from(v);

    let expected = ys.into_iter().map(|y| Ok(to(y)));
    let expected: Vec<_> = expected.chain(err.into_iter().map(Err)).collect();

    let inputs = RcIter::new(core::iter::empty());
    let out: Vec<_> = f.run((Ctx::new([], &inputs), to(x))).collect();
    assert_eq!(out, expected);
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
