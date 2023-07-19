use serde_json::Value;

fn yields(x: jaq_core::Val, f: &str, ys: impl Iterator<Item = jaq_core::ValR>) {
    let mut ctx = jaq_core::ParseCtx::new(Vec::new());
    ctx.insert_natives(jaq_native::core());
    ctx.insert_defs(jaq_std::std());

    let (f, errs) = jaq_parse::parse(f, jaq_parse::main());
    assert!(errs.is_empty());
    ctx.yields(x, f.unwrap(), ys)
}

pub fn fail(x: Value, f: &str, err: jaq_core::Error) {
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
