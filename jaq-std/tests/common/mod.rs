use serde_json::Value;

#[cfg(not(feature = "unstable-flag"))]
pub const UNSTABLE: bool = false;
#[cfg(feature = "unstable-flag")]
pub const UNSTABLE: bool = false;

#[track_caller]
fn yields(
    #[cfg_attr(not(feature = "unstable-flag"), allow(unused_variables))] unstable: bool,
    x: jaq_interpret::Val,
    f: &str,
    ys: impl Iterator<Item = jaq_interpret::ValR>,
) {
    let mut ctx = jaq_interpret::ParseCtx::new(Vec::new());
    ctx.insert_natives(jaq_core::core(
        #[cfg(feature = "unstable-flag")]
        unstable,
    ));
    ctx.insert_defs(jaq_std::std(
        #[cfg(feature = "unstable-flag")]
        unstable,
    ));

    let (f, errs) = jaq_parse::parse(
        #[cfg(feature = "unstable-flag")]
        unstable,
        f,
        jaq_parse::main(
            #[cfg(feature = "unstable-flag")]
            unstable,
        ),
    );
    assert!(errs.is_empty());
    ctx.yields(x, f.unwrap(), ys)
}

#[track_caller]
pub fn fail(unstable: bool, x: Value, f: &str, err: jaq_interpret::Error) {
    yields(unstable, x.into(), f, core::iter::once(Err(err)))
}

#[track_caller]
pub fn give(unstable: bool, x: Value, f: &str, y: Value) {
    yields(unstable, x.into(), f, core::iter::once(Ok(y.into())))
}

#[track_caller]
pub fn gives<const N: usize>(unstable: bool, x: Value, f: &str, ys: [Value; N]) {
    yields(unstable, x.into(), f, ys.into_iter().map(|y| Ok(y.into())))
}

#[macro_export]
macro_rules! yields {
    ($func_name:ident, $unstable:expr, $filter:expr, $output: expr) => {
        #[test]
        fn $func_name() {
            give($unstable, json!(null), $filter, json!($output))
        }
    };
}
