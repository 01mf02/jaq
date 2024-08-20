use jaq_json::{Error, Val, ValR};
use serde_json::Value;

fn yields(x: Val, code: &str, ys: impl Iterator<Item = ValR>) {
    use jaq_core::load::{Arena, File, Loader};
    use jaq_core::{Compiler, Native};

    let arena = Arena::default();
    let loader = Loader::new([]);
    let modules = loader.load(&arena, File { path: "", code }).unwrap();
    let filter = Compiler::<_, Native<_>>::default()
        .compile(modules)
        .unwrap();
    filter.yields(x, ys)
}

pub fn fail(x: Value, f: &str, err: Error) {
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
    ($func_name:ident, $filter:expr, $output:expr) => {
        #[test]
        fn $func_name() {
            give(json!(null), $filter, json!($output))
        }
    };
}
