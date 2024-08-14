use serde_json::Value;

fn yields(x: jaq_interpret::Val, code: &str, ys: impl Iterator<Item = jaq_interpret::ValR>) {
    use jaq_syn::load::{Arena, File, Loader};

    let arena = Arena::default();
    let loader = Loader::new([]);
    let modules = loader.load(&arena, File { path: "", code }).unwrap();
    let filter = jaq_interpret::Compiler::default().compile(modules).unwrap();
    filter.with_funs([]).yields(x, ys)
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
    ($func_name:ident, $filter:expr, $output:expr) => {
        #[test]
        fn $func_name() {
            give(json!(null), $filter, json!($output))
        }
    };
}
