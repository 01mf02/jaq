pub use jaq_json::{Error, Val, ValR};
use serde_json::{from_value, Value};

fn yields(x: Val, code: &str, ys: impl Iterator<Item = ValR>) {
    use jaq_core::load::{Arena, File, Loader};
    eprintln!("{}", code.replace('\n', " "));

    let arena = Arena::default();
    let loader = Loader::new([]);
    let modules = loader.load(&arena, File { path: (), code }).unwrap();
    let filter = jaq_core::Compiler::default().compile(modules).unwrap();
    filter.yields(x, ys)
}

pub fn fail(x: Value, f: &str, err: Error) {
    yields(from_value(x).unwrap(), f, core::iter::once(Err(err)))
}

pub fn give(x: Value, f: &str, y: Value) {
    gives(x, f, [y])
}

pub fn gives<const N: usize>(x: Value, f: &str, ys: [Value; N]) {
    let conv = |v| from_value(v).unwrap();
    yields(conv(x), f, ys.into_iter().map(|y| Ok(conv(y))))
}

#[macro_export]
macro_rules! yields {
    ($func_name:ident, $filter:expr, $output:expr) => {
        #[test]
        fn $func_name() {
            $crate::common::give(json!(null), $filter, json!($output))
        }
    };
}
