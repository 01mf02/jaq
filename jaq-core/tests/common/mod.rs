use core::convert::TryFrom;
use jaq_core::{ClosedFilter, Error, Main, Val};
use serde_json::Value;
use std::rc::Rc;

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
    let to = |v| Rc::new(Val::from(v));
    let f = Main::parse(f).unwrap();
    let f = f.open(jaq_core::std()).unwrap();
    let f = ClosedFilter::try_from(f).unwrap();

    // TODO: remove cloned() starting from Rust 1.53
    let expected = ys.iter().cloned().map(|y| Ok(to(y)));
    let expected: Vec<_> = expected.chain(err.into_iter().map(Err)).collect();

    let out: Vec<_> = f.run(to(x)).collect();
    assert_eq!(out, expected);
}
