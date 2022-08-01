use jaq_core::{parse, Ctx, Definitions, Error, RcIter, Val};
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
    let mut defs = Definitions::core();
    let mut errs = Vec::new();
    jaq_std::std()
        .into_iter()
        .for_each(|def| defs.insert(def, &mut errs));
    let f = parse::parse(&f, parse::main()).0.unwrap();
    let f = defs.finish(f, Vec::new(), &mut errs);
    assert_eq!(errs, Vec::new());

    let to = |v| Val::from(v);

    let expected = ys.into_iter().map(|y| Ok(to(y)));
    let expected: Vec<_> = expected.chain(err.into_iter().map(Err)).collect();

    let inputs = RcIter::new(core::iter::empty());
    let out: Vec<_> = f.run(Ctx::new([], &inputs), to(x)).collect();
    assert_eq!(out, expected);
}
