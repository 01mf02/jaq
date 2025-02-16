#![no_main]

use jaq_core::{
    load::{Arena, File, Loader},
    Compiler, Ctx, RcIter,
};

use jaq_json::Val;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let program = File {
        code: ".[]",
        path: (),
    };

    let loader = Loader::new(jaq_std::defs());
    let arena = Arena::default();

    let modules = loader.load(&arena, program).unwrap();

    let filter = Compiler::default()
        .with_funs(jaq_std::funs::<jaq_json::Val>())
        .compile(modules)
        .unwrap();

    let inputs = RcIter::new(core::iter::empty());
    let _ = filter.run((Ctx::new([], &inputs), Val::from(data)));
});
