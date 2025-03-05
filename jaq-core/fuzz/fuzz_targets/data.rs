#![no_main]

use jaq_core::load::{Arena, File, Loader};
use jaq_core::{Compiler, Ctx, Native, RcIter};

use jaq_json::Val;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let program = File {
        code: ".[]",
        path: (),
    };

    let loader = Loader::new([]);
    let arena = Arena::default();

    let modules = loader.load(&arena, program).unwrap();

    let filter = Compiler::<_, Native<_>>::default()
        .compile(modules)
        .unwrap();

    let inputs = RcIter::new(core::iter::empty());
    let _ = filter.run((Ctx::new([], &inputs), Val::from(data)));
});
