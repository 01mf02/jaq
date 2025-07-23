#![no_main]

use jaq_core::load::{Arena, File, Loader};
use jaq_core::{data, Compiler, Ctx, Vars};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let program = File {
        code: ".[]",
        path: (),
    };

    let loader = Loader::new([]);
    let arena = Arena::default();

    let modules = loader.load(&arena, program).unwrap();

    let filter = Compiler::default().compile(modules).unwrap();
    let ctx = Ctx::<data::JustLut<_>>::new(&filter.lut, Vars::new([]));
    let _ = filter.id.run((ctx, jaq_json::Val::from(data)));
});
