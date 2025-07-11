#![no_main]

use jaq_core::load::{Arena, File, Loader};
use jaq_core::{Compiler, Native, Vars};

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

    let _ = filter.run(Vars::new([]), &(), jaq_json::Val::from(data));
});
