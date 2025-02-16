#![no_main]

use jaq_core::{
    load::{Arena, File, Loader},
    Compiler,
};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    if data.contains("\"") || data.contains("main") {
        return;
    }

    let program = File {
        code: data,
        path: (),
    };

    let loader = Loader::new(jaq_std::defs());
    let arena = Arena::default();

    match loader.load(&arena, program) {
        Ok(modules) => {
            let _ = Compiler::default()
                .with_funs(jaq_std::funs::<jaq_json::Val>())
                .compile(modules);
        }
        _ => {}
    }
});
