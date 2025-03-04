#![no_main]

use jaq_core::load::{Arena, File, Loader};
use jaq_core::Compiler;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|code: &str| {
    if code.contains("\"") || code.contains("main") {
        return;
    }

    let program = File { code, path: () };
    let loader = Loader::new(jaq_std::defs());
    let arena = Arena::default();

    if let Ok(modules) = loader.load(&arena, program) {
        let _ = Compiler::default()
            .with_funs(jaq_std::funs::<jaq_json::Val>())
            .compile(modules);
    }
});
