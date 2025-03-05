#![no_main]

libfuzzer_sys::fuzz_target!(|tokens: Vec<jaq_core::load::lex::Token<&str>>| {
    //println!("{tokens:?}");
    let parser = || jaq_core::load::Parser::new(tokens.as_slice());
    let _ = parser().parse(|p| p.defs());
    let _ = parser().parse(|p| p.term());
});
