//! Minimalistic REPL for jaq.
//!
//! This does not provide any filters from the standard library,
//! not even very basic ones such as `length`.
//! Note that jaq has a much nicer, in-built REPL via its `repl` filter.
//!
//! For history etc., consider running this with:
//!
//!     rlwrap cargo run --example repl

use jaq_core::load::{Arena, File, Loader};
use jaq_core::{data, unwrap_valr, Compiler, Ctx, Vars};
use jaq_json::Val;
use std::io::{stdin, stdout, Write};

fn eval_print(code: &str) -> std::io::Result<()> {
    let loader = Loader::new([]);
    let arena = Arena::default();

    // parse the filter
    let modules = loader.load(&arena, File { code, path: () }).unwrap();

    // compile the filter
    let filter = Compiler::default().compile(modules).unwrap();

    // context for filter execution
    let ctx = Ctx::<data::JustLut<Val>>::new(&filter.lut, Vars::new([]));

    let mut stdout = stdout().lock();

    // iterator over the output values
    for y in filter.id.run((ctx, Val::default())).map(unwrap_valr) {
        jaq_json::json::write(&mut stdout, &y.unwrap())?;
        writeln!(stdout)?;
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let mut lines = stdin().lines();
    loop {
        print!(">>> ");
        stdout().flush()?;
        match lines.next() {
            Some(line) => eval_print(&line?)?,
            None => break,
        }
    }
    println!();
    Ok(())
}
