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
use jaq_core::{data, Compiler, Ctx, Vars};
use jaq_json::{write, Val, ValX};
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

    let unwrap_valx =
        |x: ValX| x.map_err(|e| e.unwrap_err_or_halt(|e| e, |code| std::process::exit(code)));
    // iterator over the output values
    for y in filter.id.run((ctx, Val::default())).map(unwrap_valx) {
        write::write(&mut stdout, &write::Pp::default(), 0, &y.unwrap())?;
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
