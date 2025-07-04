use crate::{filter, run, write, Cli, Error, Val};
use jaq_core::Native;
use jaq_std::Filter;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::sync::atomic::{AtomicUsize, Ordering};

static DEPTH: AtomicUsize = AtomicUsize::new(0);

pub fn fun() -> Filter<Native<Val>> {
    jaq_std::run(("repl", jaq_std::v(0), |_, cv| {
        let depth = DEPTH.fetch_add(1, Ordering::Relaxed);
        repl_with(depth, |s| match eval(s, cv.1.clone()) {
            Ok(()) => (),
            Err(e) => eprint!("{e}"),
        })
        .unwrap();
        DEPTH.fetch_sub(1, Ordering::Relaxed);

        Box::new(core::iter::empty())
    }))
}

fn eval(code: String, input: Val) -> Result<(), Error> {
    let (ctx, filter) =
        filter::parse_compile(&"<repl>".into(), &code, &[], &[]).map_err(Error::Report)?;
    let cli = &Cli::default();
    let inputs = core::iter::once(Ok(input));
    crate::with_stdout(|out| run(cli, &filter, ctx, inputs, |v| write::print(out, cli, &v)))?;
    Ok(())
}

fn repl_with(depth: usize, f: impl Fn(String)) -> Result<(), ReadlineError> {
    use rustyline::config::{Behavior, Config};
    use yansi::Paint;
    let config = Config::builder()
        .behavior(Behavior::PreferTerm)
        .auto_add_history(true)
        .build();
    let mut rl = DefaultEditor::with_config(config)?;
    let prompt = format!("{}{} ", str::repeat("  ", depth), '>'.bold());
    loop {
        match rl.readline(&prompt) {
            Ok(line) => f(line),
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => Err(err)?,
        }
    }
}
