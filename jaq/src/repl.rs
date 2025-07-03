use crate::{filter, run, Cli, Error, Val};
use jaq_core::Native;
use jaq_std::Filter;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

pub fn fun() -> Filter<Native<Val>> {
    jaq_std::run(("repl", jaq_std::v(0), |_, cv| {
        repl_with(|s| match eval(s, cv.1.clone()) {
            Ok(()) => (),
            Err(e) => eprint!("{e}"),
        })
        .unwrap();
        Box::new(core::iter::empty())
    }))
}

fn eval(code: String, input: Val) -> Result<(), Error> {
    let (ctx, filter) =
        filter::parse_compile(&"<repl>".into(), &code, &[], &[]).map_err(Error::Report)?;
    let cli = &Cli::default();
    let inputs = core::iter::once(Ok(input));
    crate::with_stdout(|out| run(cli, &filter, ctx, inputs, |v| crate::print(out, cli, &v)))?;
    Ok(())
}

fn repl_with(f: impl Fn(String)) -> Result<(), ReadlineError> {
    use rustyline::config::{Behavior, Configurer};
    use yansi::Paint;
    let mut rl = DefaultEditor::new()?;
    rl.set_auto_add_history(true);
    rl.set_behavior(Behavior::PreferTerm);
    let prompt = "> ".bold().to_string();
    loop {
        match rl.readline(&prompt) {
            Ok(line) => f(line),
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => Err(err)?,
        }
    }
}
