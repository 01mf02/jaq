use crate::{filter, run, write, Cli, Error, Val};
use jaq_core::{data, DataT, Lut, Native, RunPtr};
use jaq_std::input::{self, Inputs};
use jaq_std::{v, Filter};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct DataKind;

impl DataT for DataKind {
    type V<'a> = Val;
    type Data<'a> = &'a Data<'a>;
}

pub struct Data<'a> {
    lut: &'a Lut<DataKind>,
    inputs: Inputs<'a, Val>,
}

impl<'a> Data<'a> {
    pub fn new(lut: &'a Lut<DataKind>, inputs: Inputs<'a, Val>) -> Self {
        Self { lut, inputs }
    }
}

impl<'a> data::HasLut<'a, DataKind> for &'a Data<'a> {
    fn lut(&self) -> &'a Lut<DataKind> {
        self.lut
    }
}

impl<'a> input::HasInputs<'a, Val> for &'a Data<'a> {
    fn inputs(&self) -> Inputs<'a, Val> {
        self.inputs
    }
}

pub fn funs() -> impl Iterator<Item = Filter<Native<DataKind>>> {
    let input_funs = IntoIterator::into_iter(input::funs()).map(jaq_std::run);
    core::iter::once(jaq_std::run::<DataKind>(repl())).chain(input_funs)
}

/// counter that increases for each nested invocation of `repl`
static REPL_DEPTH: AtomicUsize = AtomicUsize::new(0);
/// immediately abort REPL if REPL_DEPTH + 1 == REPL_KILL_DEPTH
static REPL_KILL_DEPTH: AtomicUsize = AtomicUsize::new(0);

pub fn repl() -> Filter<RunPtr<DataKind>> {
    ("repl", v(0), |cv| {
        let depth = REPL_DEPTH.fetch_add(1, Ordering::Relaxed);
        repl_with(depth, |s| match eval(s, cv.1.clone()) {
            Ok(()) => (),
            Err(e) => eprint!("{e}"),
        })
        .unwrap();
        REPL_DEPTH.fetch_sub(1, Ordering::Relaxed);

        Box::new(core::iter::empty())
    })
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
    let history = dirs::cache_dir().map(|dir| dir.join("jaq-history"));
    let _ = history.iter().try_for_each(|h| rl.load_history(h));
    let prompt = format!("{}{} ", str::repeat("  ", depth), '>'.bold());
    let mut first = true;
    loop {
        use core::cmp::Ordering::{Equal, Greater, Less};
        match (depth + 1).cmp(&REPL_KILL_DEPTH.load(Ordering::Relaxed)) {
            Equal => break,
            // reset kill depth if we are below the level where REPL was killed
            Less => REPL_KILL_DEPTH.store(0, Ordering::Relaxed),
            // in this case, kill depth is zero; that is, inactive
            Greater => (),
        }
        match rl.readline(&prompt) {
            Ok(line) if line.chars().all(char::is_whitespace) => (),
            Ok(line) => f(line),
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => break,
            Err(err) => Err(err)?,
        }
        first = false;
    }
    // only kill subsequent REPLs at this depth if the first command was "^D" (EOF)
    if first {
        REPL_KILL_DEPTH.store(depth + 1, Ordering::Relaxed);
    }
    let _ = history.iter().try_for_each(|h| rl.append_history(h));
    Ok(())
}
