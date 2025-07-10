use crate::{filter, run, write, Cli, Error, Val};
use jaq_core::{DataT, Native, RunPtr};
use jaq_std::{inputs, v, Filter, HasInputs, Inputs};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Data;

impl DataT for Data {
    type Data<'a> = DataData<'a>;
}

pub struct DataData<'a> {
    inputs: Inputs<'a, Val>,
    /// counter that increases for each nested invocation of `repl`
    repl_depth: AtomicUsize,
}

impl<'a> DataData<'a> {
    pub fn new(inputs: Inputs<'a, Val>) -> Self {
        Self {
            inputs,
            repl_depth: AtomicUsize::new(0),
        }
    }
}

pub trait HasRepl {
    fn repl_depth(&self) -> &AtomicUsize;
}

impl HasRepl for DataData<'_> {
    fn repl_depth(&self) -> &AtomicUsize {
        &self.repl_depth
    }
}

impl<'a> HasInputs<'a, Val> for DataData<'a> {
    fn inputs(&self) -> Inputs<'a, Val> {
        self.inputs
    }
}

pub fn funs() -> Box<[Filter<Native<Val, Data>>]> {
    [repl(), inputs()].map(jaq_std::run).into()
}

pub fn repl<D: for<'a> DataT<Data<'a>: HasRepl>>() -> Filter<RunPtr<Val, D>> {
    ("repl", v(0), |cv| {
        let depth = cv.0.data().repl_depth().fetch_add(1, Ordering::Relaxed);
        repl_with(depth, |s| match eval(s, cv.1.clone()) {
            Ok(()) => (),
            Err(e) => eprint!("{e}"),
        })
        .unwrap();
        cv.0.data().repl_depth().fetch_sub(1, Ordering::Relaxed);

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
    loop {
        match rl.readline(&prompt) {
            Ok(line) => f(line),
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => break,
            Err(err) => Err(err)?,
        }
    }
    let _ = history.iter().try_for_each(|h| rl.append_history(h));
    Ok(())
}
