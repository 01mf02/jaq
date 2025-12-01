//! Filter parsing, compilation, and execution.
use crate::{funs, read, Error, Runner, Val};
use jaq_bla::data::Filter;
use jaq_bla::load::{compile_errors, load_errors, FileReports};
use jaq_core::{compile, load, ValT, Vars};
use std::{io, path::PathBuf};

pub fn parse_compile(
    path: &PathBuf,
    code: &str,
    vars: &[String],
    paths: &[PathBuf],
) -> Result<(Vec<Val>, Filter), Vec<FileReports<PathBuf>>> {
    use compile::Compiler;
    use load::{import, Arena, File, Loader};

    let default = ["~/.jq", "$ORIGIN/../lib/jq", "$ORIGIN/../lib"].map(|x| x.into());
    let paths = if paths.is_empty() { &default } else { paths };

    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let defs = jaq_std::defs().chain(jaq_json::defs());
    let loader = Loader::new(defs).with_std_read(paths);
    //let loader = Loader::new([]).with_std_read(paths);
    let path = path.into();
    let modules = loader
        .load(&arena, File { path, code })
        .map_err(load_errors)?;

    let mut vals = Vec::new();
    import(&modules, |p| {
        let path = p.find(paths, "json")?;
        vals.push(read::json_array(path).map_err(|e| e.to_string())?);
        Ok(())
    })
    .map_err(load_errors)?;

    let compiler = Compiler::default()
        .with_funs(funs::funs())
        .with_global_vars(vars.iter().map(|v| &**v));
    let filter = compiler.compile(modules).map_err(compile_errors)?;
    Ok((vals, filter))
}

/// Run a filter with given input values and run `f` for every value output.
pub(crate) fn run(
    runner: &Runner,
    filter: &Filter,
    vars: Vars<Val>,
    inputs: impl Iterator<Item = io::Result<Val>>,
    mut f: impl FnMut(Val) -> io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    let inputs = inputs.map(|r| r.map_err(|e| e.to_string()));
    let f = |v: jaq_json::ValR| {
        let v = v.map_err(Error::Jaq)?;
        last = Some(v.as_bool());
        f(v).map_err(Into::into)
    };
    jaq_bla::data::run(runner, filter, vars, inputs, Error::Parse, f)?;
    Ok(last)
}
