//! Filter parsing, compilation, and execution.
use crate::{funs, read, Cli, Error, Val};
use jaq_bla::{compile_errors, load_errors, FileReports};
use jaq_core::{compile, load, unwrap_valr, ValT, Vars};
use jaq_std::input::RcIter;
use std::{io, path::PathBuf};

pub type Filter = jaq_core::Filter<funs::DataKind>;
pub type Ctx<'a> = jaq_core::Ctx<'a, funs::DataKind>;

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
///
/// This function cannot return an `Iterator` because it creates an `RcIter`.
/// This is most unfortunate. We should think about how to simplify this ...
pub(crate) fn run(
    cli: &Cli,
    filter: &Filter,
    vars: Vec<Val>,
    iter: impl Iterator<Item = io::Result<Val>>,
    mut f: impl FnMut(Val) -> io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    let iter = iter.map(|r| r.map_err(|e| e.to_string()));

    let iter = Box::new(iter) as Box<dyn Iterator<Item = _>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let iter = &RcIter::new(iter);
    let null = &RcIter::new(null);

    let vars = Vars::new(vars);

    for item in if cli.null_input { null } else { iter } {
        let data = funs::Data::new(cli, &filter.lut, iter);
        let ctx = Ctx::new(&data, vars.clone());
        let input = item.map_err(Error::Parse)?;
        //println!("Got {:?}", input);
        for output in filter.id.run((ctx.clone(), input)) {
            let output = unwrap_valr(output).map_err(Error::Jaq)?;
            last = Some(output.as_bool());
            f(output)?;
        }
    }
    Ok(last)
}
