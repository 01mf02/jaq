//! Commonly used data for filter execution.
use crate::load::{compile_errors, load_errors, FileReports};
use jaq_core::load::{import, parse::Def, Arena, File, Loader};
use jaq_core::{compile::Compiler, data, unwrap_valr, DataT, Lut, Vars};
use jaq_json::{write::Pp, Val};
use jaq_std::input::{self, Inputs, RcIter};

/// Filter for given kind of data.
pub type Filter<D = DataKind> = jaq_core::Filter<D>;

/// Execution context for given kind of data.
pub type Ctx<'a> = jaq_core::Ctx<'a, DataKind>;

/// Kind of data.
pub struct DataKind;

impl DataT for DataKind {
    type V<'a> = Val;
    type Data<'a> = &'a Data<'a>;
}

/// The actual data.
pub struct Data<'a> {
    /// run options
    pub runner: &'a Runner,
    /// look-up table
    pub lut: &'a Lut<DataKind>,
    /// input values
    pub inputs: Inputs<'a, Val>,
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

/// Run options.
#[derive(Default)]
pub struct Runner {
    /// pass `null` as input
    pub null_input: bool,
    /// use colors in error messages
    pub color_err: bool,
    /// write options
    pub writer: Writer,
}

/// Write options.
#[derive(Default)]
pub struct Writer {
    /// output format
    pub format: crate::Format,
    /// pretty printer
    pub pp: Pp,
    /// concatenate outputs without newline
    pub join: bool,
}

impl Runner {
    /// Use colors on standard output?
    pub fn color_stdout(&self) -> bool {
        !self.writer.pp.colors.reset.is_empty()
    }
}

#[cfg(feature = "formats")]
pub fn compile<P: Clone + Default + Eq>(code: &str) -> Result<Filter, Vec<FileReports<P>>> {
    let defs = jaq_std::defs().chain(jaq_json::defs());
    let funs = crate::base_funs().chain(crate::rw_funs());
    compile_with(code, defs, funs, &[])
}

/// Compile a filter without access to external files.
pub fn compile_with<P: Clone + Default + Eq, D: DataT>(
    code: &str,
    defs: impl Iterator<Item = Def>,
    funs: impl Iterator<Item = crate::Fun<D>>,
    vars: &[String],
) -> Result<Filter<D>, Vec<FileReports<P>>> {
    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let loader = Loader::new(defs);
    let path = P::default();
    let modules = loader
        .load(&arena, File { path, code })
        .map_err(load_errors)?;

    import(&modules, |_path| Err("file loading not supported".into())).map_err(load_errors)?;

    Compiler::default()
        .with_funs(funs)
        .with_global_vars(vars.iter().map(|v| &**v))
        .compile(modules)
        .map_err(compile_errors)
}

/// Run a filter with given input values and run `f` for every value output.
///
/// This function cannot return an `Iterator` because it creates an `RcIter`.
/// This is most unfortunate. We should think about how to simplify this ...
pub fn run<E>(
    runner: &Runner,
    filter: &Filter,
    vars: Vars<Val>,
    inputs: impl Iterator<Item = Result<Val, String>>,
    fi: impl Fn(String) -> E,
    mut f: impl FnMut(jaq_core::ValR<Val>) -> Result<(), E>,
) -> Result<(), E> {
    let inputs = Box::new(inputs) as Box<dyn Iterator<Item = _>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let null = &RcIter::new(null);

    let data = Data {
        runner,
        lut: &filter.lut,
        inputs: &RcIter::new(inputs),
    };
    let ctx = Ctx::new(&data, vars);

    let outputs = |x| filter.id.run((ctx.clone(), x));
    (if runner.null_input { null } else { data.inputs }).try_for_each(|x| match x {
        Ok(x) => outputs(x).try_for_each(|y| f(unwrap_valr(y))),
        Err(e) => Err(fi(e)),
    })
}
