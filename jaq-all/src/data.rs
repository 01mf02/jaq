//! Commonly used data for filter compilation & execution.
use crate::{compile_with, FileReports, Fun};
use jaq_core::{data, unwrap_valr, DataT, Lut, Vars};
use jaq_fmts::write::Writer;
use jaq_json::{Val, ValR};
use jaq_std::input::{self, Inputs, RcIter};

/// Filter for given kind of data.
pub type Filter = jaq_core::Filter<DataKind>;

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

impl Runner {
    /// Use colors on standard output?
    pub fn color_stdout(&self) -> bool {
        !self.writer.pp.styles.reset.is_empty()
    }
}

/// Functions from [`jaq_std`] and [`jaq_json`].
pub fn base_funs() -> impl Iterator<Item = Fun<DataKind>> {
    let run = jaq_std::run::<DataKind>;
    let core = jaq_core::funs::<DataKind>();
    let std = jaq_std::funs::<DataKind>();
    let input = input::funs::<DataKind>().into_vec().into_iter().map(run);
    core.chain(std).chain(jaq_json::funs()).chain(input)
}

/// Base functions ([`base_funs`]) plus functions from [`jaq_fmts`].
#[cfg(feature = "formats")]
pub fn funs() -> impl Iterator<Item = Fun<DataKind>> {
    base_funs().chain(jaq_fmts::funs())
}

/// Compile a filter without access to external files/variables, including all functions/definitions.
#[cfg(feature = "formats")]
pub fn compile(code: &str) -> Result<Filter, Vec<FileReports>> {
    compile_with(code, jaq_core::defs().chain(jaq_std::defs()).chain(jaq_json::defs()), funs(), &[])
}

/// Run a filter with given input values and run `f` for every value output.
///
/// This function cannot return an `Iterator` because it creates an `RcIter`.
/// This is most unfortunate. We should think about how to simplify this ...
pub fn run<E>(
    runner: &Runner,
    filter: &Filter,
    vars: Vars<Val>,
    inputs: impl Iterator<Item = Result<Val, impl ToString>>,
    fi: impl Fn(String) -> E,
    mut f: impl FnMut(ValR) -> Result<(), E>,
) -> Result<(), E> {
    let inputs = Box::new(inputs.map(|r| r.map_err(|e| e.to_string())));
    let null = Box::new(core::iter::once(Ok(Val::Null)));

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
