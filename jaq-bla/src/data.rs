//! Commonly used data for filter execution.
use crate::Format;
use jaq_core::{data, DataT, Lut, Native};
use jaq_json::{write::Pp, Val};
use jaq_std::input::{self, Inputs};

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

/// Write options.
#[derive(Default)]
pub struct Writer {
    /// output format
    pub format: Format,
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

/// Functions from [`jaq_std`] and [`jaq_json`].
pub fn funs() -> impl Iterator<Item = jaq_std::Filter<Native<DataKind>>> {
    let run = jaq_std::run::<DataKind>;
    let std = jaq_std::funs::<DataKind>();
    let input = input::funs::<DataKind>().into_vec().into_iter().map(run);
    std.chain(jaq_json::funs()).chain(input)
}
