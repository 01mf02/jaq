use crate::Runner;
use jaq_core::{data, DataT, Lut, Native};
use jaq_json::Val;
use jaq_std::input::{self, Inputs};

pub type Filter = jaq_core::Filter<DataKind>;
pub type Ctx<'a> = jaq_core::Ctx<'a, DataKind>;

pub struct DataKind;

impl DataT for DataKind {
    type V<'a> = Val;
    type Data<'a> = &'a Data<'a>;
}

pub struct Data<'a> {
    pub runner: &'a Runner,
    pub lut: &'a Lut<DataKind>,
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

pub fn funs() -> impl Iterator<Item = jaq_std::Filter<Native<DataKind>>> {
    let run = jaq_std::run::<DataKind>;
    let std = jaq_std::funs::<DataKind>();
    let input = input::funs::<DataKind>().into_vec().into_iter().map(run);
    std.chain(jaq_json::funs()).chain(input)
}
