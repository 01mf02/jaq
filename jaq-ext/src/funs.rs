use jaq_core::{DataT, Native};
use jaq_std::Filter;
use jaq_json::Val;
use crate::data::DataKind;
use crate::{read, write};

/// Functions from [`jaq_std`], [`jaq_json`], and (de-)serialisation for various formats.
pub fn funs() -> impl Iterator<Item = Filter<Native<DataKind>>> {
    let run = jaq_std::run::<DataKind>;
    let std = jaq_std::funs::<DataKind>();
    let input = jaq_std::input::funs::<DataKind>().into_vec().into_iter().map(run);
    std.chain(jaq_json::funs())
        .chain(input)
        .chain(rw_funs())
}

/// (De-)Serialisation filters.
pub fn rw_funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Filter<Native<D>>>
{
    [read::funs::<D>(), write::funs::<D>()]
        .into_iter()
        .flat_map(move |funs| funs.into_vec().into_iter().map(jaq_std::run::<D>))
}
