//! Tools to help with native filter construction.

use crate::box_iter::box_once;
use crate::{Bind, DataT, Exn, Native, PathsPtr, RunPtr, ValR, ValXs};
use alloc::boxed::Box;

/// Name, arguments, and implementation of a filter.
pub type Filter<F> = (&'static str, Box<[Bind]>, F);

/// Convert a filter with a run pointer to a native filter.
pub fn run<D: DataT>((name, arity, run): Filter<RunPtr<D>>) -> Filter<Native<D>> {
    (name, arity, Native::new(run))
}

pub(crate) type RunPathsPtr<D> = (RunPtr<D>, PathsPtr<D>);

/// Convert a filter with a run and a paths pointer to a native filter.
pub(crate) fn paths<D: DataT>(
    (name, arity, (run, paths)): Filter<RunPathsPtr<D>>,
) -> Filter<Native<D>> {
    (name, arity, Native::new(run).with_paths(paths))
}

/// Box Once and Map Errors to exceptions.
pub fn bome<'a, V: 'a>(r: ValR<V>) -> ValXs<'a, V> {
    box_once(r.map_err(Exn::from))
}

/// Creates `n` variable arguments.
pub fn v(n: usize) -> Box<[Bind]> {
    core::iter::repeat(Bind::Var(())).take(n).collect()
}
