//! Native implementations of `inputs` and `input`.
use crate::{v, Filter};
use alloc::{boxed::Box, string::String};
use jaq_core::{Cv, DataT, Error, Exn, RunPtr, ValX};

/// Iterator over value results returned by the `inputs` filter.
pub type Inputs<'i, V> = &'i RcIter<dyn Iterator<Item = Result<V, String>> + 'i>;

/// A more flexible version of `&mut impl Iterator`.
pub struct RcIter<I: ?Sized>(core::cell::RefCell<I>);

impl<T, I: Iterator<Item = T> + ?Sized> Iterator for &RcIter<I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.0.borrow_mut().next()
    }
}

impl<I> RcIter<I> {
    /// Construct a new mutable iterator.
    pub const fn new(iter: I) -> Self {
        Self(core::cell::RefCell::new(iter))
    }
}

/// Global data that provides mutable access to input values.
pub trait HasInputs<'a, V> {
    /// Obtain the inputs from global data.
    fn inputs(&self) -> Inputs<'a, V>;
}

impl<'a, V> HasInputs<'a, V> for Inputs<'a, V> {
    fn inputs(&self) -> Inputs<'a, V> {
        self
    }
}

/// The `inputs` and `input` filters.
pub fn funs<D: for<'a> DataT<Data<'a>: HasInputs<'a, D::V<'a>>>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        ("inputs", v(0), |cv| Box::new(inputs(cv))),
        ("input", v(0), |cv| Box::new(inputs(cv).next().into_iter())),
    ])
}

fn inputs<'a, D: DataT<Data<'a>: HasInputs<'a, D::V<'a>>>>(
    cv: Cv<'a, D>,
) -> impl Iterator<Item = ValX<D::V<'a>>> + 'a {
    let inputs = cv.0.data().inputs();
    inputs.map(|r| r.map_err(|e| Exn::from(Error::str(e))))
}
