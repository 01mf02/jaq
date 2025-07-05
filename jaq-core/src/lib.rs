//! JSON query language interpreter.
//!
//! This crate allows you to execute jq-like filters.
//!
//! The example below demonstrates how to use this crate.
//! See the implementation in the `jaq` crate if you are interested in how to:
//!
//! * enable usage of the standard library,
//! * load JSON files lazily,
//! * handle errors etc.
//!
//! (This example requires enabling the `serde_json` feature for `jaq-json`.)
//!
//! ~~~
//! use jaq_core::{load, Compiler, Ctx, Error, RcIter, Vars};
//! use jaq_json::Val;
//! use serde_json::{json, Value};
//!
//! let input = json!(["Hello", "world"]);
//! let program = File { code: ".[]", path: () };
//!
//! use load::{Arena, File, Loader};
//!
//! let loader = Loader::new(jaq_std::defs().chain(jaq_json::defs()));
//! let arena = Arena::default();
//!
//! // parse the filter
//! let modules = loader.load(&arena, program).unwrap();
//!
//! // compile the filter
//! let filter = jaq_core::Compiler::default()
//!     .with_funs(jaq_std::funs().chain(jaq_json::funs()))
//!     .compile(modules)
//!     .unwrap();
//!
//! let inputs = RcIter::new(core::iter::empty());
//!
//! // iterator over the output values
//! let mut out = filter.run(Vars::new([]), &inputs, Val::from(input));
//!
//! assert_eq!(out.next(), Some(Ok(Val::from(json!("Hello")))));;
//! assert_eq!(out.next(), Some(Ok(Val::from(json!("world")))));;
//! assert_eq!(out.next(), None);;
//! ~~~
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub mod box_iter;
pub mod compile;
mod exn;
mod filter;
mod fold;
mod into_iter;
pub mod load;
pub mod ops;
pub mod path;
mod rc_iter;
mod rc_lazy_list;
mod rc_list;
mod stack;
pub mod val;

pub use compile::Compiler;
pub use exn::{Error, Exn};
pub use filter::{Ctx, Cv, Native, PathsPtr, RunPtr, UpdatePtr, Vars};
pub use rc_iter::RcIter;
pub use val::{ValR, ValT, ValX, ValXs};

use alloc::rc::Rc;
use alloc::string::String;
use rc_list::List as RcList;
use stack::Stack;

/// Iterator over value results returned by the `inputs` filter.
pub type Inputs<'i, V> = &'i RcIter<dyn Iterator<Item = Result<V, String>> + 'i>;

/// Argument of a definition, such as `$v` or `f` in `def foo($v; f): ...`.
///
/// In jq, we can bind filters in three different ways:
///
/// 1. `f as $x | ...`
/// 2. `def g($x): ...; g(f)`
/// 3. `def g(fx): ...; g(f)`
///
/// In the first two cases, we bind the outputs of `f` to a variable `$x`.
/// In the third case, we bind `f` to a filter `fx`
///
/// When writing a native filter, this is used to declare its arguments.
/// It is passed to [`compile::Compiler::with_funs`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bind<V = (), F = V> {
    /// binding to a variable
    Var(V),
    /// binding to a filter
    Fun(F),
}

impl<V, F> Bind<V, F> {
    /// Move references inward.
    pub(crate) fn as_ref(&self) -> Bind<&V, &F> {
        match self {
            Self::Var(x) => Bind::Var(x),
            Self::Fun(x) => Bind::Fun(x),
        }
    }
}

impl<T> Bind<T, T> {
    /// Apply a function to both binding types.
    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> Bind<U, U> {
        match self {
            Self::Var(x) => Bind::Var(f(x)),
            Self::Fun(x) => Bind::Fun(f(x)),
        }
    }
}

/// Function from a value to a stream of value results.
#[derive(Debug, Clone)]
pub struct Filter<F>(compile::TermId, Rc<compile::Lut<F>>);

impl<V: ValT> Filter<Native<V>> {
    /// Run a filter on given input, yielding output values.
    pub fn run<'i>(
        &self,
        vars: Vars<V>,
        inputs: Inputs<'i, V>,
        v: V,
    ) -> impl Iterator<Item = ValR<V>> + 'i {
        self.0
            .run((Ctx::new(self.1.clone(), vars, inputs), v))
            .map(|v| v.map_err(|e| e.get_err().ok().unwrap()))
    }

    /// Run a filter on given input, panic if it does not yield the given output.
    ///
    /// This is for testing purposes.
    pub fn yields(&self, x: V, ys: impl Iterator<Item = ValR<V>>) {
        let inputs = RcIter::new(core::iter::empty());
        let out = self.run(Vars::new([]), &inputs, x);
        assert!(out.eq(ys));
    }
}
