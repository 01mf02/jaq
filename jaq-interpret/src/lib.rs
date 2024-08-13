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
//! ~~~
//! use jaq_interpret::{Ctx, Error, FilterT, Compiler, RcIter, Val};
//! use serde_json::{json, Value};
//!
//! let input = json!(["Hello", "world"]);
//! let code = ".[]";
//!
//! use jaq_syn::load::{Arena, File, Loader};
//!
//! // start out only from core filters,
//! // which do not include filters in the standard library
//! // such as `map`, `select` etc.
//! let loader = Loader::new([]);
//! let arena = Arena::default();
//!
//! // parse the filter
//! let modules = loader.load(&arena, File { path: "", code }).unwrap();
//!
//! // compile the filter
//! let filter = jaq_interpret::Compiler::default().compile(modules).unwrap();
//! let filter = filter.with_funs([]);
//!
//! let inputs = RcIter::new(core::iter::empty());
//!
//! // iterator over the output values
//! let mut out = filter.run((Ctx::new([], &inputs), Val::from(input)));
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

mod box_iter;
pub mod compile;
pub mod error;
mod filter;
mod into_iter;
mod path;
mod rc_iter;
mod rc_lazy_list;
mod rc_list;
pub mod results;
mod stack;
mod val;

#[allow(dead_code)]
mod exn;

pub use compile::{Compiler, Filter};
pub use error::Error;
pub use filter::{Args, FilterT, Native, RunPtr, UpdatePtr};
pub use rc_iter::RcIter;
pub use val::{Val, ValR, ValRs, ValT};

use alloc::string::String;
use jaq_syn::Arg as Bind;
use rc_list::List as RcList;
use stack::Stack;

/// variable bindings
#[derive(Clone, Debug, PartialEq, Eq)]
struct Vars<V>(RcList<Bind<V, (filter::Id, Self)>>);
type Inputs<'i, V> = RcIter<dyn Iterator<Item = Result<V, String>> + 'i>;

impl<V> Vars<V> {
    fn get(&self, i: usize) -> Option<&Bind<V, (filter::Id, Self)>> {
        self.0.get(i)
    }
}

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a, V = Val> {
    vars: Vars<V>,
    inputs: &'a Inputs<'a, V>,
}

impl<'a, V> Ctx<'a, V> {
    /// Construct a context.
    pub fn new(vars: impl IntoIterator<Item = V>, inputs: &'a Inputs<'a, V>) -> Self {
        let vars = Vars(RcList::new().extend(vars.into_iter().map(Bind::Var)));
        Self { vars, inputs }
    }

    /// Add a new variable binding.
    pub(crate) fn cons_var(mut self, x: V) -> Self {
        self.vars.0 = self.vars.0.cons(Bind::Var(x));
        self
    }

    /// Add a new filter binding.
    pub(crate) fn cons_fun(mut self, (f, ctx): (filter::Id, Self)) -> Self {
        self.vars.0 = self.vars.0.cons(Bind::Fun((f, ctx.vars)));
        self
    }

    /// Remove the `skip` most recent variable bindings.
    fn skip_vars(mut self, skip: usize) -> Self {
        if skip > 0 {
            self.vars.0 = self.vars.0.skip(skip).clone();
        }
        self
    }

    fn with_vars(&self, vars: Vars<V>) -> Self {
        let inputs = self.inputs;
        Self { vars, inputs }
    }

    /// Return remaining input values.
    pub fn inputs(&self) -> &'a Inputs<'a, V> {
        self.inputs
    }
}

impl<V: ValT> Filter<Native<V>> {
    /// Run a filter on given input, panic if it does not yield the given output.
    ///
    /// This is for testing purposes.
    pub fn yields(&self, x: V, ys: impl Iterator<Item = val::ValR2<V>>) {
        let inputs = RcIter::new(core::iter::empty());
        let out = self.run((Ctx::new([], &inputs), x));
        assert!(out.eq(ys));
    }
}
