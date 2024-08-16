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
//! use jaq_interpret::{Compiler, Ctx, Error, FilterT, Native, RcIter, Val, run};
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
//! let filter = jaq_interpret::Compiler::<_, Native<_>>::default()
//!     .compile(modules)
//!     .unwrap();
//!
//! let inputs = RcIter::new(core::iter::empty());
//!
//! // iterator over the output values
//! let mut out = run(&filter, (Ctx::new([], &inputs), Val::from(input)));
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

pub use compile::Compiler;
pub use error::Error;
pub use filter::{Cv, FilterT, Native, RunPtr, UpdatePtr};
pub use rc_iter::RcIter;
pub use val::{Val, ValR, ValRs, ValT};

use alloc::string::String;
use rc_list::List as RcList;
use stack::Stack;

/// Variable bindings.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Vars<'a, V>(RcList<Bind<V, (&'a filter::Id, Self)>>);
type Inputs<'i, V> = RcIter<dyn Iterator<Item = Result<V, String>> + 'i>;

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bind<V = (), F = V> {
    /// binding to a variable
    Var(V),
    /// binding to a filter
    Fun(F),
}

impl<V, F> Bind<V, F> {
    /// Move references inward.
    pub fn as_ref(&self) -> Bind<&V, &F> {
        match self {
            Self::Var(x) => Bind::Var(x),
            Self::Fun(x) => Bind::Fun(x),
        }
    }
}

impl<T> Bind<T, T> {
    /// Apply a function to both binding types.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Bind<U, U> {
        match self {
            Self::Var(x) => Bind::Var(f(x)),
            Self::Fun(x) => Bind::Fun(f(x)),
        }
    }
}

impl<'a, V> Vars<'a, V> {
    fn get(&self, i: usize) -> Option<&Bind<V, (&'a filter::Id, Self)>> {
        self.0.get(i)
    }
}

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a, V> {
    vars: Vars<'a, V>,
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
    pub(crate) fn cons_fun(mut self, (f, ctx): (&'a filter::Id, Self)) -> Self {
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

    fn with_vars(&self, vars: Vars<'a, V>) -> Self {
        let inputs = self.inputs;
        Self { vars, inputs }
    }

    /// Return remaining input values.
    pub fn inputs(&self) -> &'a Inputs<'a, V> {
        self.inputs
    }
}

impl<'a, V: Clone> Ctx<'a, V> {
    pub fn pop_var(&mut self) -> V {
        let (head, tail) = match core::mem::take(&mut self.vars.0).pop() {
            Some((Bind::Var(head), tail)) => (head, tail),
            _ => panic!(),
        };
        self.vars.0 = tail;
        head
    }

    pub fn pop_fun(&mut self) -> (&'a filter::Id, Self) {
        let ((id, vars), tail) = match core::mem::take(&mut self.vars.0).pop() {
            Some((Bind::Fun(head), tail)) => (head, tail),
            _ => panic!(),
        };
        let inputs = self.inputs;
        self.vars.0 = tail;
        (id, Self { vars, inputs })
    }
}

/// Function from a value to a stream of value results.
#[derive(Debug, Clone)]
pub struct Filter<F>(compile::TermId, compile::Lut<F>);

pub fn run<'a, V: ValT, F: FilterT<V>>(f: &'a Filter<F>, cv: Cv<'a, V>) -> val::ValR2s<'a, V> {
    f.0.run(&f.1, cv)
}

/// Run a filter on given input, panic if it does not yield the given output.
///
/// This is for testing purposes.
pub fn yields<'a, V: ValT, F: FilterT<V>>(
    f: &'a Filter<F>,
    x: V,
    ys: impl Iterator<Item = val::ValR2<V>>,
) {
    let inputs = RcIter::new(core::iter::empty());
    let out = run(f, (Ctx::new([], &inputs), x));
    assert!(out.eq(ys));
}
