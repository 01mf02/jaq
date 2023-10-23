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
//! use jaq_interpret::{Ctx, Error, FilterT, ParseCtx, RcIter, Val};
//! use serde_json::{json, Value};
//!
//! let input = json!(["Hello", "world"]);
//! let filter = ".[]";
//!
//! // start out only from core filters,
//! // which do not include filters in the standard library
//! // such as `map`, `select` etc.
//! let mut defs = ParseCtx::new(Vec::new());
//!
//! // parse the filter
//! let (f, errs) = jaq_parse::parse(filter, jaq_parse::main());
//! assert_eq!(errs, Vec::new());
//!
//! // compile the filter in the context of the given definitions
//! let f = defs.compile(f.unwrap());
//! assert!(defs.errs.is_empty());
//!
//! let inputs = RcIter::new(core::iter::empty());
//!
//! // iterator over the output values
//! let mut out = f.run((Ctx::new([], &inputs), Val::from(input)));
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
pub mod error;
mod filter;
mod lazy_iter;
mod lir;
mod mir;
mod path;
mod rc_iter;
mod rc_lazy_list;
mod rc_list;
pub mod results;
mod val;

pub use error::Error;
pub use filter::{Args, FilterT, Native, Owned as Filter, RunPtr, UpdatePtr};
pub use mir::Ctx as ParseCtx;
pub use rc_iter::RcIter;
pub use val::{Val, ValR, ValRs};

use alloc::string::String;
use lazy_iter::LazyIter;
use rc_list::List as RcList;

type Inputs<'i> = RcIter<dyn Iterator<Item = Result<Val, String>> + 'i>;

/// Binding of a value or a filter.
///
/// In jq, we can bind filters in three different ways:
///
/// 1. `f as $x | ...`
/// 2. `def g($x): ...; g(f)`
/// 3. `def g(fx): ...; g(f)`
///
/// In the first two cases, we bind the outputs of `f` to a variable `$x`.
/// In the third case, we bind `f` to a filter `fx`
enum Bind<V, F> {
    Var(V),
    Fun(F),
}

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a> {
    /// variable bindings
    vars: RcList<Bind<Val, (filter::Ref<'a>, Self)>>,
    inputs: &'a Inputs<'a>,
}

impl<'a> Ctx<'a> {
    /// Construct a context.
    pub fn new(vars: impl IntoIterator<Item = Val>, inputs: &'a Inputs<'a>) -> Self {
        let vars = RcList::new().extend(vars.into_iter().map(Bind::Var));
        Self { vars, inputs }
    }

    /// Add a new variable binding.
    pub(crate) fn cons_var(mut self, x: Val) -> Self {
        self.vars = self.vars.cons(Bind::Var(x));
        self
    }

    /// Remove the `skip` most recent variable bindings.
    fn skip_vars(mut self, skip: usize) -> Self {
        self.vars = self.vars.skip(skip).clone();
        self
    }

    /// Return remaining input values.
    pub fn inputs(&self) -> &'a Inputs<'a> {
        self.inputs
    }
}

impl ParseCtx {
    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn compile(&mut self, main: jaq_syn::Main) -> Filter {
        self.insert_defs(main.defs);
        self.root_filter(main.body);

        if !self.errs.is_empty() {
            return Default::default();
        }
        //std::dbg!("before LIR");
        lir::root_def(&self.defs)
    }

    /// Compile and run a filter on given input, panic if it does not compile or yield the given output.
    ///
    /// This is for testing purposes.
    pub fn yields(&mut self, x: Val, f: jaq_syn::Main, ys: impl Iterator<Item = ValR>) {
        let f = self.compile(f);
        assert!(self.errs.is_empty());

        let inputs = RcIter::new(core::iter::empty());
        let out = f.run((Ctx::new([], &inputs), x));
        itertools::assert_equal(out, ys);
    }
}
