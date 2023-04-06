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
//! use jaq_core::{parse, Ctx, Definitions, Error, RcIter, Val};
//! use serde_json::{json, Value};
//!
//! let input = json!(["Hello", "world"]);
//! let filter = ".[]";
//!
//! // start out only from core filters,
//! // which do not include filters in the standard library
//! // such as `map`, `select` etc.
//! let defs = Definitions::core();
//!
//! // parse the filter in the context of the given definitions
//! let mut errs = Vec::new();
//! let f = parse::parse(&filter, parse::main()).0.unwrap();
//! let f = defs.finish(f, Vec::new(), &mut errs);
//! assert_eq!(errs, Vec::new());
//!
//! let inputs = RcIter::new(core::iter::empty());
//!
//! // iterator over the output values
//! let mut out = f.run(Ctx::new([], &inputs), Val::from(input));
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

mod error;
mod filter;
mod lazy_iter;
mod mir;
mod path;
mod rc_iter;
mod rc_lazy_list;
mod rc_list;
mod regex;
mod results;
mod unparse;
mod val;

pub use jaq_parse as parse;

pub use error::Error;
pub use rc_iter::RcIter;
pub use val::{Val, ValR};

use alloc::{collections::BTreeMap, string::String, vec::Vec};
use lazy_iter::LazyIter;
use parse::{Def, Main};
use rc_list::RcList;

type Inputs<'i> = RcIter<dyn Iterator<Item = Result<Val, String>> + 'i>;

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'i> {
    /// variable bindings
    vars: RcList<Val>,
    inputs: &'i Inputs<'i>,
    recs: &'i [(usize, filter::Filter)],
}

impl<'i> Ctx<'i> {
    /// Construct a context.
    pub fn new(vars: impl IntoIterator<Item = Val>, inputs: &'i Inputs<'i>) -> Self {
        let vars = vars.into_iter().fold(RcList::Nil, |acc, v| acc.cons(v));
        let recs = &[];
        Self { vars, inputs, recs }
    }

    /// Add a new variable binding.
    pub fn cons_var(mut self, x: Val) -> Self {
        self.vars = self.vars.cons(x);
        self
    }

    fn skip_vars(&self, n: usize) -> Self {
        Self {
            vars: self.vars.skip(n).clone(),
            inputs: self.inputs,
            recs: self.recs,
        }
    }

    /// Obtain and remove the `save` most recent variable bindings,
    /// then remove additional `skip` most recent bindings,
    /// finally add the original `save` bindings.
    ///
    /// This seemingly complicated behaviour stems from
    /// calls to recursive filters with `save` variable arguments.
    /// To call such a filter, we have to first produce the
    /// argument values and save them in the context.
    /// Next, we have to remove `skip` variables that might have been bound
    /// by the last call to the recursive filter.
    /// Finally, we add the `save` arguments to the context again,
    /// so that the recursive filter can start again with the same context length.
    fn save_skip_vars(mut self, save: usize, skip: usize) -> Self {
        self.vars = if save == 0 {
            self.vars.skip(skip).clone()
        } else {
            let (saved, rest) = self.vars.pop_many(save);
            let saved = saved.into_iter().rev().cloned();
            rest.skip(skip).clone().cons_many(saved)
        };
        self
    }
}

/// Function from a value to a stream of value results.
#[derive(Debug, Default, Clone)]
pub struct Filter(filter::Filter, Vec<(usize, filter::Filter)>);

impl Filter {
    /// Apply the filter to the given value and return stream of results.
    pub fn run<'a>(&'a self, mut ctx: Ctx<'a>, val: Val) -> val::ValRs<'a> {
        ctx.recs = &self.1;
        self.0.run((ctx, val))
    }
}

/// Link names and arities to corresponding filters.
///
/// For example, if we define a filter `def map(f): [.[] | f]`,
/// then the definitions will associate `map/1` to its definition.
pub struct Definitions(BTreeMap<(String, usize), filter::Filter>);

impl Definitions {
    /// Start out with only core filters, such as `length`, `keys`, ...
    ///
    /// Does not import filters from the standard library, such as `map`.
    pub fn core() -> Self {
        Self(filter::Filter::core().into_iter().collect())
    }

    /// Import a parsed definition, such as obtained from the standard library.
    ///
    /// Errors that might occur include undefined variables, for example.
    pub fn insert(&mut self, def: Def, errs: &mut Vec<parse::Error>) {
        let f = unparse::def(&self.get(), &def.args, def.body, errs);
        self.0.insert((def.name, def.args.len()), f);
    }

    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn finish(
        mut self,
        (defs, body): Main,
        vars: Vec<String>,
        errs: &mut Vec<parse::Error>,
    ) -> Filter {
        defs.into_iter().for_each(|def| self.insert(def, errs));
        let recs = Vec::new();
        Filter(unparse::filter(&self.get(), &[], vars, body, errs), recs)
    }

    /// Obtain filters by name and arity.
    fn get(&self) -> impl Fn(&(String, usize)) -> Option<filter::Filter> + '_ {
        |fun| self.0.get(fun).cloned()
    }
}
