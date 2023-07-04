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
//! use jaq_core::{Ctx, Error, FilterT, ParseCtx, RcIter, Val};
//! use serde_json::{json, Value};
//!
//! let input = json!(["Hello", "world"]);
//! let filter = ".[]";
//!
//! // start out only from core filters,
//! // which do not include filters in the standard library
//! // such as `map`, `select` etc.
//! let mut defs = ParseCtx::new(Vec::new());
//! defs.insert_core();
//!
//! // parse the filter in the context of the given definitions
//! let f = defs.parse_filter(&filter);
//! assert_eq!(defs.errs, Vec::new());
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

mod core;
mod error;
mod filter;
mod lazy_iter;
mod lir;
mod mir;
mod path;
mod rc_iter;
mod rc_lazy_list;
mod rc_list;
mod regex;
mod results;
mod val;

pub use jaq_parse as parse;

pub use error::Error;
pub use filter::{Args, FilterT, Native, Owned as Filter};
pub use mir::Ctx as ParseCtx;
pub use rc_iter::RcIter;
pub use val::{Val, ValR};

use alloc::string::String;
use lazy_iter::LazyIter;
use rc_list::RcList;

type Inputs<'i> = RcIter<dyn Iterator<Item = Result<Val, String>> + 'i>;

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a> {
    /// variable bindings
    vars: RcList<Val>,
    inputs: &'a Inputs<'a>,
}

impl<'a> Ctx<'a> {
    /// Construct a context.
    pub fn new(vars: impl IntoIterator<Item = Val>, inputs: &'a Inputs<'a>) -> Self {
        let vars = vars.into_iter().fold(RcList::Nil, |acc, v| acc.cons(v));
        Self { vars, inputs }
    }

    /// Add a new variable binding.
    pub(crate) fn cons_var(mut self, x: Val) -> Self {
        self.vars = self.vars.cons(x);
        self
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

impl ParseCtx {
    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn parse_filter(&mut self, filter: &str) -> Filter {
        let (filter, mut errs) = parse::parse(filter, parse::main());
        self.errs.append(&mut errs);

        if let Some((defs, body)) = filter {
            self.insert_defs(defs);
            self.root_filter(body);
        }

        if !self.errs.is_empty() {
            return Default::default();
        }
        //std::dbg!("before LIR");
        lir::root_def(&self.defs)
    }
}
