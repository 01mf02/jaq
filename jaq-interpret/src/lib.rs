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
pub use rc_iter::RcIter;
pub use val::{Val, ValR, ValRs};

use alloc::{string::String, vec::Vec};
use jaq_syn::Arg as Bind;
use lazy_iter::LazyIter;
use rc_list::List as RcList;

type Inputs<'i> = RcIter<dyn Iterator<Item = Result<Val, String>> + 'i>;

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a> {
    /// variable bindings
    vars: RcList<Bind<Val, (filter::Id, Self)>>,
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

    /// Add a new filter binding.
    pub(crate) fn cons_fun(mut self, f: (filter::Id, Self)) -> Self {
        self.vars = self.vars.cons(Bind::Fun(f));
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

/// Combined MIR/LIR compilation.
///
/// This allows to go from a parsed filter to a filter executable by this crate.
pub struct ParseCtx {
    /// errors occurred during transformation
    // TODO for v2.0: remove this and make it a function
    pub errs: Vec<jaq_syn::Spanned<mir::Error>>,
    native: Vec<(String, usize, filter::Native)>,
    def: jaq_syn::Def,
}

impl ParseCtx {
    /// Initialise new context with list of global variables.
    ///
    /// When running a filter produced by this context,
    /// values corresponding to the variables have to be supplied in the execution context.
    pub fn new(vars: Vec<String>) -> Self {
        use alloc::string::ToString;
        let def = jaq_syn::Def {
            lhs: jaq_syn::Call {
                name: "$".to_string(),
                args: vars.into_iter().map(Bind::Var).collect(),
            },
            rhs: jaq_syn::Main {
                defs: Vec::new(),
                body: (jaq_syn::filter::Filter::Id, 0..0),
            },
        };

        Self {
            errs: Vec::new(),
            native: Vec::new(),
            def,
        }
    }

    /// Add a native filter with given name and arity.
    pub fn insert_native(&mut self, name: String, arity: usize, f: filter::Native) {
        self.native.push((name, arity, f))
    }

    /// Add native filters with given names and arities.
    pub fn insert_natives<I>(&mut self, natives: I)
    where
        I: IntoIterator<Item = (String, usize, filter::Native)>,
    {
        self.native.extend(natives)
    }

    /// Import parsed definitions, such as obtained from the standard library.
    ///
    /// Errors that might occur include undefined variables, for example.
    pub fn insert_defs(&mut self, defs: impl IntoIterator<Item = jaq_syn::Def>) {
        self.def.rhs.defs.extend(defs);
    }

    /// Insert a root definition.
    #[deprecated(since="1.1", note="use `insert_defs` instead")]
    pub fn root_def(&mut self, def: jaq_syn::Def) {
        self.def.rhs.defs.push(def);
    }

    /// Insert a root filter.
    #[deprecated(since="1.1", note="this call has no effect")]
    pub fn root_filter(&mut self, filter: jaq_syn::Spanned<jaq_syn::filter::Filter>) {
        self.def.rhs.body = filter;
    }

    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn compile(&mut self, main: jaq_syn::Main) -> Filter {
        let mut mctx = mir::Ctx::default();
        mctx.native = self.native.clone();
        self.def.rhs.defs.extend(main.defs);
        self.def.rhs.body = main.body;
        let def = mctx.def(self.def.clone());
        self.errs = mctx.errs;

        if !self.errs.is_empty() {
            return Default::default();
        }

        lir::root_def(def)
    }

    /// Compile and run a filter on given input, panic if it does not compile or yield the given output.
    ///
    /// This is for testing purposes.
    pub fn yields(&mut self, x: Val, f: jaq_syn::Main, ys: impl Iterator<Item = ValR>) {
        let f = self.compile(f);
        assert!(self.errs.is_empty());

        let inputs = RcIter::new(core::iter::empty());
        let out = f.run((Ctx::new([], &inputs), x));

        assert!(out.eq(ys))
    }
}
