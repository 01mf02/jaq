//! JSON query language interpreter.
#![no_std]
#![warn(missing_docs)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod error;
mod filter;
mod path;
mod unparse;
mod val;

pub use jaq_parse as parse;

pub use error::Error;
pub use val::Val;

use alloc::{collections::BTreeMap, string::String, vec::Vec};
use parse::{Def, Main};
use unparse::unparse;

/// Function from a value to a stream of value results.
pub struct Filter(crate::filter::Filter);

impl Filter {
    /// Apply the filter to the given value and return stream of results.
    pub fn run(&self, val: Val) -> val::ValRs {
        self.0.run_with_empty_ctx(val)
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
        let f = unparse(&self.get(), &def.args, Vec::new(), def.body, errs);
        self.0.insert((def.name, def.args.len()), f);
    }

    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn finish(mut self, (defs, body): Main, errs: &mut Vec<parse::Error>) -> Filter {
        defs.into_iter().for_each(|def| self.insert(def, errs));
        Filter(unparse(&self.get(), &[], Vec::new(), body, errs))
    }

    /// Obtain filters by name and arity.
    fn get(&self) -> impl Fn(&(String, usize)) -> Option<filter::Filter> + '_ {
        |fun| self.0.get(fun).cloned()
    }
}
