use crate::{unparse, Filter};
use alloc::{collections::BTreeMap, string::String, vec::Vec};
use jaq_parse::{Def, Error, Main};

/// Link names and arities to corresponding filters.
///
/// For example, if we define a filter `def map(f): [.[] | f]`,
/// then the definitions will associate `map/1` to its definition.
pub struct Definitions(BTreeMap<(String, usize), Filter>);

impl Definitions {
    /// Start out with only core filters, such as `length`, `type`, ...
    ///
    /// Does not import filters from the standard library, such as `map`.
    pub fn core() -> Self {
        Self(Filter::core().into_iter().collect())
    }

    /// Import a list of parsed definitions, such as the standard library.
    ///
    /// Errors that might occur include undefined variables, for example.
    pub fn add(&mut self, defs: Vec<Def>, errs: &mut Vec<Error>) {
        for def in defs {
            let f = unparse(&self.get(), &def.args, Vec::new(), def.body, errs);
            self.0.insert((def.name, def.args.len()), f);
        }
    }

    /// Given a main filter (consisting of definitions and a body), return a finished filter.
    pub fn finish(mut self, (defs, body): Main, errs: &mut Vec<Error>) -> Filter {
        self.add(defs, errs);
        unparse(&self.get(), &[], Vec::new(), body, errs)
    }

    /// Obtain filters by name and arity.
    fn get(&self) -> impl Fn(&(String, usize)) -> Option<Filter> + '_ {
        |fun| self.0.get(fun).cloned()
    }
}
