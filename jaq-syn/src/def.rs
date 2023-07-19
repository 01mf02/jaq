use crate::filter::Filter;
use crate::Spanned;
use alloc::{string::String, vec::Vec};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A definition, such as `def map(f): [.[] | f];`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Def {
    /// Name of the filter, e.g. `map`
    pub name: String,
    /// Arguments of the filter, e.g. `["f"]`
    pub args: Vec<Arg>,
    /// Definitions at the top of the filter
    pub defs: Vec<Self>,
    /// Body of the filter, e.g. `[.[] | f`.
    pub body: Spanned<Filter>,
}

/// Argument of a definition, such as `$v` or `f` in `def foo($v; f): ...`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Arg {
    name: String,
    var: bool,
}

impl Arg {
    /// Create a variable argument with given name (without leading "$").
    pub fn new_var(name: String) -> Self {
        Self { name, var: true }
    }

    /// Create a filter argument with given name.
    pub fn new_filter(name: String) -> Self {
        Self { name, var: false }
    }

    /// True if the argument is a variable.
    pub fn is_var(&self) -> bool {
        self.var
    }

    /// If the argument is a variable, return its name without leading "$", otherwise `None`.
    pub fn get_var(&self) -> Option<&str> {
        self.var.then_some(&*self.name)
    }

    /// If the argument is a filter, return its name, otherwise `None`.
    pub fn get_filter(&self) -> Option<&str> {
        (!self.var).then_some(&*self.name)
    }
}

// TODO: rename to Body?
/// (Potentially empty) sequence of definitions, followed by a filter.
pub type Main = (Vec<Def>, Spanned<Filter>);
