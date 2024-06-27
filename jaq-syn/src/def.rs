use crate::filter::Filter;
use crate::Spanned;
use alloc::{string::String, vec::Vec};
use core::ops::Deref;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Call to a filter identified by a name type `N` with arguments of type `A`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Call<A = Arg, N = String> {
    /// Name of the filter, e.g. `map`
    pub name: N,
    /// Arguments of the filter, e.g. `["f"]`
    pub args: Vec<A>,
}

impl<A, N> Call<A, N> {
    /// Apply a function to the call arguments.
    pub fn map_args<B>(self, f: impl FnMut(A) -> B) -> Call<B, N> {
        Call {
            name: self.name,
            args: self.args.into_iter().map(f).collect(),
        }
    }
}

/// A definition, such as `def map(f): [.[] | f];`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Def<Rhs = Main> {
    /// left-hand side, i.e. what shall be defined, e.g. `map(f)`
    pub lhs: Call,
    /// right-hand side, i.e. what the LHS should be defined as, e.g. `[.[] | f]`
    pub rhs: Rhs,
}

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
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg<V = String, F = V> {
    /// binding to a variable
    Var(V),
    /// binding to a filter
    Fun(F),
}

impl<T> Arg<T, T> {
    /// Apply a function to both binding types.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Arg<U, U> {
        match self {
            Self::Var(x) => Arg::Var(f(x)),
            Self::Fun(x) => Arg::Fun(f(x)),
        }
    }
}

impl<V, F> Arg<V, F> {
    /// Move references inward.
    pub fn as_ref(&self) -> Arg<&V, &F> {
        match self {
            Self::Var(x) => Arg::Var(x),
            Self::Fun(x) => Arg::Fun(x),
        }
    }
}

impl<V: Deref, F: Deref> Arg<V, F> {
    /// Move references inward, while dereferencing content.
    pub fn as_deref(&self) -> Arg<&<V as Deref>::Target, &<F as Deref>::Target> {
        match self {
            Self::Var(x) => Arg::Var(x),
            Self::Fun(x) => Arg::Fun(x),
        }
    }
}

// TODO for v2.0: remove this
impl<V, F> Arg<V, F> {
    /// Create a variable argument with given name (without leading "$").
    pub fn new_var(name: V) -> Self {
        Self::Var(name)
    }

    /// Create a filter argument with given name.
    pub fn new_filter(name: F) -> Self {
        Self::Fun(name)
    }

    /// True if the argument is a variable.
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
}

// TODO for v2.0: remove this
impl<V: Deref, F: Deref> Arg<V, F> {
    /// If the argument is a variable, return its name without leading "$", otherwise `None`.
    pub fn get_var(&self) -> Option<&<V as Deref>::Target> {
        match self {
            Self::Var(v) => Some(v),
            Self::Fun(_) => None,
        }
    }

    /// If the argument is a filter, return its name, otherwise `None`.
    pub fn get_filter(&self) -> Option<&<F as Deref>::Target> {
        match self {
            Self::Var(_) => None,
            Self::Fun(f) => Some(f),
        }
    }
}

/// (Potentially empty) sequence of definitions, followed by a filter.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Main<F = Filter> {
    /// Definitions at the top of the filter
    pub defs: Vec<Def<Self>>,
    /// Body of the filter, e.g. `[.[] | f]`.
    pub body: Spanned<F>,
}

use crate::parse;

impl From<&parse::Def<&str, parse::Term<&str>>> for Def {
    fn from(def: &parse::Def<&str, parse::Term<&str>>) -> Self {
        use alloc::string::ToString;
        let args = def.args.iter().map(|arg| {
            if let Some(v) = arg.strip_prefix('$') {
                Arg::Var(v.to_string())
            } else {
                Arg::Fun(arg.to_string())
            }
        });
        Def {
            lhs: Call {
                name: def.name.to_string(),
                args: args.collect(),
            },
            rhs: (&def.body).into(),
        }
    }
}

impl From<&parse::Term<&str>> for Main {
    fn from(tm: &parse::Term<&str>) -> Self {
        use alloc::string::ToString;
        match tm {
            parse::Term::Def(defs, tm) => Main {
                defs: defs.iter().map(Def::from).collect(),
                body: ((&**tm).into(), 0..42),
            },
            tm => Main {
                defs: Vec::new(),
                body: ((&*tm).into(), 0..42),
            },
        }
    }
}
