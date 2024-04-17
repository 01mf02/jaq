//! Runtime errors.
use crate::Val;
use alloc::string::ToString;
use core::fmt;

/// Errors that can occur during filter execution.
///
/// Each variant shows an example of how it can be produced.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error<V = Val> {
    /// `0 | error`
    Val(V),

    /// Expected a value of given type, but got something else
    Type(V, Type),
    /// `1 - "a"`
    MathOp(V, jaq_syn::MathOp, V),
    /// `{} | .[0]` or `[] | has("a")` or `{} | has(0)`
    Index(V, V),

    /// `[] | .[0] = 0`
    IndexOutOfBounds(isize),
    /// `0 |= .+1`
    PathExp,

    /// Tail-recursive call.
    ///
    /// This is used internally to execute tail-recursive filters.
    /// If this can be observed by users, then this is a bug.
    TailCall(crate::filter::TailCall<V>),
}

/// Types and sets of types.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Type {
    /// `[] | .["a"]` or `limit("a"; 0)` or `range(0; "a")`
    Int,
    /// `"1" | sin` or `pow(2; "3")` or `fma(2; 3; "4")`
    Float,
    /// `-"a"`, `"a" | round`
    Num,
    /// `{(0): 1}` or `0 | fromjson` or `0 | explode` or `"a b c" | split(0)`
    Str,
    /// `0 | sort` or `0 | implode` or `[] | .[0:] = 0`
    Arr,
    /// `0 | .[]` or `0 | .[0]` or `0 | keys` (array or object)
    Iter,
    /// `{}[0:1]` (string or array)
    Range,
}

impl Error {
    /// Convert the error into a value to be used by `catch` filters.
    pub fn as_val(self) -> Val {
        match self {
            Self::Val(ev) => ev,
            _ => Val::str(self.to_string()),
        }
    }

    /// Build an error from something that can be converted to a string.
    pub fn str(s: impl ToString) -> Self {
        Self::Val(Val::str(s.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Val(Val::Str(s)) => s.fmt(f),
            Self::Val(v) => v.fmt(f),
            Self::Type(v, ty) => write!(f, "cannot use {v} as {ty}"),
            Self::MathOp(l, op, r) => write!(f, "cannot calculate {l} {op} {r}"),
            Self::Index(v, i) => write!(f, "cannot index {v} with {i}"),
            Self::IndexOutOfBounds(i) => write!(f, "index {i} is out of bounds"),
            Self::PathExp => write!(f, "invalid path expression"),
            Self::TailCall(_) => panic!(),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => "integer".fmt(f),
            Self::Float => "floating-point number".fmt(f),
            Self::Num => "number".fmt(f),
            Self::Str => "string".fmt(f),
            Self::Arr => "array".fmt(f),
            Self::Iter => "iterable (array or object)".fmt(f),
            Self::Range => "rangeable (array or string)".fmt(f),
        }
    }
}
