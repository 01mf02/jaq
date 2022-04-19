use crate::Val;
use alloc::string::String;
use core::fmt;

/// Errors that can occur during filter execution.
///
/// Each variant shows an example of how it can be produced.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Error {
    /// `0 | error`
    Val(Val),
    /// `{(0): 1}`
    Str(Val),
    /// `0 == 0 | length`
    Length(Val),
    /// `"a" | round`
    Round(Val),
    /// `0 | fromjson` or `"[1, 2" | fromjson`
    FromJson(Val, Option<String>),
    /// `0 | sort`
    Sort(Val),
    /// `[] | has("a")` or `{} | has(0)`
    Has(Val, Val),
    /// `"a b c" | split(0)`
    Split,
    /// `range(0; "a")`
    Range,
    /// `0 | keys`
    Keys(Val),
    /// `0 | .[]`
    Iter(Val),
    /// `-"a"`
    Neg(Val),
    /// `1 - "a"`
    MathOp(Val, jaq_parse::MathOp, Val),
    /// `0 | .[0]`
    Index(Val),
    /// `{} | .[0]`
    IndexWith(Val, Val),
    /// `[] | .[0] = 0`
    IndexOutOfBounds(isize),
    /// `[] | .["a"]` or `limit("a"; 0)`
    Int(Val),
    /// `[] | .[0:] = 0`
    SliceAssign(Val),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Val(Val::Str(s)) => s.fmt(f),
            Self::Val(v) => v.fmt(f),
            Self::Str(v) => write!(f, "cannot use {v} as string"),
            Self::Length(v) => write!(f, "{v} has no length"),
            Self::Round(v) => write!(f, "cannot round {v}"),
            Self::Sort(v) => write!(f, "cannot sort {v}, as it is not an array"),
            Self::FromJson(v, None) => write!(f, "cannot parse {v} as JSON"),
            Self::FromJson(v, Some(why)) => write!(f, "cannot parse {v} as JSON: {why}"),
            Self::Keys(v) => write!(f, "{v} has no keys"),
            Self::Has(v, k) => write!(f, "cannot check whether {v} has key {k}"),
            Self::Split => write!(f, "split input and separator must be strings"),
            Self::Range => write!(f, "range bounds must be integers"),
            Self::Iter(v) => write!(f, "cannot iterate over {v}"),
            Self::Neg(v) => write!(f, "cannot negate {v}"),
            Self::MathOp(l, op, r) => write!(f, "cannot calculate {l} {op} {r}"),
            Self::Index(v) => write!(f, "cannot index {v}"),
            Self::IndexWith(v, i) => write!(f, "cannot index {v} with {i}"),
            Self::IndexOutOfBounds(i) => write!(f, "index {i} is out of bounds"),
            Self::Int(v) => write!(f, "cannot use {v} as integer"),
            Self::SliceAssign(v) => write!(f, "cannot assign non-array ({v}) to an array slice"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
