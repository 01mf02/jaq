use crate::Val;
use alloc::string::{String, ToString};
use core::fmt;

/// Errors that can occur during filter execution.
///
/// Each variant shows an example of how it can be produced.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    /// `inputs` (when given invalid JSON data)
    Parse(String),
    /// `0 | error`
    Val(Val),
    /// `[1114112] | implode`
    Char(isize),
    /// `{(0): 1}` or `0 | fromjson` or `0 | explode` or `"a b c" | split(0)`
    Str(Val),
    /// `0 | sort` or `0 | implode`
    Arr(Val),
    /// `0 == 0 | length`
    Length(Val),
    /// `"a" | round`
    Round(Val),
    /// `"[1, 2" | fromjson`
    FromJson(Val, String),
    /// `[] | has("a")` or `{} | has(0)`
    Has(Val, Val),
    /// `0 | keys`
    Keys(Val),
    /// `0 | .[]`
    Iter(Val),
    /// `-"a"`
    Neg(Val),
    /// `1 - "a"`
    MathOp(Val, jaq_syn::MathOp, Val),
    /// `0 | .[0]`
    Index(Val),
    /// `{} | .[0]`
    IndexWith(Val, Val),
    /// `[] | .[0] = 0`
    IndexOutOfBounds(isize),
    /// `[] | .["a"]` or `limit("a"; 0)` or `range(0; "a")`
    Int(Val),
    /// `"1" | sin` or `pow(2; "3")` or `fma(2; 3; "4")`
    Float(Val),
    /// `[] | .[0:] = 0`
    SliceAssign(Val),
    /// `0 |= .+1`
    PathExp,
    /// `"a" | test("(")`
    Regex(String),
    /// `"a" | test("."; "b")`
    RegexFlag(char),
    /// arbitrary errors for custom filters
    Custom(String),
}

impl Error {
    /// Convert the error into a value to be used by `catch` filters.
    pub fn as_val(self) -> Val {
        match self {
            Self::Val(ev) => ev,
            _ => Val::str(self.to_string()),
        }
    }

    /// Build an error from anything stringifyable.
    pub fn from_any(s: impl ToString) -> Self {
        Self::Val(Val::str(s.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Parse(s) => s.fmt(f),
            Self::Val(Val::Str(s)) => s.fmt(f),
            Self::Val(v) => v.fmt(f),
            Self::Char(i) => write!(f, "cannot use {i} as character"),
            Self::Str(v) => write!(f, "cannot use {v} as string"),
            Self::Arr(v) => write!(f, "cannot use {v} as array"),
            Self::Length(v) => write!(f, "{v} has no length"),
            Self::Round(v) => write!(f, "cannot round {v}"),
            Self::FromJson(v, why) => write!(f, "cannot parse {v} as JSON: {why}"),
            Self::Keys(v) => write!(f, "{v} has no keys"),
            Self::Has(v, k) => write!(f, "cannot check whether {v} has key {k}"),
            Self::Iter(v) => write!(f, "cannot iterate over {v}"),
            Self::Neg(v) => write!(f, "cannot negate {v}"),
            Self::MathOp(l, op, r) => write!(f, "cannot calculate {l} {op} {r}"),
            Self::Index(v) => write!(f, "cannot index {v}"),
            Self::IndexWith(v, i) => write!(f, "cannot index {v} with {i}"),
            Self::IndexOutOfBounds(i) => write!(f, "index {i} is out of bounds"),
            Self::Int(v) => write!(f, "cannot use {v} as integer"),
            Self::Float(v) => write!(f, "cannot use {v} as float"),
            Self::SliceAssign(v) => write!(f, "cannot assign non-array ({v}) to an array slice"),
            Self::PathExp => write!(f, "invalid path expression"),
            Self::Regex(e) => write!(f, "invalid regex: {e}"),
            Self::RegexFlag(c) => write!(f, "invalid regex flag '{c}'"),
            Self::Custom(e) => write!(f, "custom filter error: {e}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
