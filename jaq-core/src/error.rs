use crate::Val;
use alloc::string::String;
use core::fmt;

/// Errors that can occur during filter execution.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Error {
    Val(Val),
    ObjKey(Val),
    Length(Val),
    Round(Val),
    FromJson(Val, Option<String>),
    ToNumber(Val),
    Sort(Val),
    Has(Val, Val),
    Split,
    Range,
    Keys(Val),
    Iter(Val),
    Neg(Val),
    MathOp(Val, jaq_parse::MathOp, Val),
    Index(Val),
    IndexWith(Val, Val),
    IndexOutOfBounds((usize, bool)),
    Int(Val),
    Nat(Val),
    SliceAssign(Val),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Val(Val::Str(s)) => s.fmt(f),
            Self::Val(v) => v.fmt(f),
            Self::ObjKey(v) => write!(f, "cannot use {v} as object key"),
            Self::Length(v) => write!(f, "{v} has no length"),
            Self::Round(v) => write!(f, "cannot round {v}"),
            Self::Sort(v) => write!(f, "cannot sort {v}, as it is not an array"),
            Self::FromJson(v, None) => write!(f, "cannot parse {v} as JSON"),
            Self::FromJson(v, Some(why)) => write!(f, "cannot parse {v} as JSON: {why}"),
            Self::ToNumber(v) => write!(f, "cannot parse {v} as number"),
            Self::Keys(v) => write!(f, "{v} has no keys"),
            Self::Has(v, k) => write!(f, "cannot check whether {v} has key {k}"),
            Self::Split => write!(f, "split input and separator must be strings"),
            Self::Range => write!(f, "range bounds must be integers"),
            Self::Iter(v) => write!(f, "cannot iterate over {v}"),
            Self::Neg(v) => write!(f, "cannot negate {v}"),
            Self::MathOp(l, op, r) => write!(f, "cannot calculate {l} {op} {r}"),
            Self::Index(v) => write!(f, "cannot index {v}"),
            Self::IndexWith(v, i) => write!(f, "cannot index {v} with {i}"),
            Self::IndexOutOfBounds((i, true)) => write!(f, "index {i} is out of bounds"),
            Self::IndexOutOfBounds((i, false)) => write!(f, "index -{i} is out of bounds"),
            Self::Int(v) => write!(f, "cannot use {v} as integer"),
            Self::Nat(v) => write!(f, "cannot use {v} as positive integer"),
            Self::SliceAssign(v) => write!(f, "cannot assign non-array ({v}) to an array slice"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
