//! Errors that can occur during filter execution.

use crate::ops::MathOp;
use crate::Val;
use core::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    ObjKey(Val),
    Length(Val),
    Round(Val),
    Keys(Val),
    Iter(Val),
    Neg(Val),
    MathOp(Val, Val, MathOp),
    Index(Val),
    IndexWith(Val, Val),
    IndexOutOfBounds((usize, bool)),
    Isize(Val),
    Usize(Val),
    SliceAssign(Val),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Error::*;
        match self {
            ObjKey(v) => write!(f, "cannot use {} as object key", v),
            Length(v) => write!(f, "{} has no length", v),
            Round(v) => write!(f, "cannot round {}", v),
            Keys(v) => write!(f, "{} has no keys", v),
            Iter(v) => write!(f, "cannot iterate over {}", v),
            Neg(v) => write!(f, "{} cannot be negated", v),
            MathOp(l, r, op) => write!(f, "{} and {} cannot be {}", l, r, passive(op)),
            Index(v) => write!(f, "cannot index {}", v),
            IndexWith(v, i) => write!(f, "cannot index {} with {}", v, i),
            IndexOutOfBounds((i, true)) => write!(f, "index {} is out of bounds", i),
            IndexOutOfBounds((i, false)) => write!(f, "index -{} is out of bounds", i),
            Isize(v) => write!(f, "cannot use {} as (signed) integer", v),
            Usize(v) => write!(f, "cannot use {} as unsigned integer", v),
            SliceAssign(v) => write!(f, "cannot assign non-array ({}) to an array slice", v),
        }
    }
}

fn passive(op: &MathOp) -> &str {
    match op {
        MathOp::Add => "added",
        MathOp::Sub => "subtracted",
        MathOp::Mul => "multiplied",
        MathOp::Div => "divided",
        MathOp::Rem => "divided (remainder)",
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
