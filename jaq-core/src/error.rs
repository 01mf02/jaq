use crate::ops::MathOp;
use crate::Val;
use core::fmt;

#[derive(Clone, Debug)]
pub enum Error {
    ObjKey(Val),
    Length(Val),
    Iter(Val),
    MathOp(Val, Val, MathOp),
    IndexIsize,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Error::*;
        match self {
            ObjKey(v) => write!(f, "cannot use {} as object key", v),
            Length(v) => write!(f, "{} has no length", v),
            Iter(v) => write!(f, "cannot iterate over {}", v),
            MathOp(l, r, op) => write!(f, "{} and {} cannot be {}", l, r, op.passive()),
            IndexIsize => write!(f, "cannot use ??? as index"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
