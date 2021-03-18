use crate::ops::MathOp;
use crate::Val;

#[derive(Clone, Debug)]
pub enum Error {
    ObjKey(Val),
    Length(Val),
    Iter(Val),
    MathOp(Val, Val, MathOp),
    IndexIsize,
}
