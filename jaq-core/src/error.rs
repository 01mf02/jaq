use crate::ops::MathOp;
use crate::Val;

#[derive(Clone, Debug)]
pub enum Error {
    ObjKey(Val),
    Iter(Val),
    MathOp(Val, Val, MathOp),
    IndexIsize,
}
