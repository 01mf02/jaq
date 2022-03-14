//! Logical and mathematical operations on values.

use crate::{Error, RValRs, Val, ValR, ValRs};
use alloc::boxed::Box;
pub use jaq_parse::{MathOp, OrdOp};

#[derive(Clone, Debug)]
pub enum LogicOp {
    /// Logical conjunction (&&).
    And,
    /// Logical disjunction (||).
    Or,
}

impl core::ops::Add for Val {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Ok(x),
            (Num(l), Num(r)) => Ok(Num(l + r)),
            (Str(mut l), Str(r)) => {
                l.push_str(&r);
                Ok(Str(l))
            }
            (Arr(mut l), Arr(r)) => {
                l.extend(r);
                Ok(Arr(l))
            }
            (Obj(l), Obj(r)) => Ok(Obj(l + r)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Add)),
        }
    }
}

impl core::ops::Sub for Val {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Ok(Num(l - r)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Sub)),
        }
    }
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Ok(Num(l * r)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Mul)),
        }
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Ok(Num(l / r)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Div)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Ok(Num(l % r)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Rem)),
        }
    }
}

impl LogicOp {
    pub fn run<'a>(&self, l: bool, r: impl FnOnce() -> RValRs<'a>) -> ValRs<'a> {
        use core::iter::once;
        match (l, self) {
            (false, LogicOp::And) | (true, LogicOp::Or) => Box::new(once(Ok(Val::Bool(l)))),
            _ => Box::new(r().map(|r| Ok(Val::Bool(r?.as_bool())))),
        }
    }
}
