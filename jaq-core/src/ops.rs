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
            (Pos(x), Pos(y)) => Ok(Pos(x + y)),
            (Neg(x), Neg(y)) => Ok(Neg(x + y)),
            (Pos(s), Neg(l)) | (Neg(l), Pos(s)) if s < l => Ok(Neg(l - s)),
            (Pos(l), Neg(s)) | (Neg(s), Pos(l)) => Ok(Pos(l - s)),
            (Pos(p), Float(f)) | (Float(f), Pos(p)) => Ok(Float(f + p as f64)),
            (Neg(n), Float(f)) | (Float(f), Neg(n)) => Ok(Float(f - n as f64)),
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
            (Pos(p), Neg(n)) => Ok(Pos(p + n)),
            (Neg(n), Pos(p)) => Ok(Neg(p + n)),
            (Pos(s), Pos(l)) | (Neg(l), Neg(s)) if s < l => Ok(Neg(l - s)),
            (Pos(l), Pos(s)) | (Neg(s), Neg(l)) => Ok(Pos(l - s)),
            (Pos(p), Float(f)) => Ok(Float(p as f64 - f)),
            (Neg(n), Float(f)) => Ok(Float(-(n as f64) - f)),
            (Float(f), Pos(p)) => Ok(Float(f - p as f64)),
            (Float(f), Neg(n)) => Ok(Float(f + n as f64)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Sub)),
        }
    }
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) => Ok(Pos(x * y)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) => Ok(Neg(x * y)),
            (Pos(p), Float(f)) | (Float(f), Pos(p)) => Ok(Float(f * p as f64)),
            (Neg(n), Float(f)) | (Float(f), Neg(n)) => Ok(Float(-f * n as f64)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Mul)),
        }
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) if x % y == 0 => Ok(Pos(x / y)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) if x % y == 0 => Ok(Neg(x / y)),
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) => Ok(Float(x as f64 / y as f64)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) => Ok(Float(-(x as f64 / y as f64))),
            (Pos(p), Float(f)) => Ok(Float(p as f64 / f)),
            (Neg(n), Float(f)) => Ok(Float(n as f64 / -f)),
            (Float(f), Pos(p)) => Ok(Float(f / p as f64)),
            (Float(f), Neg(n)) => Ok(Float(-f / n as f64)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Div)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y) | Neg(y)) => Ok(Pos(x % y)),
            (Neg(x), Pos(y) | Neg(y)) => Ok(Neg(x % y)),
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
