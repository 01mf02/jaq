//! Logical and mathematical operations on values.

use crate::{Error, RValRs, Val, ValR, ValRs};

#[derive(Clone, Debug)]
pub enum MathOp {
    /// Addition (+).
    Add,
    /// Subtraction (-).
    Sub,
    /// Multiplication (*).
    Mul,
    /// Division (/).
    Div,
    /// Remainder (%).
    Rem,
}

/// An operation that takes two values and returns a boolean value.
#[derive(Debug)]
pub enum OrdOp {
    /// Less-than (<).
    Lt,
    /// Less-than or equal (<=).
    Le,
    /// Greater-than (>).
    Gt,
    /// Greater-than or equal (>=).
    Ge,
    /// Equals (=).
    Eq,
    /// Not equals (!=).
    Ne,
}

#[derive(Debug)]
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

impl Val {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }
}

impl MathOp {
    pub fn run(&self, l: Val, r: Val) -> ValR {
        use MathOp::*;
        match self {
            Add => l + r,
            Sub => l - r,
            Mul => l * r,
            Div => l / r,
            Rem => l % r,
        }
    }

    pub fn passive(&self) -> &str {
        match self {
            MathOp::Add => "added",
            MathOp::Sub => "subtracted",
            MathOp::Mul => "multiplied",
            MathOp::Div => "divided",
            MathOp::Rem => "divided (remainder)",
        }
    }
}

impl OrdOp {
    pub fn run(&self, l: &Val, r: &Val) -> bool {
        use OrdOp::*;
        match self {
            Gt => l > r,
            Ge => l >= r,
            Lt => l < r,
            Le => l <= r,
            Eq => l == r,
            Ne => l != r,
        }
    }
}

impl LogicOp {
    pub fn run<'a>(&self, l: bool, r: impl Fn() -> RValRs<'a>) -> ValRs<'a> {
        use core::iter::once;
        match (l, self) {
            (false, LogicOp::And) | (true, LogicOp::Or) => Box::new(once(Ok(Val::Bool(l)))),
            _ => Box::new(r().map(|r| Ok(Val::Bool(r?.as_bool())))),
        }
    }
}
