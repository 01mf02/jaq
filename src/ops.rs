//! Logical and mathematical operations on values.

use crate::val::Val;

#[derive(Debug)]
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
pub enum LogicOp {
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
    /// Logical conjunction (&&).
    And,
    /// Logical disjunction (||).
    Or,
}

impl core::ops::Add for Val {
    type Output = Option<Val>;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Some(x),
            (Num(l), Num(r)) => Some(Num(l + r)),
            _ => None,
        }
    }
}

impl core::ops::Sub for Val {
    type Output = Option<Val>;
    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Some(Num(l - r)),
            _ => None,
        }
    }
}

impl core::ops::Mul for Val {
    type Output = Option<Val>;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Some(Num(l * r)),
            _ => None,
        }
    }
}

impl core::ops::Div for Val {
    type Output = Option<Val>;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Some(Num(l / r)),
            _ => None,
        }
    }
}

impl core::ops::Rem for Val {
    type Output = Option<Val>;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(l), Num(r)) => Some(Num(l % r)),
            _ => None,
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

impl Val {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }
}

impl MathOp {
    pub fn run(&self, l: Val, r: Val) -> Option<Val> {
        use MathOp::*;
        match self {
            Add => l + r,
            Sub => l - r,
            Mul => l * r,
            Div => l / r,
            Rem => l % r,
        }
    }
}

impl LogicOp {
    pub fn run(&self, l: &Val, r: &Val) -> bool {
        use LogicOp::*;
        match self {
            Gt => l > r,
            Ge => l >= r,
            Lt => l < r,
            Le => l <= r,
            Eq => l == r,
            Ne => l != r,
            And => l.as_bool() && r.as_bool(),
            Or => l.as_bool() || r.as_bool(),
        }
    }
}
