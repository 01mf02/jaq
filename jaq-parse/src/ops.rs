use core::fmt;
use core::ops::{Add, Div, Mul, Rem, Sub};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Arithmetic operation, such as `+`, `-`, `*`, `/`, `%`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub enum MathOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,
}

impl MathOp {
    /// Perform the arithmetic operation on the given inputs.
    pub fn run<I, O>(&self, l: I, r: I) -> O
    where
        I: Add<Output = O>,
        I: Sub<Output = O>,
        I: Mul<Output = O>,
        I: Div<Output = O>,
        I: Rem<Output = O>,
    {
        match self {
            Self::Add => l + r,
            Self::Sub => l - r,
            Self::Mul => l * r,
            Self::Div => l / r,
            Self::Rem => l % r,
        }
    }
}

impl fmt::Display for MathOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => "+".fmt(f),
            Self::Sub => "-".fmt(f),
            Self::Mul => "*".fmt(f),
            Self::Div => "/".fmt(f),
            Self::Rem => "%".fmt(f),
        }
    }
}

/// An operation that orders two values, such as `<`, `<=`, `>`, `>=`, `==`, `!=`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
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

impl OrdOp {
    /// Perform the ordering operation on the given inputs.
    pub fn run<I: PartialOrd + PartialEq>(&self, l: &I, r: &I) -> bool {
        match self {
            Self::Gt => l > r,
            Self::Ge => l >= r,
            Self::Lt => l < r,
            Self::Le => l <= r,
            Self::Eq => l == r,
            Self::Ne => l != r,
        }
    }
}

impl fmt::Display for OrdOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lt => "<".fmt(f),
            Self::Gt => ">".fmt(f),
            Self::Le => "<=".fmt(f),
            Self::Ge => ">=".fmt(f),
            Self::Eq => "==".fmt(f),
            Self::Ne => "!=".fmt(f),
        }
    }
}
