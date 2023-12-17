use core::fmt;
use core::ops::{Add, Div, Mul, Rem, Sub};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Binary arithmetical operators (`+`, `-`, `*`, `/`, `%`, …)
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MathOp {
    /// Addition operator (`+`)
    Add,
    /// Subtraction operator (`-`)
    Sub,
    /// Multiplication operator (`*`)
    Mul,
    /// Division operator (`/`)
    Div,
    /// Remainder operator (`%`)
    Rem,
}

impl MathOp {
    /// Perform the arithmetical operation on the given inputs.
    pub fn run<I, O>(&self, l: I, r: I) -> O
    where
        I: Add<Output = O> + Sub<Output = O> + Mul<Output = O> + Div<Output = O> + Rem<Output = O>,
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

/// Binary comparative operators (`<`, `<=`, `>`, `>=`, `==`, `!=`, …)
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OrdOp {
    /// Less-than operation (`<`).
    Lt,
    /// Less-than or equal-to operation (`<=`).
    Le,
    /// Greater-than operation (`>`).
    Gt,
    /// Greater-than or equal-to operation (`>=`).
    Ge,
    /// Equal-to operation (`=`).
    Eq,
    /// Not equal-to operation (`!=`).
    Ne,
}

impl OrdOp {
    /// Perform the comparative operation on the given inputs.
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
