use alloc::boxed::Box;
use core::{cmp, fmt};

/// Integer value.
#[derive(Copy, Clone, Debug)]
pub struct Int {
    /// absolute value of the integer
    abs: usize,
    /// is the integer positive?
    sign: Sign,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Sign {
    Neg,
    Pos,
}

use Sign::*;

impl Int {
    fn pos(abs: usize) -> Self {
        Self { abs, sign: Pos }
    }

    fn neg(abs: usize) -> Self {
        Self { abs, sign: Neg }
    }

    /// Absolute value of the integer.
    pub fn abs(self) -> usize {
        self.abs
    }

    /// Return true if the integer is greater-equal zero.
    pub fn is_positive(self) -> bool {
        self.sign == Pos
    }

    /// Convert integer to a floating-point value.
    pub fn as_f64(self) -> f64 {
        match self.sign {
            Pos => self.abs as f64,
            Neg => -(self.abs as f64),
        }
    }

    /// Return all numbers in the range `[self, other)`.
    pub fn range(&self, other: &Self) -> Box<dyn Iterator<Item = Self>> {
        match (self.sign, other.sign) {
            (Pos, Pos) => (Box::new((self.abs..other.abs).map(Self::pos))),
            (Neg, Neg) => (Box::new((other.abs + 1..self.abs + 1).rev().map(|i| Self::neg(i)))),
            (Neg, Pos) => {
                let neg = self.range(&Self::neg(0));
                let pos = Self::pos(0).range(other);
                Box::new(neg.chain(pos))
            }
            (Pos, Neg) => Box::new(core::iter::empty()),
        }
    }
}

impl From<usize> for Int {
    fn from(abs: usize) -> Self {
        Self::pos(abs)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.sign {
            Pos => write!(f, "{}", self.abs),
            Neg => write!(f, "-{}", self.abs),
        }
    }
}

impl core::ops::Add for Int {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self.sign, self.abs, rhs.sign, rhs.abs) {
            (Pos, x, Pos, y) => Self::pos(x + y),
            (Neg, x, Neg, y) => Self::neg(x + y),
            (Pos, s, Neg, l) | (Neg, l, Pos, s) if s < l => Self::neg(l - s),
            (Pos, l, Neg, s) | (Neg, s, Pos, l) => Self::pos(l - s),
        }
    }
}

impl core::ops::Sub for Int {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        self + (-rhs)
    }
}

impl core::ops::Mul for Int {
    type Output = Int;
    fn mul(self, rhs: Self) -> Self {
        match (self.sign, self.abs, rhs.sign, rhs.abs) {
            (Pos, x, Pos, y) | (Neg, x, Neg, y) => Self::pos(x * y),
            (Pos, x, Neg, y) | (Neg, x, Pos, y) => Self::neg(x * y),
        }
    }
}

impl core::ops::Div for Int {
    type Output = Int;
    fn div(self, rhs: Self) -> Self {
        match (self.sign, self.abs, rhs.sign, rhs.abs) {
            (Pos, x, Pos, y) | (Neg, x, Neg, y) => Self::pos(x / y),
            (Pos, x, Neg, y) | (Neg, x, Pos, y) => Self::neg(x / y),
        }
    }
}

impl core::ops::Rem for Int {
    type Output = Int;
    fn rem(self, rhs: Self) -> Self::Output {
        match (self.sign, self.abs, rhs.sign, rhs.abs) {
            (Pos, x, Pos | Neg, y) => Self::pos(x % y),
            (Neg, x, Pos | Neg, y) => Self::neg(x % y),
        }
    }
}

impl core::ops::Neg for Int {
    type Output = Self;
    fn neg(mut self) -> Self {
        self.sign = match self.sign {
            Pos => Neg,
            Neg => Pos,
        };
        self
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        if self.sign == other.sign {
            self.abs == other.abs
        } else {
            self.abs == 0 && other.abs == 0
        }
    }
}

impl Eq for Int {}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Int {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        use cmp::Ordering::*;
        match (self.sign, other.sign) {
            (Pos, Pos) => self.abs.cmp(&other.abs),
            (Neg, Neg) => self.abs.cmp(&other.abs).reverse(),
            (Pos, Neg) | (Neg, Pos) if self.abs + other.abs == 0 => Equal,
            (Pos, Neg) => Greater,
            (Neg, Pos) => Less,
        }
    }
}
