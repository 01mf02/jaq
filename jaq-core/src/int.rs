use core::{cmp, fmt};

/// Integer value.
#[derive(Copy, Clone, Debug)]
pub struct Int {
    /// absolute value of the integer
    abs: usize,
    /// is the integer positive?
    pos: bool,
}

impl Int {
    /// Absolute value of the integer.
    pub fn abs(self) -> usize {
        self.abs
    }

    /// Return true if the integer is greater-equal zero.
    pub fn is_positive(self) -> bool {
        self.pos
    }

    /// Convert integer to a floating-point value.
    pub fn as_f64(self) -> f64 {
        if self.pos {
            self.abs as f64
        } else {
            -(self.abs as f64)
        }
    }
}

impl From<usize> for Int {
    fn from(abs: usize) -> Self {
        Self { abs, pos: true }
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.pos {
            "-".fmt(f)?;
        }
        self.abs.fmt(f)
    }
}

impl core::ops::Neg for Int {
    type Output = Self;
    fn neg(mut self) -> Self {
        self.pos = !self.pos;
        self
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        if self.pos == other.pos {
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
        match (self.pos, other.pos) {
            (true, true) => self.abs.cmp(&other.abs),
            (false, false) => self.abs.cmp(&other.abs).reverse(),
            (true, false) | (false, true) if self.abs + other.abs == 0 => Equal,
            (true, false) => Greater,
            (false, true) => Less,
        }
    }
}
