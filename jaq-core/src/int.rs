use core::fmt;

/// Integer value.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Int {
    /// absolute value of the integer
    abs: usize,
    /// is the integer positive?
    pos: bool,
}

impl Int {
    pub fn is_positive(&self) -> bool {
        self.pos
    }

    pub fn abs(&self) -> usize {
        self.abs
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
