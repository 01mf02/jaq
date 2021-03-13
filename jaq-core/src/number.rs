use core::convert::TryFrom;
use serde_json::Number;

pub type Num = Typ<u64, i64, f64>;
type Pair = Typ<(u64, u64), (i64, i64), (f64, f64)>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Typ<N, I, F> {
    Nat(N),
    Int(I),
    Flt(F),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Kind {
    Nat,
    Int,
    Flt,
}

impl<N, I, F> Typ<N, I, F> {
    fn kind(&self) -> Kind {
        match self {
            Typ::Nat(_) => Kind::Nat,
            Typ::Int(_) => Kind::Int,
            Typ::Flt(_) => Kind::Flt,
        }
    }
}

impl Num {
    // TODO: what happens on 32-bit machines?
    pub fn to_usize(self) -> Option<usize> {
        match self {
            Typ::Nat(n) => Some(n as usize),
            _ => None,
        }
    }

    pub fn to_isize(self) -> Option<isize> {
        match self {
            Typ::Nat(n) => Some(n as isize),
            Typ::Int(i) => Some(i as isize),
            _ => None,
        }
    }

    fn as_nat(self) -> u64 {
        match self {
            Typ::Nat(n) => n,
            Typ::Int(i) => i as u64,
            Typ::Flt(f) => f as u64,
        }
    }

    fn as_int(self) -> i64 {
        match self {
            Typ::Nat(n) => n as i64,
            Typ::Int(i) => i,
            Typ::Flt(f) => f as i64,
        }
    }

    fn as_flt(self) -> f64 {
        match self {
            Typ::Nat(n) => n as f64,
            Typ::Int(i) => i as f64,
            Typ::Flt(f) => f,
        }
    }

    /// Cast both numbers to the higher kind of both.
    fn pair(self, other: Self) -> Pair {
        match core::cmp::max(self.kind(), other.kind()) {
            Kind::Nat => Typ::Nat((self.as_nat(), other.as_nat())),
            Kind::Int => Typ::Int((self.as_int(), other.as_int())),
            Kind::Flt => Typ::Flt((self.as_flt(), other.as_flt())),
        }
    }
}

impl From<usize> for Num {
    fn from(n: usize) -> Num {
        Num::Nat(n as u64)
    }
}

impl TryFrom<Number> for Num {
    type Error = ();

    fn try_from(n: Number) -> Result<Self, Self::Error> {
        match n.as_u64() {
            Some(n) => Ok(Self::Nat(n)),
            _ => match n.as_i64() {
                Some(i) => Ok(Self::Int(i)),
                _ => match n.as_f64() {
                    Some(f) => Ok(Self::Flt(f)),
                    _ => Err(()),
                },
            },
        }
    }
}

impl TryFrom<Num> for Number {
    type Error = ();

    fn try_from(n: Num) -> Result<Self, Self::Error> {
        match n {
            Num::Nat(n) => Ok(Number::from(n)),
            Num::Int(n) => Ok(Number::from(n)),
            Num::Flt(n) => Number::from_f64(n).ok_or(()),
        }
    }
}

macro_rules! trait_op {
    ($i:ident, $trait:path) => {
        impl $trait for Num {
            type Output = Num;
            fn $i(self, rhs: Self) -> Self::Output {
                self.pair(rhs).$i()
            }
        }
    };
}

use core::ops::{Add, Div, Mul, Rem, Sub};

trait_op!(add, Add);
trait_op!(sub, Sub);
trait_op!(mul, Mul);
trait_op!(div, Div);
trait_op!(rem, Rem);

impl Pair {
    pub fn add(self) -> Num {
        match self {
            Typ::Nat((l, r)) => Typ::Nat(l + r),
            Typ::Int((l, r)) => Typ::Int(l + r),
            Typ::Flt((l, r)) => Typ::Flt(l + r),
        }
    }

    /// Preserve naturality of number if output is positive.
    pub fn mul(self) -> Num {
        match self {
            Typ::Int((l, r)) if l * r >= 0 => Typ::Nat((l * r) as u64),
            Typ::Nat((l, r)) => Typ::Nat(l * r),
            Typ::Int((l, r)) => Typ::Int(l * r),
            Typ::Flt((l, r)) => Typ::Flt(l * r),
        }
    }

    /// Preserve naturality of number if output is positive.
    pub fn sub(self) -> Num {
        match self {
            Typ::Nat((l, r)) if l >= r => Typ::Nat(l - r),
            Typ::Int((l, r)) if l >= r => Typ::Nat((l - r) as u64),
            Typ::Nat((l, r)) => Typ::Int(l as i64 - r as i64),
            Typ::Int((l, r)) => Typ::Int(l - r),
            Typ::Flt((l, r)) => Typ::Flt(l - r),
        }
    }

    /// Preserve integrality of numbers if remainder of division is zero.
    pub fn div(self) -> Num {
        match self {
            Typ::Nat((l, r)) if l % r == 0 => Typ::Nat(l / r),
            Typ::Int((l, r)) if l % r == 0 => Typ::Int(l / r),
            Typ::Nat((l, r)) => Typ::Flt(l as f64 / r as f64),
            Typ::Int((l, r)) => Typ::Flt(l as f64 / r as f64),
            Typ::Flt((l, r)) => Typ::Flt(l / r),
        }
    }

    /// Preserve naturality of number if output is positive.
    pub fn rem(self) -> Num {
        match self {
            Typ::Int((l, r)) if l % r >= 0 => Typ::Nat((l % r) as u64),
            Typ::Nat((l, r)) => Typ::Nat(l % r),
            Typ::Int((l, r)) => Typ::Int(l % r),
            Typ::Flt((l, r)) => Typ::Flt(l % r),
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.pair(*other) {
            Typ::Nat((l, r)) => l.partial_cmp(&r),
            Typ::Int((l, r)) => l.partial_cmp(&r),
            Typ::Flt((l, r)) => l.partial_cmp(&r),
        }
    }
}
