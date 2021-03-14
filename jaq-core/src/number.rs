use core::convert::{TryFrom, TryInto};
use core::fmt;
use serde_json::Number;

pub type Num = Typ<i64, f64>;
type Pair = Typ<(i64, i64), (f64, f64)>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Typ<I, F> {
    Int(I),
    Flt(F),
}

impl Num {
    pub fn to_isize(self) -> Option<isize> {
        match self {
            Typ::Int(i) => i.try_into().ok(),
            _ => None,
        }
    }

    fn as_flt(self) -> f64 {
        match self {
            Typ::Int(i) => i as f64,
            Typ::Flt(f) => f,
        }
    }

    /// Cast both numbers to the higher kind of both.
    fn pair(self, other: Self) -> Pair {
        use Typ::{Flt, Int};
        match (self, other) {
            (Int(l), Int(r)) => Int((l, r)),
            (l, r) => Flt((l.as_flt(), r.as_flt())),
        }
    }
}

impl From<usize> for Num {
    fn from(n: usize) -> Num {
        Num::Int(n as i64)
    }
}

impl TryFrom<Number> for Num {
    type Error = ();

    fn try_from(n: Number) -> Result<Self, Self::Error> {
        match n.as_i64() {
            Some(i) => Ok(Self::Int(i)),
            _ => match n.as_f64() {
                Some(f) => Ok(Self::Flt(f)),
                _ => Err(()),
            },
        }
    }
}

impl TryFrom<Num> for Number {
    type Error = ();

    fn try_from(n: Num) -> Result<Self, Self::Error> {
        match n {
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

macro_rules! impl_op {
    ($i:ident, $op:path) => {
        impl Pair {
            fn $i(self) -> Num {
                match self {
                    Typ::Int((l, r)) => Typ::Int($op(l, r)),
                    Typ::Flt((l, r)) => Typ::Flt($op(l, r)),
                }
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

impl_op!(add, Add::add);
impl_op!(sub, Sub::sub);
impl_op!(mul, Mul::mul);
impl_op!(rem, Rem::rem);

impl Pair {
    /// Preserve integrality of numbers if remainder of division is zero.
    fn div(self) -> Num {
        match self {
            Typ::Int((l, r)) if l % r == 0 => Typ::Int(l / r),
            Typ::Int((l, r)) => Typ::Flt(l as f64 / r as f64),
            Typ::Flt((l, r)) => Typ::Flt(l / r),
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match self.pair(*other) {
            Typ::Int((l, r)) => l.partial_cmp(&r),
            Typ::Flt((l, r)) => l.partial_cmp(&r),
        }
    }
}

impl fmt::Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Int(n) => n.fmt(f),
            Self::Flt(n) => n.fmt(f),
        }
    }
}
