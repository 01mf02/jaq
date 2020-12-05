use core::convert::TryFrom;
use serde_json::Number;

enum Num {
    Nat(u64),
    Int(i64),
    Flt(f64),
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

enum NumPair {
    Nat(u64, u64),
    Int(i64, i64),
    Flt(f64, f64),
}

impl TryFrom<(Number, Number)> for NumPair {
    type Error = ();

    fn try_from((l, r): (Number, Number)) -> Result<Self, Self::Error> {
        match (l.as_u64(), r.as_u64()) {
            (Some(l), Some(r)) => Ok(Self::Nat(l, r)),
            _ => match (l.as_i64(), r.as_i64()) {
                (Some(l), Some(r)) => Ok(Self::Int(l, r)),
                _ => match (l.as_f64(), r.as_f64()) {
                    (Some(l), Some(r)) => Ok(Self::Flt(l, r)),
                    _ => Err(()),
                },
            },
        }
    }
}

macro_rules! pair_op {
    ($i:ident, $p:path) => {
        pub fn $i(self) -> Num {
            match self {
                Self::Nat(l, r) => Num::Nat($p(l, r)),
                Self::Int(l, r) => Num::Int($p(l, r)),
                Self::Flt(l, r) => Num::Flt($p(l, r)),
            }
        }
    };
}

impl NumPair {
    pair_op!(add, std::ops::Add::add);
    pair_op!(sub, std::ops::Sub::sub);
    pair_op!(mul, std::ops::Mul::mul);
    pair_op!(div, std::ops::Div::div);
    pair_op!(rem, std::ops::Rem::rem);
}
