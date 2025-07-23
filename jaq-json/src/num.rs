use alloc::rc::Rc;
use alloc::string::{String, ToString};
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use num_bigint::{BigInt, Sign};
use num_traits::cast::{FromPrimitive, ToPrimitive};

/// Arbitrary-precision number.
#[derive(Clone, Debug)]
pub enum Num {
    /// Machine-size integer
    Int(isize),
    /// Arbitrarily large integer
    BigInt(Rc<BigInt>),
    /// Floating-point number
    Float(f64),
    /// Decimal number
    Dec(Rc<String>),
}

impl Num {
    fn big_int(i: BigInt) -> Self {
        Self::BigInt(i.into())
    }

    pub(crate) fn from_str(s: &str) -> Self {
        Self::try_from_int_str(s, 10).unwrap_or_else(|| Self::Dec(Rc::new(s.to_string())))
    }

    pub(crate) fn from_integral<T: Copy + TryInto<isize> + Into<BigInt>>(x: T) -> Self {
        x.try_into()
            .map_or_else(|_| Num::big_int(x.into()), Num::Int)
    }

    pub(crate) fn try_from_int_str(i: &str, radix: u32) -> Option<Self> {
        let big = || BigInt::parse_bytes(i.as_bytes(), radix).map(Self::big_int);
        isize::from_str_radix(i, radix)
            .ok()
            .map(Num::Int)
            .or_else(big)
    }

    /// Try to parse a decimal string to a [`Self::Float`], else return NaN.
    pub(crate) fn from_dec_str(n: &str) -> Self {
        // TODO: changed to NaN!
        n.parse().map_or(Self::Float(f64::NAN), Self::Float)
    }

    pub(crate) fn is_int(&self) -> bool {
        matches!(self, Self::Int(_) | Self::BigInt(_))
    }

    /// If the value is a machine-sized integer, return it, else fail.
    pub(crate) fn as_isize(&self) -> Option<isize> {
        match self {
            Self::Int(i) => Some(*i),
            Self::BigInt(i) => i.to_isize(),
            _ => None,
        }
    }

    /// If the value is or can be converted to float, return it, else fail.
    pub(crate) fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Int(n) => Some(*n as f64),
            Self::BigInt(n) => Some(n.to_f64().unwrap()),
            Self::Float(n) => Some(*n),
            Self::Dec(n) => n.parse().ok(),
        }
    }

    pub(crate) fn length(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(i.abs()),
            Self::BigInt(i) => match i.sign() {
                Sign::Plus | Sign::NoSign => Self::BigInt(i.clone()),
                Sign::Minus => Self::BigInt(BigInt::from(i.magnitude().clone()).into()),
            },
            Self::Dec(n) => Self::from_dec_str(n).length(),
            Self::Float(f) => Self::Float(f.abs()),
        }
    }
}

fn int_or_big<const N: usize>(
    i: Option<isize>,
    x: [isize; N],
    f: fn([BigInt; N]) -> BigInt,
) -> Num {
    i.map_or_else(|| Num::big_int(f(x.map(BigInt::from))), Num::Int)
}

impl core::ops::Add for Num {
    type Output = Num;
    fn add(self, rhs: Self) -> Self::Output {
        use num_bigint::BigInt;
        use Num::*;
        match (self, rhs) {
            (Int(x), Int(y)) => int_or_big(x.checked_add(y), [x, y], |[x, y]| x + y),
            (Int(i), BigInt(b)) | (BigInt(b), Int(i)) => Self::big_int(&BigInt::from(i) + &*b),
            (Int(i), Float(f)) | (Float(f), Int(i)) => Float(f + i as f64),
            (BigInt(x), BigInt(y)) => Self::big_int(&*x + &*y),
            (BigInt(i), Float(f)) | (Float(f), BigInt(i)) => Float(f + i.to_f64().unwrap()),
            (Float(x), Float(y)) => Float(x + y),
            (Dec(n), r) => Self::from_dec_str(&n) + r,
            (l, Dec(n)) => l + Self::from_dec_str(&n),
        }
    }
}

impl core::ops::Sub for Num {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        use num_bigint::BigInt;
        use Num::*;
        match (self, rhs) {
            (Int(x), Int(y)) => int_or_big(x.checked_sub(y), [x, y], |[x, y]| x - y),
            (Int(i), BigInt(b)) => Self::big_int(&BigInt::from(i) - &*b),
            (BigInt(b), Int(i)) => Self::big_int(&*b - &BigInt::from(i)),
            (BigInt(x), BigInt(y)) => Self::big_int(&*x - &*y),
            (Float(f), Int(i)) => Float(f - i as f64),
            (Int(i), Float(f)) => Float(i as f64 - f),
            (Float(f), BigInt(i)) => Float(f - i.to_f64().unwrap()),
            (BigInt(i), Float(f)) => Float(i.to_f64().unwrap() - f),
            (Float(x), Float(y)) => Float(x - y),
            (Dec(n), r) => Self::from_dec_str(&n) - r,
            (l, Dec(n)) => l - Self::from_dec_str(&n),
        }
    }
}

impl core::ops::Mul for Num {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        use num_bigint::BigInt;
        use Num::*;
        match (self, rhs) {
            (Int(x), Int(y)) => int_or_big(x.checked_mul(y), [x, y], |[x, y]| x * y),

            (Int(i), BigInt(b)) | (BigInt(b), Int(i)) => Self::big_int(&BigInt::from(i) * &*b),
            (BigInt(x), BigInt(y)) => Self::big_int(&*x * &*y),
            (BigInt(i), Float(f)) | (Float(f), BigInt(i)) => Float(f * i.to_f64().unwrap()),
            (Float(f), Int(i)) | (Int(i), Float(f)) => Float(f * i as f64),
            (Float(x), Float(y)) => Float(x * y),
            (Dec(n), r) => Self::from_dec_str(&n) * r,
            (l, Dec(n)) => l * Self::from_dec_str(&n),
        }
    }
}

impl core::ops::Div for Num {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        use Num::{BigInt, Dec, Float, Int};
        match (self, rhs) {
            (Int(l), r) => Float(l as f64) / r,
            (l, Int(r)) => l / Float(r as f64),
            (BigInt(l), r) => Float(l.to_f64().unwrap()) / r,
            (l, BigInt(r)) => l / Float(r.to_f64().unwrap()),
            (Float(x), Float(y)) => Float(x / y),
            (Dec(n), r) => Self::from_dec_str(&n) / r,
            (l, Dec(n)) => l / Self::from_dec_str(&n),
        }
    }
}

impl core::ops::Rem for Num {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        use num_bigint::BigInt;
        use Num::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Int(x % y),
            (BigInt(x), BigInt(y)) => Num::big_int(&*x % &*y),
            (Int(i), BigInt(b)) => Num::big_int(&BigInt::from(i) % &*b),
            (BigInt(b), Int(i)) => Num::big_int(&*b % &BigInt::from(i)),
            (Int(i), Float(f)) => Float(i as f64 % f),
            (Float(f), Int(i)) => Float(f % i as f64),
            (BigInt(i), Float(f)) => Float(i.to_f64().unwrap() % f),
            (Float(f), BigInt(i)) => Float(f % i.to_f64().unwrap()),
            (Float(x), Float(y)) => Float(x % y),
            (Dec(n), r) => Self::from_dec_str(&n) % r,
            (l, Dec(n)) => l % Self::from_dec_str(&n),
        }
    }
}

impl core::ops::Neg for Num {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Self::Int(x) => int_or_big(x.checked_neg(), [x], |[x]| -x),
            Self::BigInt(x) => Self::big_int(-&*x),
            Self::Float(x) => Self::Float(-x),
            Self::Dec(n) => -Self::from_dec_str(&n),
        }
    }
}

/// Hash a number.
///
/// This hasher maps all finite numbers to integers.
/// For floating-point numbers, this creates many hash collisions,
/// because e.g. 0.2 and 0.4 yield the same hash value.
/// However, it makes it easy to uphold the property that
/// for each number `n` and `m`, if `n == m`, then `n.hash() == m.hash()`.
/// In any case, a hash map with floating-point numbers as keys is pure madness,
/// so it's not something that we should encourage with high performance.
impl Hash for Num {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let big = |i: &BigInt, state: &mut H| {
            state.write_u8(0);
            i.hash(state)
        };
        match self {
            Self::BigInt(i) => big(i, state),
            Self::Int(i) => big(&(*i).into(), state),
            // SAFETY: this should always succeed
            // TODO: use this also for `round()`?
            Self::Float(f) if f.is_finite() => big(&BigInt::from_f64(*f).unwrap(), state),
            // hash all remaining floats, like NaN and infinity, to the same
            Self::Float(_) => state.write_u8(1),
            Self::Dec(d) => Self::from_dec_str(d).hash(state),
        }
    }
}

#[test]
fn hash_nums() {
    use core::hash::BuildHasher;
    let h = |n| foldhash::fast::FixedState::with_seed(42).hash_one(n);

    assert_eq!(h(Num::Int(4096)), h(Num::big_int(4096.into())));
    assert_eq!(h(Num::Float(0.2)), h(Num::Float(0.4)));
    assert_eq!(h(Num::Float(0.0)), h(Num::Int(0)));
    assert_eq!(h(Num::Float(3.1415)), h(Num::big_int(3.into())));
    assert_eq!(h(Num::Float(f64::NAN)), h(Num::Float(0.0 / 0.0)));

    assert_ne!(h(Num::Float(0.2)), h(Num::Int(1)));
    assert_ne!(h(Num::Int(1)), h(Num::Int(2)));
}

impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Int(i), Self::Float(f)) | (Self::Float(f), Self::Int(i)) => {
                float_eq(*i as f64, *f)
            }
            (Self::BigInt(x), Self::BigInt(y)) => x == y,
            (Self::Int(i), Self::BigInt(b)) | (Self::BigInt(b), Self::Int(i)) => {
                **b == BigInt::from(*i)
            }
            (Self::BigInt(i), Self::Float(f)) | (Self::Float(f), Self::BigInt(i)) => {
                float_eq(i.to_f64().unwrap(), *f)
            }
            (Self::Float(x), Self::Float(y)) => float_eq(*x, *y),
            (Self::Dec(x), Self::Dec(y)) if Rc::ptr_eq(x, y) => true,
            (Self::Dec(n), y) => &Self::from_dec_str(n) == y,
            (x, Self::Dec(n)) => x == &Self::from_dec_str(n),
        }
    }
}

impl Eq for Num {}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Num {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x.cmp(y),
            (Self::Int(x), Self::BigInt(y)) => BigInt::from(*x).cmp(y),
            (Self::Int(i), Self::Float(f)) => float_cmp(*i as f64, *f),
            (Self::BigInt(x), Self::Int(y)) => (**x).cmp(&BigInt::from(*y)),
            (Self::BigInt(x), Self::BigInt(y)) => x.cmp(y),
            // BigInt::to_f64 always yields Some, large values become f64::INFINITY
            (Self::BigInt(x), Self::Float(y)) => float_cmp(x.to_f64().unwrap(), *y),
            (Self::Float(f), Self::Int(i)) => float_cmp(*f, *i as f64),
            (Self::Float(x), Self::BigInt(y)) => float_cmp(*x, y.to_f64().unwrap()),
            (Self::Float(x), Self::Float(y)) => float_cmp(*x, *y),
            (Self::Dec(x), Self::Dec(y)) if Rc::ptr_eq(x, y) => Ordering::Equal,
            (Self::Dec(n), y) => Self::from_dec_str(n).cmp(y),
            (x, Self::Dec(n)) => x.cmp(&Self::from_dec_str(n)),
        }
    }
}

fn float_eq(left: f64, right: f64) -> bool {
    float_cmp(left, right) == Ordering::Equal
}

fn float_cmp(left: f64, right: f64) -> Ordering {
    if left == 0. && right == 0. {
        // consider negative and positive 0 as equal
        Ordering::Equal
    } else if left.is_nan() {
        // there are more than 50 shades of NaN, and which of these
        // you strike when you perform a calculation is not deterministic (!),
        // therefore `total_cmp` may yield different results for the same calculation
        // so we bite the bullet and handle this like in jq
        Ordering::Less
    } else if right.is_nan() {
        Ordering::Greater
    } else {
        f64::total_cmp(&left, &right)
    }
}

impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::BigInt(i) => write!(f, "{i}"),
            Self::Float(x) if x.is_finite() => write!(f, "{x:?}"),
            Self::Float(_) => write!(f, "null"),
            Self::Dec(n) => write!(f, "{n}"),
        }
    }
}
