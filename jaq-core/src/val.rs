//! JSON values with reference-counted sharing.

use crate::Error;
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::fmt;
use jaq_parse::MathOp;

/// JSON value with sharing.
///
/// The speciality of this type is that numbers are distinguished into
/// machine-sized integers and 64-bit floating-point numbers.
/// This allows using integers to index arrays,
/// while using floating-point numbers to do general math.
///
/// Operations on numbers follow a few principles:
/// * The sum, difference, product, and remainder of two integers is integer.
/// * Any other operation between two numbers yields a float.
#[derive(Clone, Debug)]
pub enum Val {
    /// Null
    Null,
    /// Boolean
    Bool(bool),
    /// Integer
    Int(isize),
    /// Floating-point number
    Float(f64),
    /// Floating-point number or integer not fitting into `Int`
    Num(Rc<serde_json::Number>),
    /// String
    Str(Rc<String>),
    /// Array
    Arr(Rc<Vec<Val>>),
    /// Order-preserving map
    Obj(Rc<indexmap::IndexMap<Rc<String>, Val, ahash::RandomState>>),
}

/// A value result.
pub type ValR = Result<Val, Error>;

/// A stream of value results.
pub type ValRs<'a> = Box<dyn Iterator<Item = ValR> + 'a>;

impl Val {
    /// True if the value is neither null nor false.
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }

    /// If the value is integer, return it, else fail.
    pub fn as_int(&self) -> Result<isize, Error> {
        match self {
            Self::Int(i) => Ok(*i),
            _ => Err(Error::Int(self.clone())),
        }
    }

    /// If the value is a string, return it, else fail.
    pub fn as_str(&self) -> Result<Rc<String>, Error> {
        match self {
            Self::Str(s) => Ok(Rc::clone(s)),
            _ => Err(Error::Str(self.clone())),
        }
    }

    /// If the value is an array, return it, else fail.
    fn as_arr(&self) -> Result<Rc<Vec<Val>>, Error> {
        match self {
            Self::Arr(a) => Ok(Rc::clone(a)),
            _ => Err(Error::Arr(self.clone())),
        }
    }

    /// If the value is an integer representing a valid Unicode codepoint, return it, else fail.
    fn as_codepoint(&self) -> Result<char, Error> {
        let i = self.as_int()?;
        // conversion from isize to u32 may fail on 64-bit systems for high values of c
        let u = u32::try_from(i).map_err(|_| Error::Char(i))?;
        char::from_u32(u).ok_or(Error::Char(i))
    }

    /// Return 0 for null, the absolute value for numbers, and
    /// the length for strings, arrays, and objects.
    ///
    /// Fail on booleans.
    pub fn len(&self) -> Result<Self, Error> {
        match self {
            Self::Null => Ok(Self::Int(0)),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Int(i) => Ok(Self::Int(i.abs())),
            Self::Num(n) => Self::from(&**n).len(),
            Self::Float(f) => Ok(Self::Float(f.abs())),
            Self::Str(s) => Ok(Self::Int(s.chars().count() as isize)),
            Self::Arr(a) => Ok(Self::Int(a.len() as isize)),
            Self::Obj(o) => Ok(Self::Int(o.len() as isize)),
        }
    }

    /// Apply a rounding function to floating-point numbers, then convert them to integers.
    ///
    /// Return integers unchanged, and fail on any other input.
    pub fn round(&self, f: impl FnOnce(f64) -> f64) -> Result<Self, Error> {
        match self {
            Self::Int(_) => Ok(self.clone()),
            Self::Float(x) => Ok(Self::Int(f(*x) as isize)),
            Self::Num(n) => Self::from(&**n).round(f),
            _ => Err(Error::Round(self.clone())),
        }
    }

    /// Return true if `value | .[key]` is defined.
    ///
    /// Fail on values that are neither null, arrays, nor objects.
    pub fn has(&self, key: &Self) -> Result<bool, Error> {
        match (self, key) {
            (Self::Null, _) => Ok(false),
            (Self::Arr(a), Self::Int(i)) if *i >= 0 => Ok((*i as usize) < a.len()),
            (Self::Obj(o), Self::Str(s)) => Ok(o.contains_key(&**s)),
            _ => Err(Error::Has(self.clone(), key.clone())),
        }
    }

    /// Return any `key` for which `value | .[key]` is defined.
    ///
    /// Fail on values that are neither arrays nor objects.
    pub fn keys(&self) -> Result<Vec<Val>, Error> {
        match self {
            Self::Arr(a) => Ok((0..a.len() as isize).map(Val::Int).collect()),
            Self::Obj(o) => Ok(o.keys().map(|k| Val::Str(Rc::clone(k))).collect()),
            _ => Err(Error::Keys(self.clone())),
        }
    }

    /// Return the elements of an array or the values of an object (omitting its keys).
    ///
    /// Fail on any other value.
    pub fn iter(&self) -> Result<Box<dyn Iterator<Item = Val> + '_>, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new(a.iter().cloned())),
            Self::Obj(o) => Ok(Box::new(o.values().cloned())),
            _ => Err(Error::Iter(self.clone())),
        }
    }

    /// `a` contains `b` iff either
    /// * the string `b` is a substring of `a`,
    /// * every element in the array `b` is contained in some element of the array `a`,
    /// * for every key-value pair `k, v` in `b`,
    ///   there is a key-value pair `k, v'` in `a` such that `v'` contains `v`, or
    /// * `a equals `b`.
    pub fn contains(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l), Self::Str(r)) => l.contains(&**r),
            (Self::Arr(l), Self::Arr(r)) => r.iter().all(|r| l.iter().any(|l| l.contains(r))),
            (Self::Obj(l), Self::Obj(r)) => r
                .iter()
                .all(|(k, r)| l.get(k).map(|l| l.contains(r)).unwrap_or(false)),
            _ => self == other,
        }
    }

    /// Convert string to JSON.
    ///
    /// Fail on any other value.
    pub fn from_json(&self) -> ValR {
        let serde = serde_json::from_str::<serde_json::Value>(&self.as_str()?)
            .map_err(|e| Error::FromJson(self.clone(), e.to_string()))?;
        Ok(Val::from(serde))
    }

    /// Convert a string into an array of its Unicode codepoints.
    pub fn explode(&self) -> Result<Vec<Val>, Error> {
        // conversion from u32 to isize may fail on 32-bit systems for high values of c
        let conv = |c: char| Val::Int(isize::try_from(c as u32).unwrap());
        Ok(self.as_str()?.chars().map(conv).collect())
    }

    /// Convert an array of Unicode codepoints into a string.
    pub fn implode(&self) -> Result<String, Error> {
        self.as_arr()?.iter().map(|v| v.as_codepoint()).collect()
    }

    /// Apply a function to a string.
    pub fn mutate_str(&self, f: impl Fn(&mut String)) -> ValR {
        let mut s = self.as_str()?;
        f(Rc::make_mut(&mut s));
        Ok(Self::Str(s))
    }

    /// Apply a function to an array.
    pub fn mutate_arr(&self, f: impl Fn(&mut Vec<Val>)) -> ValR {
        let mut a = self.as_arr()?;
        f(Rc::make_mut(&mut a));
        Ok(Self::Arr(a))
    }

    /// Sort array by the given function.
    ///
    /// Fail on any other value.
    pub fn sort_by<'a>(&self, f: impl Fn(Val) -> ValRs<'a>) -> ValR {
        let mut a = self.as_arr()?;
        // Some(e) iff an error has previously occurred
        let mut err = None;
        Rc::make_mut(&mut a).sort_by_cached_key(|x| {
            if err.is_some() {
                return Vec::new();
            };
            match f(x.clone()).collect() {
                Ok(y) => y,
                Err(e) => {
                    err = Some(e);
                    Vec::new()
                }
            }
        });
        err.map_or(Ok(Val::Arr(a)), Err)
    }

    /// Split a string by a given separator string.
    ///
    /// Fail if any of the two given values is not a string.
    pub fn split(&self, sep: &Self) -> Result<Vec<Val>, Error> {
        Ok(self
            .as_str()?
            .split(&*sep.as_str()?)
            .map(|s| Val::Str(Rc::new(s.to_string())))
            .collect())
    }
}

impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => n
                .to_string()
                .parse()
                .map_or_else(|_| Self::Num(Rc::new(n)), Self::Int),
            String(s) => Self::Str(Rc::new(s)),
            Array(a) => Self::Arr(Rc::new(a.into_iter().map(|x| x.into()).collect())),
            Object(o) => Self::Obj(Rc::new(
                o.into_iter().map(|(k, v)| (Rc::new(k), v.into())).collect(),
            )),
        }
    }
}

impl From<Val> for serde_json::Value {
    fn from(v: Val) -> serde_json::Value {
        use serde_json::Value::*;
        match v {
            Val::Null => Null,
            Val::Bool(b) => Bool(b),
            Val::Int(i) => Number(i.into()),
            Val::Float(f) => serde_json::Number::from_f64(f).map_or(Null, Number),
            Val::Num(n) => Number((*n).clone()),
            Val::Str(s) => String((*s).clone()),
            Val::Arr(a) => Array(a.iter().map(|x| x.clone().into()).collect()),
            Val::Obj(o) => Object(
                o.iter()
                    .map(|(k, v)| ((**k).clone(), v.clone().into()))
                    .collect(),
            ),
        }
    }
}

impl From<&serde_json::Number> for Val {
    fn from(n: &serde_json::Number) -> Self {
        n.to_string().parse().map_or(Self::Null, Self::Float)
    }
}

impl core::ops::Add for Val {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Ok(x),
            (Int(x), Int(y)) => Ok(Int(x + y)),
            (Int(i), Float(f)) | (Float(f), Int(i)) => Ok(Float(f + i as f64)),
            (Float(x), Float(y)) => Ok(Float(x + y)),
            (Num(n), r) => Self::from(&*n) + r,
            (l, Num(n)) => l + Self::from(&*n),
            (Str(mut l), Str(r)) => {
                Rc::make_mut(&mut l).push_str(&r);
                Ok(Str(l))
            }
            (Arr(mut l), Arr(r)) => {
                Rc::make_mut(&mut l).extend(r.iter().cloned());
                Ok(Arr(l))
            }
            (Obj(mut l), Obj(r)) => {
                Rc::make_mut(&mut l).extend(r.iter().map(|(k, v)| (k.clone(), v.clone())));
                Ok(Obj(l))
            }
            (l, r) => Err(Error::MathOp(l, MathOp::Add, r)),
        }
    }
}

impl core::ops::Sub for Val {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x - y)),
            (Float(f), Int(i)) => Ok(Float(f - i as f64)),
            (Int(i), Float(f)) => Ok(Float(i as f64 - f)),
            (Float(x), Float(y)) => Ok(Float(x - y)),
            (Num(n), r) => Self::from(&*n) - r,
            (l, Num(n)) => l - Self::from(&*n),
            (l, r) => Err(Error::MathOp(l, MathOp::Sub, r)),
        }
    }
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x * y)),
            (Float(f), Int(i)) | (Int(i), Float(f)) => Ok(Float(f * i as f64)),
            (Float(x), Float(y)) => Ok(Float(x * y)),
            (Num(n), r) => Self::from(&*n) * r,
            (l, Num(n)) => l * Self::from(&*n),
            (l, r) => Err(Error::MathOp(l, MathOp::Mul, r)),
        }
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Float(x as f64 / y as f64)),
            (Float(f), Int(i)) => Ok(Float(f / i as f64)),
            (Int(i), Float(f)) => Ok(Float(i as f64 / f)),
            (Float(x), Float(y)) => Ok(Float(x / y)),
            (Num(n), r) => Self::from(&*n) / r,
            (l, Num(n)) => l / Self::from(&*n),
            (l, r) => Err(Error::MathOp(l, MathOp::Div, r)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x % y)),
            (l, r) => Err(Error::MathOp(l, MathOp::Rem, r)),
        }
    }
}

impl core::ops::Neg for Val {
    type Output = ValR;
    fn neg(self) -> Self::Output {
        use Val::*;
        match self {
            Int(x) => Ok(Int(-x)),
            Float(x) => Ok(Float(-x)),
            Num(n) => -Self::from(&*n),
            x => Err(Error::Neg(x)),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Int(i), Self::Float(f)) | (Self::Float(f), Self::Int(i)) => {
                float_eq(&(*i as f64), f)
            }
            (Self::Float(x), Self::Float(y)) => float_eq(x, y),
            (Self::Num(x), Self::Num(y)) if Rc::ptr_eq(x, y) => true,
            (Self::Num(n), y) => &Self::from(&**n) == y,
            (x, Self::Num(n)) => x == &Self::from(&**n),
            (Self::Str(x), Self::Str(y)) => x == y,
            (Self::Arr(x), Self::Arr(y)) => x == y,
            (Self::Obj(x), Self::Obj(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for Val {}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Val {
    fn cmp(&self, other: &Self) -> Ordering {
        use Ordering::*;
        match (self, other) {
            (Self::Null, Self::Null) => Equal,
            (Self::Bool(x), Self::Bool(y)) => x.cmp(y),
            (Self::Int(x), Self::Int(y)) => x.cmp(y),
            (Self::Int(i), Self::Float(f)) => float_cmp(&(*i as f64), f),
            (Self::Float(f), Self::Int(i)) => float_cmp(f, &(*i as f64)),
            (Self::Float(x), Self::Float(y)) => float_cmp(x, y),
            (Self::Num(x), Self::Num(y)) if Rc::ptr_eq(x, y) => Equal,
            (Self::Num(n), y) => Self::from(&**n).cmp(y),
            (x, Self::Num(n)) => x.cmp(&Self::from(&**n)),
            (Self::Str(x), Self::Str(y)) => x.cmp(y),
            (Self::Arr(x), Self::Arr(y)) => x.cmp(y),
            (Self::Obj(x), Self::Obj(y)) => match (x.len(), y.len()) {
                (0, 0) => Equal,
                (0, _) => Less,
                (_, 0) => Greater,
                _ => {
                    let mut l: Vec<_> = x.iter().collect();
                    let mut r: Vec<_> = y.iter().collect();
                    l.sort_by_key(|(k, _v)| *k);
                    r.sort_by_key(|(k, _v)| *k);
                    // TODO: make this nicer
                    let kl = l.iter().map(|(k, _v)| k);
                    let kr = r.iter().map(|(k, _v)| k);
                    let vl = l.iter().map(|(_k, v)| v);
                    let vr = r.iter().map(|(_k, v)| v);
                    kl.cmp(kr).then_with(|| vl.cmp(vr))
                }
            },

            // nulls are smaller than anything else
            (Self::Null, _) => Less,
            (_, Self::Null) => Greater,
            // bools are smaller than anything else, except for nulls
            (Self::Bool(_), _) => Less,
            (_, Self::Bool(_)) => Greater,
            // numbers are smaller than anything else, except for nulls and bools
            (Self::Int(_) | Self::Float(_), _) => Less,
            (_, Self::Int(_) | Self::Float(_)) => Greater,
            // etc.
            (Self::Str(_), _) => Less,
            (_, Self::Str(_)) => Greater,
            (Self::Arr(_), _) => Less,
            (_, Self::Arr(_)) => Greater,
        }
    }
}

fn float_eq(left: &f64, right: &f64) -> bool {
    float_cmp(left, right) == Ordering::Equal
}

// adapted from the currently nightly-only function `f64::total_cmp`
fn float_cmp(left: &f64, right: &f64) -> Ordering {
    if *left == 0. && *right == 0. {
        return Ordering::Equal;
    }

    let mut left = left.to_bits() as i64;
    let mut right = right.to_bits() as i64;

    left ^= (((left >> 63) as u64) >> 1) as i64;
    right ^= (((right >> 63) as u64) >> 1) as i64;

    left.cmp(&right)
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => b.fmt(f),
            Self::Int(i) => i.fmt(f),
            Self::Float(x) => x.fmt(f),
            Self::Num(n) => n.fmt(f),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Arr(a) => {
                write!(f, "[")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    first.fmt(f)?
                };
                iter.try_for_each(|x| write!(f, ",{}", x))?;
                write!(f, "]")
            }
            Self::Obj(o) => {
                write!(f, "{{")?;
                let mut iter = o.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "\"{}\":{}", k, v)?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",\"{}\":{}", k, v))?;
                write!(f, "}}")
            }
        }
    }
}
