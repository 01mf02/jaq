//! JSON values with reference-counted sharing.

use crate::Error;
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::convert::{TryFrom, TryInto};
use core::fmt;
use fxhash::FxBuildHasher;
use indexmap::IndexMap;
pub use jaq_parse::{MathOp, OrdOp};

#[derive(Clone, Debug)]
pub enum Val {
    Null,
    /// Boolean
    Bool(bool),
    /// Positive integer
    Pos(usize),
    /// Negative integer
    Neg(usize),
    /// Floating-point value
    Float(f64),
    /// String
    Str(Rc<String>),
    /// Array
    Arr(Rc<Vec<Val>>),
    /// Order-preserving map
    Obj(Rc<IndexMap<String, Val, FxBuildHasher>>),
}

/// A value result.
pub type ValR = Result<Val, Error>;

/// A stream of values.
pub type Vals<'a> = Box<dyn Iterator<Item = Val> + 'a>;

/// A stream of value results.
pub type ValRs<'a> = Box<dyn Iterator<Item = ValR> + 'a>;

impl Val {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }

    pub fn as_usize(&self) -> Result<usize, Error> {
        match self {
            Self::Pos(p) => Ok(*p),
            _ => Err(Error::Usize(self.clone())),
        }
    }

    pub fn as_posneg(&self) -> Result<(usize, bool), Error> {
        match self {
            Self::Pos(p) => Ok((*p, true)),
            Self::Neg(n) => Ok((*n, false)),
            _ => Err(Error::Isize(self.clone())),
        }
    }

    pub fn as_obj_key(&self) -> Result<String, Error> {
        match self {
            Self::Str(s) => Ok(s.to_string()),
            _ => Err(Error::ObjKey(self.clone())),
        }
    }

    pub fn len(&self) -> Result<Self, Error> {
        match self {
            Self::Null => Ok(Self::Pos(0)),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Pos(l) | Self::Neg(l) => Ok(Self::Pos(*l)),
            Self::Float(f) => Ok(Self::Float(f.abs())),
            Self::Str(s) => Ok(Self::Pos(s.chars().count())),
            Self::Arr(a) => Ok(Self::Pos(a.len())),
            Self::Obj(o) => Ok(Self::Pos(o.keys().count())),
        }
    }

    pub fn typ(&self) -> &str {
        match self {
            Self::Null => "null",
            Self::Bool(_) => "boolean",
            Self::Pos(_) | Self::Neg(_) | Self::Float(_) => "number",
            Self::Str(_) => "string",
            Self::Arr(_) => "array",
            Self::Obj(_) => "object",
        }
    }

    pub fn round(&self, f: impl FnOnce(f64) -> f64) -> Result<Self, Error> {
        match self {
            Self::Pos(_) | Self::Neg(_) => Ok(self.clone()),
            Self::Float(x) => {
                let rounded = f(*x);
                if rounded < 0.0 {
                    Ok(Self::Neg(-rounded as usize))
                } else {
                    Ok(Self::Pos(rounded as usize))
                }
            }
            _ => Err(Error::Round(self.clone())),
        }
    }

    pub fn range(&self, other: &Self) -> Result<Box<dyn Iterator<Item = Self>>, Error> {
        match (self, other) {
            (Self::Pos(x), Self::Pos(y)) => Ok(Box::new((*x..*y).map(Self::Pos))),
            (Self::Neg(x), Self::Neg(y)) => Ok(Box::new((*y + 1..*x + 1).rev().map(Self::Neg))),
            (Self::Neg(_), Self::Pos(_)) => {
                let neg = self.range(&Self::Neg(0));
                let pos = Self::Pos(0).range(other);
                Ok(Box::new(neg?.chain(pos?)))
            }
            (Self::Pos(_), Self::Neg(_)) => Ok(Box::new(core::iter::empty())),
            _ => todo!(),
        }
    }

    pub fn keys(&self) -> Result<Vals, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new((0..a.len()).map(Val::Pos))),
            Self::Obj(o) => Ok(Box::new(o.keys().map(|k| Val::Str(Rc::new(k.clone()))))),
            _ => Err(Error::Keys(self.clone())),
        }
    }

    pub fn iter(&self) -> Result<Vals, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new(a.iter().cloned())),
            Self::Obj(o) => Ok(Box::new(o.values().cloned())),
            _ => Err(Error::Iter(self.clone())),
        }
    }
}

impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => match n.as_u64() {
                Some(p) => p.try_into().map_or(Self::Null, Self::Pos),
                None => match n.as_i64() {
                    Some(n) => (-n).try_into().map_or(Self::Null, Self::Neg),
                    None => match n.as_f64() {
                        Some(f) => Self::Float(f),
                        None => Self::Null,
                    },
                },
            },
            String(s) => Self::Str(Rc::new(s)),
            Array(a) => Self::Arr(Rc::new(a.into_iter().map(|x| x.into()).collect())),
            Object(o) => Self::Obj(Rc::new(o.into_iter().map(|(k, v)| (k, v.into())).collect())),
        }
    }
}

impl From<Val> for serde_json::Value {
    fn from(v: Val) -> serde_json::Value {
        use serde_json::Value::*;
        match v {
            Val::Null => Null,
            Val::Bool(b) => Bool(b),
            Val::Pos(p) => Number(p.into()),
            Val::Neg(n) => isize::try_from(n).map_or(Null, |n| Number((-n).into())),
            Val::Float(f) => serde_json::Number::from_f64(f).map_or(Null, Number),
            Val::Str(s) => String((*s).clone()),
            Val::Arr(a) => Array(a.iter().map(|x| x.clone().into()).collect()),
            Val::Obj(o) => Object(
                o.iter()
                    .map(|(k, v)| (k.clone(), v.clone().into()))
                    .collect(),
            ),
        }
    }
}

impl core::ops::Add for Val {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Ok(x),
            (Pos(x), Pos(y)) => Ok(Pos(x + y)),
            (Neg(x), Neg(y)) => Ok(Neg(x + y)),
            (Pos(s), Neg(l)) | (Neg(l), Pos(s)) if s < l => Ok(Neg(l - s)),
            (Pos(l), Neg(s)) | (Neg(s), Pos(l)) => Ok(Pos(l - s)),
            (Pos(p), Float(f)) | (Float(f), Pos(p)) => Ok(Float(f + p as f64)),
            (Neg(n), Float(f)) | (Float(f), Neg(n)) => Ok(Float(f - n as f64)),
            (Float(x), Float(y)) => Ok(Float(x + y)),
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
            (l, r) => Err(Error::MathOp(l, r, MathOp::Add)),
        }
    }
}

impl core::ops::Sub for Val {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(p), Neg(n)) => Ok(Pos(p + n)),
            (Neg(n), Pos(p)) => Ok(Neg(p + n)),
            (Pos(s), Pos(l)) | (Neg(l), Neg(s)) if s < l => Ok(Neg(l - s)),
            (Pos(l), Pos(s)) | (Neg(s), Neg(l)) => Ok(Pos(l - s)),
            (Pos(p), Float(f)) => Ok(Float(p as f64 - f)),
            (Neg(n), Float(f)) => Ok(Float(-(n as f64) - f)),
            (Float(f), Pos(p)) => Ok(Float(f - p as f64)),
            (Float(f), Neg(n)) => Ok(Float(f + n as f64)),
            (Float(x), Float(y)) => Ok(Float(x - y)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Sub)),
        }
    }
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) => Ok(Pos(x * y)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) => Ok(Neg(x * y)),
            (Pos(p), Float(f)) | (Float(f), Pos(p)) => Ok(Float(f * p as f64)),
            (Neg(n), Float(f)) | (Float(f), Neg(n)) => Ok(Float(-f * n as f64)),
            (Float(x), Float(y)) => Ok(Float(x * y)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Mul)),
        }
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) if x % y == 0 => Ok(Pos(x / y)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) if x % y == 0 => Ok(Neg(x / y)),
            (Pos(x), Pos(y)) | (Neg(x), Neg(y)) => Ok(Float(x as f64 / y as f64)),
            (Pos(x), Neg(y)) | (Neg(x), Pos(y)) => Ok(Float(-(x as f64 / y as f64))),
            (Pos(p), Float(f)) => Ok(Float(p as f64 / f)),
            (Neg(n), Float(f)) => Ok(Float(n as f64 / -f)),
            (Float(f), Pos(p)) => Ok(Float(f / p as f64)),
            (Float(f), Neg(n)) => Ok(Float(-f / n as f64)),
            (Float(x), Float(y)) => Ok(Float(x / y)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Div)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Pos(x), Pos(y) | Neg(y)) => Ok(Pos(x % y)),
            (Neg(x), Pos(y) | Neg(y)) => Ok(Neg(x % y)),
            (l, r) => Err(Error::MathOp(l, r, MathOp::Rem)),
        }
    }
}

impl core::ops::Neg for Val {
    type Output = ValR;
    fn neg(self) -> Self::Output {
        use Val::*;
        match self {
            Pos(x) => Ok(Neg(x)),
            Neg(x) => Ok(Pos(x)),
            Float(x) => Ok(Float(-x)),
            x => Err(Error::Neg(x)),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Pos(x), Self::Pos(y)) | (Self::Neg(x), Self::Neg(y)) => x == y,
            (Self::Pos(p), Self::Neg(n)) | (Self::Neg(n), Self::Pos(p)) => *p == 0 && *n == 0,
            (Self::Pos(p), Self::Float(f)) | (Self::Float(f), Self::Pos(p)) => {
                float_eq(&(*p as f64), f)
            }
            (Self::Neg(n), Self::Float(f)) | (Self::Float(f), Self::Neg(n)) => {
                float_eq(&-(*n as f64), f)
            }
            (Self::Float(x), Self::Float(y)) => float_eq(x, y),
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
            (Self::Pos(x), Self::Pos(y)) => x.cmp(y),
            (Self::Neg(x), Self::Neg(y)) => x.cmp(y).reverse(),
            (Self::Pos(p), Self::Neg(n)) | (Self::Neg(n), Self::Pos(p)) if *p + *n == 0 => Equal,
            (Self::Pos(_), Self::Neg(_)) => Greater,
            (Self::Neg(_), Self::Pos(_)) => Less,
            (Self::Pos(p), Self::Float(f)) => float_cmp(&(*p as f64), f),
            (Self::Neg(n), Self::Float(f)) => float_cmp(&-(*n as f64), f),
            (Self::Float(f), Self::Pos(p)) => float_cmp(f, &(*p as f64)),
            (Self::Float(f), Self::Neg(n)) => float_cmp(f, &-(*n as f64)),
            (Self::Float(x), Self::Float(y)) => float_cmp(x, y),
            (Self::Str(x), Self::Str(y)) => x.cmp(y),
            (Self::Arr(x), Self::Arr(y)) => x.cmp(y),
            (Self::Obj(x), Self::Obj(y)) => {
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

            // nulls are smaller than anything else
            (Self::Null, _) => Less,
            (_, Self::Null) => Greater,
            // bools are smaller than anything else, except for nulls
            (Self::Bool(_), _) => Less,
            (_, Self::Bool(_)) => Greater,
            // numbers are smaller than anything else, except for nulls and bools
            (Self::Pos(_) | Self::Neg(_) | Self::Float(_), _) => Less,
            (_, Self::Pos(_) | Self::Neg(_) | Self::Float(_)) => Greater,
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
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Pos(p) => write!(f, "{}", p),
            Self::Neg(n) => write!(f, "-{}", n),
            Self::Float(x) => write!(f, "{}", x),
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
                    write!(f, "{}:{}", k, v)?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",{}:{}", k, v))?;
                write!(f, "}}")
            }
        }
    }
}
