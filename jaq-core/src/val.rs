//! JSON values with reference-counted sharing.

use crate::{Error, RVals};
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::convert::{TryFrom, TryInto};
use core::fmt;
use fxhash::FxBuildHasher;
use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub enum Val {
    Null,
    Bool(bool),
    Pos(usize),
    Neg(usize),
    Float(f64),
    Str(String),
    Arr(Vec<Rc<Val>>),
    /// A map that preserves the order of its elements.
    Obj(IndexMap<String, Rc<Val>, FxBuildHasher>),
}

#[derive(Clone, Debug)]
pub enum Atom {
    Pos(usize),
    Neg(usize),
    Float(f64),
    Str(String),
}

impl Val {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }

    pub fn as_posneg(&self) -> Result<(usize, bool), Error> {
        match self {
            Self::Pos(p) => Ok((*p, true)),
            Self::Neg(n) => Ok((*n, false)),
            _ => Err(Error::Isize(self.clone())),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        self.as_str().map(|s| s.to_string())
    }

    pub fn as_obj_key(&self) -> Result<String, Error> {
        self.as_string().ok_or_else(|| Error::ObjKey(self.clone()))
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

    pub fn iter(&self) -> Result<RVals, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new(a.iter().cloned())),
            Self::Obj(o) => Ok(Box::new(o.values().cloned())),
            _ => Err(Error::Iter(self.clone())),
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
}

impl From<Atom> for Val {
    fn from(a: Atom) -> Self {
        match a {
            Atom::Pos(p) => Self::Pos(p),
            Atom::Neg(n) => Self::Neg(n),
            Atom::Float(f) => Self::Float(f),
            Atom::Str(s) => Self::Str(s),
        }
    }
}

impl TryFrom<&Val> for usize {
    type Error = Error;
    fn try_from(v: &Val) -> Result<usize, Error> {
        match v {
            Val::Pos(n) => Ok(*n),
            _ => Err(Error::Usize(v.clone())),
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
                Some(p) => Self::Pos(p.try_into().unwrap()),
                None => match n.as_i64() {
                    Some(n) => Self::Neg((-n).try_into().unwrap()),
                    None => match n.as_f64() {
                        Some(f) => Self::Float(f),
                        _ => todo!(),
                    },
                },
            },
            String(s) => Self::Str(s),
            Array(a) => Self::Arr(a.into_iter().map(|x| Rc::new(x.into())).collect()),
            Object(o) => Self::Obj(o.into_iter().map(|(k, v)| (k, Rc::new(v.into()))).collect()),
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
            Val::Neg(n) => Number(serde_json::Number::from(-isize::try_from(n).unwrap())),
            Val::Float(f) => Number(serde_json::Number::from_f64(f).unwrap()),
            Val::Str(s) => String(s),
            Val::Arr(a) => Array(a.into_iter().map(|x| (*x).clone().into()).collect()),
            Val::Obj(o) => Object(
                o.into_iter()
                    .map(|(k, v)| (k, (*v).clone().into()))
                    .collect(),
            ),
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
            // this behaviour is more like jq:
            /*
            (Self::Pos(p), Self::Float(f)) | (Self::Float(f), Self::Pos(p)) => *p as f64 == *f,
            (Self::Neg(n), Self::Float(f)) | (Self::Float(f), Self::Neg(n)) => -(*n as f64) == *f,
            */
            (Self::Float(x), Self::Float(y)) => x == y,
            (Self::Str(x), Self::Str(y)) => x == y,
            (Self::Arr(x), Self::Arr(y)) => x == y,
            (Self::Obj(x), Self::Obj(y)) => x == y,
            _ => false,
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Ordering::*;
        match (self, other) {
            (Self::Null, Self::Null) => Some(Equal),
            (Self::Null, _) => Some(Less),
            (_, Self::Null) => Some(Greater),
            (Self::Bool(x), Self::Bool(y)) => x.partial_cmp(y),
            (Self::Bool(_), _) => Some(Less),
            (_, Self::Bool(_)) => Some(Greater),
            (Self::Pos(x), Self::Pos(y)) => x.partial_cmp(y),
            (Self::Neg(x), Self::Neg(y)) => x.partial_cmp(y).map(Ordering::reverse),
            (Self::Pos(_), Self::Neg(_)) => Some(Greater),
            (Self::Neg(_), Self::Pos(_)) => Some(Less),
            (Self::Pos(p), Self::Float(f)) => (*p as f64).partial_cmp(f),
            (Self::Neg(n), Self::Float(f)) => (-(*n as f64)).partial_cmp(f),
            (Self::Float(f), Self::Pos(p)) => f.partial_cmp(&(*p as f64)),
            (Self::Float(f), Self::Neg(n)) => f.partial_cmp(&-(*n as f64)),
            (Self::Pos(_) | Self::Neg(_) | Self::Float(_), _) => Some(Less),
            (_, Self::Pos(_) | Self::Neg(_) | Self::Float(_)) => Some(Greater),
            (Self::Str(x), Self::Str(y)) => x.partial_cmp(y),
            (Self::Str(_), _) => Some(Less),
            (_, Self::Str(_)) => Some(Greater),
            (Self::Arr(x), Self::Arr(y)) => x.partial_cmp(y),
            (Self::Arr(_), _) => Some(Less),
            (_, Self::Arr(_)) => Some(Greater),
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
                match kl.cmp(kr) {
                    Ordering::Equal => vl.partial_cmp(vr),
                    ord => Some(ord),
                }
            }
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "boolean ({})", b),
            Self::Pos(p) => write!(f, "number ({})", p),
            Self::Neg(n) => write!(f, "number (-{})", n),
            Self::Float(x) => write!(f, "number ({})", x),
            Self::Str(s) => write!(f, "string (\"{}\")", s),
            Self::Arr(a) => {
                write!(f, "array ([")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    first.fmt(f)?
                };
                iter.try_for_each(|x| write!(f, ",{}", x))?;
                write!(f, "])")
            }
            Self::Obj(o) => {
                write!(f, "object ({{")?;
                let mut iter = o.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{}:{}", k, v)?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",{}:{}", k, v))?;
                write!(f, "}})")
            }
        }
    }
}
