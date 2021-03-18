//! JSON values with reference-counted sharing.

use crate::map::Map;
use crate::number::Num;
use crate::Error;
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryFrom;
use core::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Val {
    Null,
    Bool(bool),
    Num(Num),
    Str(String),
    Arr(Vec<Rc<Val>>),
    Obj(Map<String, Rc<Val>>),
}

#[derive(Clone, Debug)]
pub enum Atom {
    Null,
    Bool(bool),
    Num(Num),
    Str(String),
}

/// A stream of reference-counted values.
pub type RVals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

/// A reference-counted value result.
pub type RValR = Result<Rc<Val>, Error>;

/// A stream of reference-counted value results.
pub type RValRs<'a> = Box<dyn Iterator<Item = RValR> + 'a>;

impl Val {
    pub fn as_isize(&self) -> Option<isize> {
        match self {
            Self::Num(n) => n.to_isize(),
            _ => None,
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

    pub fn len(&self) -> Result<Num, Error> {
        match self {
            Self::Null => Ok(0.into()),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Num(n) => Ok(*n),
            Self::Str(s) => Ok(s.chars().count().into()),
            Self::Arr(a) => Ok(a.len().into()),
            Self::Obj(o) => Ok(o.keys().count().into()),
        }
    }

    pub fn iter(&self) -> Result<RVals, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new(a.iter().cloned())),
            Self::Obj(o) => Ok(Box::new(o.values().cloned())),
            _ => Err(Error::Iter(self.clone())),
        }
    }
}

impl From<Atom> for Val {
    fn from(a: Atom) -> Self {
        match a {
            Atom::Null => Self::Null,
            Atom::Bool(b) => Self::Bool(b),
            Atom::Num(n) => Self::Num(n),
            Atom::Str(s) => Self::Str(s),
        }
    }
}

impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => Self::Num(Num::try_from(n).unwrap()),
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
            Val::Num(n) => Number(serde_json::Number::try_from(n).unwrap()),
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

impl fmt::Display for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "boolean ({})", b),
            Self::Num(n) => write!(f, "number ({})", n),
            Self::Str(s) => write!(f, "string (\"{}\")", s),
            _ => todo!(),
        }
    }
}
