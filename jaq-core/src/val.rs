//! JSON values with reference-counted sharing.

use crate::{Error, Map, Num, RVals};
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::{TryFrom, TryInto};
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
    Num(Num),
    Str(String),
}

impl Val {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }

    pub fn as_isize(&self) -> Result<isize, Error> {
        match self {
            Self::Num(n) => n.to_isize().ok_or_else(|| Error::Isize(self.clone())),
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

    pub fn len(&self) -> Result<Num, Error> {
        match self {
            Self::Null => Ok(0_usize.into()),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Num(n) => Ok(*n),
            Self::Str(s) => Ok(s.chars().count().into()),
            Self::Arr(a) => Ok(a.len().into()),
            Self::Obj(o) => Ok(o.keys().count().into()),
        }
    }

    pub fn is_empty(&self) -> Result<bool, Error> {
        match self {
            Self::Null => Ok(true),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Num(n) => Ok(n.is_zero()),
            Self::Str(s) => Ok(s.is_empty()),
            Self::Arr(a) => Ok(a.is_empty()),
            Self::Obj(o) => Ok(o.is_empty()),
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
            Self::Num(_) => "number",
            Self::Str(_) => "string",
            Self::Arr(_) => "array",
            Self::Obj(_) => "object",
        }
    }
}

impl From<Atom> for Val {
    fn from(a: Atom) -> Self {
        match a {
            Atom::Num(n) => Self::Num(n),
            Atom::Str(s) => Self::Str(s),
        }
    }
}

impl TryFrom<&Val> for usize {
    type Error = Error;
    fn try_from(v: &Val) -> Result<usize, Error> {
        match v {
            Val::Num(n) => n.try_into().map_err(|_| Error::Usize(v.clone())),
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
            Self::Arr(a) => {
                write!(f, "array ([")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    first.fmt(f)?
                };
                iter.try_for_each(|x| write!(f, ",{}", x))?;
                write!(f, "])")
            }
            Self::Obj(o) => write!(f, "object ({})", o),
        }
    }
}
