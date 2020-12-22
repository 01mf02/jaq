//! JSON values with reference-counted sharing.

use crate::map::Map;
use crate::number::Num;
use std::convert::TryFrom;
use std::rc::Rc;

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
pub type Vals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

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

    pub fn len(&self) -> Option<Num> {
        match self {
            Self::Null => Some(0.into()),
            Self::Bool(_) => None,
            Self::Num(n) => Some(*n),
            Self::Str(s) => Some(s.chars().count().into()),
            Self::Arr(a) => Some(a.len().into()),
            Self::Obj(o) => Some(o.keys().count().into()),
        }
    }

    pub fn iter(&self) -> Option<Vals> {
        match self {
            Self::Arr(a) => Some(Box::new(a.iter().cloned())),
            Self::Obj(o) => Some(Box::new(o.values().cloned())),
            _ => None,
        }
    }
}

impl From<Val> for Vals<'_> {
    fn from(v: Val) -> Self {
        Box::new(core::iter::once(Rc::new(v)))
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
