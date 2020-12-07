//! JSON values with reference-counted sharing.

use fxhash::FxBuildHasher;
use indexmap::IndexMap;
use std::rc::Rc;

/// A map that preserves the order of its elements.
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

type Number = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Null,
    Bool(bool),
    // TODO: use a type that preserves numbers as long as possible
    Num(Number),
    Str(String),
    Arr(Vec<Rc<Val>>),
    Obj(FxIndexMap<String, Rc<Val>>),
}

/// A stream of reference-counted values.
pub type Vals<'a> = Box<dyn Iterator<Item = Rc<Val>> + 'a>;

impl Val {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn len(&self) -> Option<Number> {
        match self {
            Self::Null => Some(0.0),
            Self::Bool(_) => None,
            Self::Num(n) => Some(*n),
            Self::Str(s) => Some(s.chars().count() as Number),
            Self::Arr(a) => Some(a.len() as Number),
            Self::Obj(o) => Some(o.keys().count() as Number),
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

impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => Self::Num(n.as_f64().unwrap()),
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
            Val::Num(n) => Number(serde_json::Number::from_f64(n).unwrap()),
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
