use crate::{Num, Tag, Val};
use alloc::string::ToString;
use bytes::Bytes;
use jaq_std::ValT;

impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => Self::Num(Num::from_str(&n.to_string())),
            String(s) => Self::from(s),
            Array(a) => a.into_iter().map(Self::from).collect(),
            Object(o) => Self::obj(o.into_iter().map(|(k, v)| (k.into(), v.into())).collect()),
        }
    }
}

/// Deserialisation error.
pub enum DError {
    /// Number could not be parsed (likely out of bounds)
    Num(Num),
    /// Non-string key in object
    Key(Val),
    /// Raw string
    Raw(Bytes),
}

fn from_key(k: &Val) -> Result<&[u8], DError> {
    k.as_utf8_bytes().ok_or_else(|| DError::Key(k.clone()))
}

impl TryFrom<&Val> for serde_json::Value {
    type Error = DError;
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        let from_utf8 = |s| alloc::string::String::from_utf8_lossy(s).into_owned();
        use core::str::FromStr;
        use serde_json::Value::*;
        Ok(match v {
            Val::Null => Null,
            Val::Bool(b) => Bool(*b),
            Val::Num(Num::Int(i)) => Number((*i).into()),
            Val::Num(Num::Float(f)) => serde_json::Number::from_f64(*f).map_or(Null, Number),
            Val::Num(n) => Number(
                serde_json::Number::from_str(&n.to_string()).map_err(|_| DError::Num(n.clone()))?,
            ),
            Val::Str(s, Tag::Utf8) => String(from_utf8(&*s)),
            Val::Str(s, Tag::Bytes) => String(s.iter().copied().map(char::from).collect()),
            Val::Str(s, Tag::Inline) => Err(DError::Raw(s.clone()))?,
            Val::Arr(a) => Array(a.iter().map(TryInto::try_into).collect::<Result<_, _>>()?),
            Val::Obj(o) => Object(
                o.iter()
                    .map(|(k, v)| Ok((from_utf8(from_key(k)?), v.try_into()?)))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}
