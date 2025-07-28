//! TOML decoding.
use crate::{Map, Num, Val};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::{self, Display, Formatter};
use toml_edit::{Document, DocumentMut, Formatted, Item, Table, Value};

/// Encoding error.
#[derive(Debug)]
pub enum EError {
    /// non-string key in object
    Key(Val),
    /// non-table value as root
    Root(Val),
    /// null or too large integer as value
    Val(Val),
}

impl fmt::Display for EError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Key(v) => write!(f, "TOML keys must be strings, found: {v}"),
            Self::Root(v) => write!(f, "TOML root must be an object, found: {v}"),
            Self::Val(v) => write!(f, "could not encode {v} as TOML value"),
        }
    }
}

/// Decoding error.
#[derive(Debug)]
pub struct DError(toml_edit::TomlError);

impl From<toml_edit::TomlError> for DError {
    fn from(e: toml_edit::TomlError) -> Self {
        Self(e)
    }
}

impl fmt::Display for DError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for DError {}

#[cfg(feature = "std")]
impl std::error::Error for EError {}

fn value(v: Value) -> Result<Val, DError> {
    Ok(match v {
        Value::String(s) => Val::Str(s.into_value().into()),
        Value::Integer(i) => Val::Num(Num::from_integral(i.into_value())),
        Value::Float(f) => Val::Num(Num::Float(f.into_value())),
        Value::Boolean(b) => Val::Bool(b.into_value()),
        Value::Array(a) => return a.into_iter().map(value).collect(),
        Value::InlineTable(t) => return table(t.into_table()),
        Value::Datetime(d) => Val::Str(d.into_value().to_string().into()),
    })
}

fn item(item: Item) -> Result<Val, DError> {
    match item {
        // TODO: what is this? can this be triggered?
        Item::None => panic!(),
        Item::Value(v) => value(v),
        Item::Table(t) => table(t),
        Item::ArrayOfTables(a) => a.into_array().into_iter().map(value).collect(),
    }
}

fn table(t: Table) -> Result<Val, DError> {
    t.into_iter()
        .map(|(k, v)| Ok((k.into(), item(v)?)))
        .collect::<Result<_, _>>()
        .map(Val::obj)
}

fn obj_table(o: &Map) -> Result<Table, EError> {
    use jaq_core::ValT;
    let kvs = o.iter().map(|(k, v)| {
        let k = k.as_str().ok_or_else(|| EError::Key(k.clone()))?;
        Ok((k, val_item(v)?))
    });
    kvs.collect::<Result<_, _>>()
}

fn val_obj(v: &Val) -> Option<&Map> {
    match v {
        Val::Obj(o) => Some(o),
        _ => None,
    }
}

fn val_item(v: &Val) -> Result<Item, EError> {
    if let Val::Obj(o) = v {
        return obj_table(o).map(Item::Table);
    } else if let Val::Arr(a) = v {
        if let Some(objs) = a.iter().map(val_obj).collect::<Option<Vec<_>>>() {
            let tables = objs.into_iter().map(obj_table);
            return tables.collect::<Result<_, _>>().map(Item::ArrayOfTables);
        }
    }
    val_value(v).map(Item::Value)
}

fn val_value(v: &Val) -> Result<Value, EError> {
    let fail = || EError::Val(v.clone());
    Ok(match v {
        Val::Null => Err(fail())?,
        Val::Bool(b) => Value::Boolean(Formatted::new(*b)),
        Val::Str(s) => Value::String(Formatted::new((**s).clone())),
        Val::Bin(b) => Value::String(Formatted::new(
            b.iter().copied().map(char::from).collect::<String>(),
        )),
        Val::Num(Num::Float(f)) => Value::Float(Formatted::new(*f)),
        Val::Num(Num::Dec(n)) => val_value(&Val::Num(Num::from_dec_str(n)))?,
        Val::Num(n @ (Num::Int(_) | Num::BigInt(_))) => {
            let from_int = |n: &Num| n.as_isize()?.try_into().ok();
            Value::Integer(Formatted::new(from_int(n).ok_or_else(fail)?))
        }
        Val::Arr(a) => a
            .iter()
            .map(val_value)
            .collect::<Result<_, _>>()
            .map(Value::Array)?,
        Val::Obj(o) => obj_table(o).map(|t| Value::InlineTable(Table::into_inline_table(t)))?,
    })
}

/// Decode a TOML document from a string.
pub fn decode_str(s: &str) -> Result<Val, DError> {
    table(s.parse::<Document<String>>()?.into_table())
}

/// Encode a value as a TOML document.
pub fn encode_val(v: &Val) -> Result<impl Display, EError> {
    let obj = val_obj(v).ok_or_else(|| EError::Root(v.clone()));
    obj.and_then(obj_table).map(DocumentMut::from)
}
