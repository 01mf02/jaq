//! TOML support.
use alloc::{string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};
use jaq_json::{Map, Num, Tag, Val};
use toml_edit::{DocumentMut, Formatted, Item, Table, Value};

/// Serialisation error.
#[derive(Debug)]
pub enum SError {
    /// non-string key in object
    Key(Val),
    /// non-table value as root
    Root(Val),
    /// null or too large integer as value
    Val(Val),
}

impl fmt::Display for SError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Key(v) => write!(f, "TOML keys must be strings, found: {v}"),
            Self::Root(v) => write!(f, "TOML root must be an object, found: {v}"),
            Self::Val(v) => write!(f, "could not encode {v} as TOML value"),
        }
    }
}

impl std::error::Error for SError {}

/// TOML root value.
pub struct Toml(DocumentMut);

impl Display for Toml {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl TryFrom<&Val> for Toml {
    type Error = SError;
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        let obj = val_obj(v).ok_or_else(|| SError::Root(v.clone()));
        obj.and_then(obj_table).map(DocumentMut::from).map(Self)
    }
}

fn obj_table(o: &Map) -> Result<Table, SError> {
    use jaq_std::ValT;
    let kvs = o.iter().map(|(k, v)| {
        let k = k.as_utf8_bytes().ok_or_else(|| SError::Key(k.clone()))?;
        Ok((String::from_utf8_lossy(k).into_owned(), val_item(v)?))
    });
    kvs.collect::<Result<_, _>>()
}

fn val_obj(v: &Val) -> Option<&Map> {
    match v {
        Val::Obj(o) => Some(o),
        _ => None,
    }
}

fn val_item(v: &Val) -> Result<Item, SError> {
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

fn val_value(v: &Val) -> Result<Value, SError> {
    let fail = || SError::Val(v.clone());
    Ok(match v {
        Val::Null | Val::Str(_, Tag::Bytes) => Err(fail())?,
        Val::Bool(b) => Value::Boolean(Formatted::new(*b)),
        Val::Str(s, Tag::Utf8) => {
            Value::String(Formatted::new(String::from_utf8_lossy(s).into_owned()))
        }
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
