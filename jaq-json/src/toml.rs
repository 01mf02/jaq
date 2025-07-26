//! TOML decoding.
use crate::{Num, Val};
use alloc::string::String;
use core::fmt::{self, Formatter};
use toml_edit::{Document, Item, Table, Value};

/// Decoding error.
#[derive(Debug)]
pub enum DError {
    /// date-time value encountered (not supported in jaq right now)
    Datetime,
    /// parse error
    Parse(PError),
}

/// Parse error.
#[derive(Debug)]
pub struct PError(toml_edit::TomlError);

impl From<toml_edit::TomlError> for DError {
    fn from(e: toml_edit::TomlError) -> Self {
        Self::Parse(PError(e))
    }
}

impl fmt::Display for DError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Parse(e) => e.0.fmt(f),
            Self::Datetime => write!(f, "date-time values not supported in jaq"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for DError {}

fn value(v: Value) -> Result<Val, DError> {
    Ok(match v {
        Value::String(s) => Val::Str(s.into_value().into()),
        Value::Integer(i) => Val::Num(Num::from_integral(i.into_value())),
        Value::Float(f) => Val::Num(Num::Float(f.into_value())),
        Value::Boolean(b) => Val::Bool(b.into_value()),
        Value::Array(a) => return a.into_iter().map(value).collect(),
        Value::InlineTable(t) => return table(t.into_table()),
        Value::Datetime(_) => Err(DError::Datetime)?,
    })
}

fn item(item: Item) -> Result<Val, DError> {
    match item {
        Item::None => Ok(Val::obj(Default::default())),
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

/// Decode a TOML document from a string.
pub fn decode_str(s: &str) -> Result<Val, DError> {
    let doc = s.parse::<Document<String>>()?;
    table(doc.into_table())
}
