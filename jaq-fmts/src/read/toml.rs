//! TOML support.
use alloc::string::{String, ToString};
use jaq_json::{Num, Tag, Val};
use toml_edit::{Document, Item, Table, TomlError as Error, Value};

/// Parse a TOML document from a string.
pub fn parse(s: &str) -> Result<Val, Error> {
    table(s.parse::<Document<String>>()?.into_table())
}

fn value(v: Value) -> Result<Val, Error> {
    Ok(match v {
        Value::String(s) => Val::Str(s.into_value().into(), Tag::Utf8),
        Value::Integer(i) => Val::Num(Num::from_integral(i.into_value())),
        Value::Float(f) => Val::Num(Num::Float(f.into_value())),
        Value::Boolean(b) => Val::Bool(b.into_value()),
        Value::Array(a) => return a.into_iter().map(value).collect(),
        Value::InlineTable(t) => return table(t.into_table()),
        Value::Datetime(d) => Val::Str(d.into_value().to_string().into(), Tag::Utf8),
    })
}

fn item(item: Item) -> Result<Val, Error> {
    match item {
        // TODO: what is this? can this be triggered?
        Item::None => panic!(),
        Item::Value(v) => value(v),
        Item::Table(t) => table(t),
        Item::ArrayOfTables(a) => a.into_array().into_iter().map(value).collect(),
    }
}

fn table(t: Table) -> Result<Val, Error> {
    t.into_iter()
        .map(|(k, v)| Ok((k.into(), item(v)?)))
        .collect::<Result<_, _>>()
        .map(Val::obj)
}
