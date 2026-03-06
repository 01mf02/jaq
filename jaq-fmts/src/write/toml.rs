//! TOML support.
use alloc::vec::Vec;
use bytes::Bytes;
use core::fmt::{self, Display, Formatter};
use jaq_json::{bstr, write_utf8, Num, Val};

/// Serialisation error.
#[derive(Debug)]
pub enum Error {
    /// non-string key in object
    Key(Val),
    /// non-table value as root
    Root(Val),
    /// null or byte string
    Val(Val),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Key(v) => write!(f, "TOML keys must be strings, found: {v}"),
            Self::Root(v) => write!(f, "TOML root must be an object, found: {v}"),
            Self::Val(v) => write!(f, "could not encode {v} as TOML value"),
        }
    }
}

impl std::error::Error for Error {}

/// Whole TOML document.
pub struct Root<'a>(Table<'a>);

/// Table key.
///
/// See https://toml.io/en/v1.1.0#keys.
struct Key<'a>(&'a Bytes);

/// Sequence of table keys.
struct Path<'a>(Vec<Key<'a>>);

/// Object.
type Table<'a> = Vec<(Key<'a>, Value<'a>)>;

impl<'a> TryFrom<&'a Val> for Root<'a> {
    type Error = Error;
    fn try_from(v: &'a Val) -> Result<Self, Self::Error> {
        match val_value(v)? {
            Value::Table(t) => Ok(Root(t)),
            _ => Err(Error::Root(v.clone())),
        }
    }
}

enum Value<'a> {
    Boolean(bool),
    Number(&'a Num),
    String(&'a Bytes),
    Array(Vec<Self>),
    Table(Table<'a>),
}

impl Display for Root<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write_table(&mut Path(Vec::new()), &self.0, f)
    }
}

impl Display for Key<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let is_bare = |c: &u8| c.is_ascii_alphanumeric() || b"_-".contains(c);
        if self.0.iter().all(is_bare) {
            bstr(self.0).fmt(f)
        } else {
            Value::String(self.0).fmt(f)
        }
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut iter = self.0.iter();
        if let Some(x) = iter.next() {
            x.fmt(f)?;
        }
        iter.try_for_each(|k| write!(f, ".{k}"))
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Boolean(b) => b.fmt(f),
            Self::Number(Num::Float(f64::INFINITY)) => "inf".fmt(f),
            Self::Number(Num::Float(f64::NEG_INFINITY)) => "-inf".fmt(f),
            Self::Number(Num::Float(n)) if n.is_nan() => "nan".fmt(f),
            Self::Number(n) => n.fmt(f),
            Self::String(s) => write_utf8!(f, s, |part| write!(f, "{}", bstr(part))),
            Self::Array(a) => {
                "[".fmt(f)?;
                let mut iter = a.iter();
                if let Some(x) = iter.next() {
                    x.fmt(f)?;
                }
                iter.try_for_each(|x| write!(f, ", {x}"))?;
                "]".fmt(f)
            }
            Self::Table(t) => {
                "{".fmt(f)?;
                let mut iter = t.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{k} = {v}")?
                }
                iter.try_for_each(|(k, v)| write!(f, ", {k} = {v}"))?;
                "}".fmt(f)
            }
        }
    }
}

fn write_table<'a>(path: &mut Path<'a>, t: &'a Table, f: &mut Formatter) -> fmt::Result {
    enum Container {
        Table,
        Array,
    }
    let mut tables: Vec<(Container, &Key, &Table)> = Vec::new();

    let mut newline = !path.0.is_empty();
    let mut write_inline = |k, v| {
        newline = true;
        writeln!(f, "{k} = {v}")
    };

    for (k, v) in t.iter() {
        match v {
            Value::Table(o) => tables.push((Container::Table, k, o)),
            Value::Array(a) => match a.iter().map(Value::get_table).collect::<Option<Vec<_>>>() {
                // all values in the array are tables
                Some(objs) if !objs.is_empty() => {
                    tables.extend(objs.iter().map(|o| (Container::Array, k, *o)))
                }
                _ => write_inline(k, v)?,
            },
            _ => write_inline(k, v)?,
        }
    }

    let path_len = path.0.len();
    for (container, k, t) in tables {
        path.0.push(Key(k.0));
        if core::mem::replace(&mut newline, true) {
            writeln!(f)?
        }
        match container {
            Container::Table => writeln!(f, "[{path}]")?,
            Container::Array => writeln!(f, "[[{path}]]")?,
        }
        write_table(path, t, f)?;
        path.0.pop();
    }
    assert_eq!(path.0.len(), path_len);
    Ok(())
}

impl Value<'_> {
    fn get_table(&self) -> Option<&Table<'_>> {
        match self {
            Value::Table(t) => Some(t),
            _ => None,
        }
    }
}

fn val_key(v: &Val) -> Result<Key<'_>, Error> {
    match v {
        Val::TStr(b) => Ok(Key(b)),
        _ => Err(Error::Key(v.clone()))?,
    }
}

fn val_value<'a>(v: &'a Val) -> Result<Value<'a>, Error> {
    let kvs = |(k, v)| Ok((val_key(k)?, val_value(v)?));
    Ok(match v {
        Val::Null | Val::BStr(_) => Err(Error::Val(v.clone()))?,
        Val::Bool(b) => Value::Boolean(*b),
        Val::TStr(b) => Value::String(b),
        Val::Num(n) => Value::Number(n),
        Val::Arr(a) => Value::Array(a.iter().map(val_value).collect::<Result<_, _>>()?),
        Val::Obj(o) => Value::Table(o.iter().map(kvs).collect::<Result<_, _>>()?),
    })
}
