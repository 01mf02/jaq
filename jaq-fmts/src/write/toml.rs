//! TOML support.
use alloc::vec::Vec;
use bytes::Bytes;
use core::fmt::{self, Display, Formatter};
use jaq_json::{bstr, write_utf8, Map, Num, Val};

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

enum Tables<'a> {
    Table(Table<'a>),
    Array(Vec<Table<'a>>),
}

/// Whole TOML document.
pub type Root<'a> = Table<'a>;

/// Table key.
///
/// See https://toml.io/en/v1.1.0#keys.
struct Key<'a>(&'a Bytes);

/// Sequence of table keys.
struct Path<'a>(Vec<Key<'a>>);

/// Object.
#[derive(Default)]
pub struct Table<'a> {
    inlines: Vec<(Key<'a>, Inline<'a>)>,
    tables: Vec<(Key<'a>, Tables<'a>)>,
}

impl<'a> TryFrom<&'a Val> for Table<'a> {
    type Error = Error;
    fn try_from(v: &'a Val) -> Result<Self, Self::Error> {
        obj_table(val_obj(v).ok_or_else(|| Error::Root(v.clone()))?)
    }
}

enum Inline<'a> {
    Boolean(bool),
    Number(&'a Num),
    String(&'a Bytes),
    Array(Vec<Self>),
    Table(Vec<(Key<'a>, Self)>),
}

impl Display for Root<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write_table(&mut Path(Vec::new()), self, f)
    }
}

impl Display for Key<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let is_bare = |c: &u8| c.is_ascii_alphanumeric() || b"_-".contains(c);
        if self.0.iter().all(is_bare) {
            bstr(self.0).fmt(f)
        } else {
            Inline::String(self.0).fmt(f)
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

impl Display for Inline<'_> {
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

fn write_table<'a>(path: &mut Path<'a>, t: &Table<'a>, f: &mut Formatter) -> fmt::Result {
    t.inlines
        .iter()
        .try_for_each(|(k, v)| writeln!(f, "{k} = {v}"))?;
    let path_len = path.0.len();

    let mut newline = !t.inlines.is_empty() || !path.0.is_empty();
    let mut newline = || core::mem::replace(&mut newline, true);

    for (k, v) in &t.tables {
        path.0.push(Key(k.0));
        match v {
            Tables::Table(t) => {
                if newline() {
                    writeln!(f)?
                }
                writeln!(f, "[{path}]")?;
                write_table(path, t, f)?;
            }
            Tables::Array(a) => {
                for t in a {
                    if newline() {
                        writeln!(f)?
                    }
                    writeln!(f, "[[{path}]]")?;
                    write_table(path, t, f)?;
                }
            }
        }
        path.0.pop();
    }
    assert_eq!(path.0.len(), path_len);
    Ok(())
}

fn obj_table<'a>(o: &'a Map) -> Result<Table<'a>, Error> {
    let mut table = Table::default();
    for (k, v) in o.iter() {
        let k = val_key(k)?;
        match v {
            Val::Obj(o) => table.tables.push((k, Tables::Table(obj_table(o)?))),
            Val::Arr(a) => match a.iter().map(val_obj).collect::<Option<Vec<_>>>() {
                // all values in the array are objects
                Some(objs) if !objs.is_empty() => {
                    let tables = objs.iter().map(|t| obj_table(t));
                    let tables: Result<_, _> = tables.collect();
                    table.tables.push((k, Tables::Array(tables?)))
                }
                _ => table.inlines.push((k, val_inline(v)?)),
            },
            _ => table.inlines.push((k, val_inline(v)?)),
        }
    }
    Ok(table)
}

fn val_obj(v: &Val) -> Option<&Map> {
    match v {
        Val::Obj(o) => Some(o),
        _ => None,
    }
}

fn val_key(v: &Val) -> Result<Key<'_>, Error> {
    match v {
        Val::TStr(b) => Ok(Key(b)),
        _ => Err(Error::Key(v.clone()))?,
    }
}

fn val_inline<'a>(v: &'a Val) -> Result<Inline<'a>, Error> {
    let kvs = |(k, v)| Ok((val_key(k)?, val_inline(v)?));
    Ok(match v {
        Val::Null | Val::BStr(_) => Err(Error::Val(v.clone()))?,
        Val::Bool(b) => Inline::Boolean(*b),
        Val::TStr(b) => Inline::String(b),
        Val::Num(n) => Inline::Number(n),
        Val::Arr(a) => Inline::Array(a.iter().map(val_inline).collect::<Result<_, _>>()?),
        Val::Obj(o) => Inline::Table(o.iter().map(kvs).collect::<Result<_, _>>()?),
    })
}
