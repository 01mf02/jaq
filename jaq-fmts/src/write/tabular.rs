//! CSV and TSV support.
use core::fmt::{self, Display, Formatter};
use jaq_json::Val;
use std::io;

/// CSV/TSV row, i.e. a list of fields.
pub struct Row(Vec<Val>);

/// Serialisation error.
pub enum Error {
    /// invalid row value
    Row(Val),
    /// invalid field value
    Field(Val),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let field = "null, bool, number, or string";
        match self {
            Self::Field(v) => write!(f, "expected table field ({field}), found {v}"),
            Self::Row(v) => write!(f, "expected table row (array), found {v}"),
        }
    }
}

impl TryFrom<&Val> for Row {
    type Error = Error;
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        if let Val::Arr(a) = v {
            let iter = a.iter().map(|v| match v {
                Val::Null | Val::Bool(_) | Val::Num(_) | Val::TStr(_) => Ok(v.clone()),
                _ => Err(Error::Field(v.clone())),
            });
            iter.collect::<Result<_, _>>().map(Self)
        } else {
            Err(Error::Row(v.clone()))
        }
    }
}

macro_rules! write_field {
    ($w:ident, $v:ident, $fs:expr) => {{
        match $v {
            Val::Null => Ok(()),
            Val::TStr(s) => $fs(s),
            v => write!($w, "{v}"),
        }
    }};
}

macro_rules! write_row {
    ($w:ident, $v:ident, $delim:expr, $f:expr) => {{
        let mut iter = $v.0.iter();

        if let Some(v) = iter.next() {
            $f(v)?;
        }
        for v in iter {
            write!($w, "{}", $delim)?;
            $f(v)?;
        }
        Ok(())
    }};
}

fn write_csv_str(w: &mut dyn io::Write, b: &[u8]) -> io::Result<()> {
    use bstr::ByteSlice;
    write!(w, "\"")?;
    w.write_all(&b.replace(b"\"", b"\"\""))?;
    write!(w, "\"")
}

fn write_tsv_str(w: &mut dyn io::Write, b: &[u8]) -> io::Result<()> {
    let pats = ["\n", "\r", "\t", "\\", "\0"];
    let reps = ["\\n", "\\r", "\\t", "\\\\", "\\0"];
    let ac = aho_corasick::AhoCorasick::new(pats).unwrap();
    w.write_all(&ac.replace_all_bytes(b, &reps))
}

impl Row {
    /// Format array value as CSV row.
    pub fn write_csv(&self, w: &mut dyn io::Write) -> io::Result<()> {
        write_row!(w, self, ',', |v: &Val| write_field!(w, v, |s| {
            write_csv_str(w, s)
        }))
    }

    /// Format array value as TSV row.
    pub fn write_tsv(&self, w: &mut dyn io::Write) -> io::Result<()> {
        write_row!(w, self, '\t', |v: &Val| write_field!(w, v, |s| {
            write_tsv_str(w, s)
        }))
    }
}
