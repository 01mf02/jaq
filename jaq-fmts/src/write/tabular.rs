//! CSV and TSV support.
use crate::invalid_data;
use jaq_json::Val;
use std::io::Write;

type Result<T = (), E = std::io::Error> = core::result::Result<T, E>;

type WriteFn<T> = fn(&mut dyn Write, &T) -> Result;

fn write_value(w: &mut dyn Write, v: &Val, f: WriteFn<[u8]>) -> Result {
    let fail = || format!("expected CSV/TSV field (null, bool, number, or string), found {v}");
    match v {
        Val::Null => Ok(()),
        Val::Bool(_) | Val::Num(_) => write!(w, "{v}"),
        Val::TStr(b) => f(w, b),
        _ => Err(invalid_data(fail())),
    }
}

fn write_csv_str(w: &mut dyn Write, b: &[u8]) -> Result {
    use bstr::ByteSlice;
    write!(w, "\"")?;
    w.write_all(&b.replace(b"\"", b"\"\""))?;
    write!(w, "\"")
}

fn write_tsv_str(w: &mut dyn Write, b: &[u8]) -> Result {
    w.write_all(&jaq_std::escape_tsv(b))
}

fn write_row<'a>(w: &mut dyn Write, v: &Val, delim: char, f: WriteFn<Val>) -> Result {
    let fail = || format!("expected CSV/TSV row (array), got {v}");
    let mut iter = match v {
        Val::Arr(a) => a.iter(),
        _ => Err(invalid_data(fail()))?,
    };

    if let Some(v) = iter.next() {
        f(w, v)?;
    }
    for v in iter {
        write!(w, "{delim}")?;
        f(w, v)?;
    }
    writeln!(w)
}

/// Format array value as CSV row.
pub fn write_csv_row(w: &mut dyn Write, v: &Val) -> Result {
    write_row(w, v, ',', |w, v| write_value(w, v, write_csv_str))
}

/// Format array value as TSV row.
pub fn write_tsv_row(w: &mut dyn Write, v: &Val) -> Result {
    write_row(w, v, ',', |w, v| write_value(w, v, write_tsv_str))
}
