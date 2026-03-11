//! CSV and TSV support.
use crate::invalid_data;
use core::iter;
use jaq_json::{Map, Val};

fn fail<T>(msg: String) -> Result<T, std::io::Error> {
    Err(invalid_data(msg))
}

/// Writer for CSV files.
pub fn csv(
    w: &mut dyn std::io::Write,
    prefix_delimiter: bool,
    field: &Val,
) -> Result<(), std::io::Error> {
    if prefix_delimiter {
        write!(w, ",")?;
    }
    match field {
        Val::Null => Ok(()),
        Val::Bool(b) => write!(w, "{b}"),
        Val::Num(num) => write!(w, "{num}"),
        Val::TStr(bytes) => {
            if bytes.iter().any(|b| *b == b'"' || *b == b',') {
                write!(w, "\"")?;
                for byte in bytes.iter().copied() {
                    if byte == b'"' {
                        write!(w, "\"\"")?;
                    } else {
                        w.write(&[byte])?;
                    }
                }
                write!(w, "\"")?;
                Ok(())
            } else {
                w.write(bytes)?;
                Ok(())
            }
        }
        _ => fail(format!(
            "CSV field was wrong type, expected null, bool, number, or string, got {}",
            field
        )),
    }
}
/// Writer for TSV files.
pub fn tsv(
    w: &mut dyn std::io::Write,
    prefix_delimiter: bool,
    field: &Val,
) -> Result<(), std::io::Error> {
    if prefix_delimiter {
        write!(w, "\t")?;
    }
    match field {
        Val::Null => Ok(()),
        Val::Bool(b) => write!(w, "{b}"),
        Val::Num(num) => write!(w, "{num}"),
        Val::TStr(bytes) => {
            if bytes.iter().any(|b| *b == b'"' || *b == b',') {
                write!(w, "\"")?;
                for byte in bytes.iter().copied() {
                    if byte == b'"' {
                        write!(w, "\"\"")?;
                    } else {
                        w.write(&[byte])?;
                    }
                }
                write!(w, "\"")?;
                Ok(())
            } else {
                w.write(bytes)?;
                Ok(())
            }
        }
        _ => fail(format!(
            "CSV field was wrong type, expected null, bool, number, or string, got {}",
            field
        )),
    }
}

fn write_row<'a>(
    w: &mut dyn std::io::Write,
    fields: impl Iterator<Item = &'a Val>,
    f: impl Fn(&mut dyn std::io::Write, bool, &Val) -> Result<(), std::io::Error>,
    newline: bool,
) -> Result<(), std::io::Error> {
    if newline {
        writeln!(w)?;
    }
    let mut delim = false;
    for field in fields {
        f(w, delim, field)?;
        delim = true;
    }
    Ok(())
}

fn write_objects<'a>(
    w: &mut dyn std::io::Write,
    f: &impl Fn(&mut dyn std::io::Write, bool, &Val) -> Result<(), std::io::Error>,
    map: &Map,
    records: impl Iterator<Item = &'a Val>,
) -> Result<(), std::io::Error> {
    let ks: Vec<Val> = map.keys().cloned().collect();
    write_row(w, ks.iter(), f, false)?;
    for val in records {
        match val {
            Val::Obj(map) if map.len() == ks.len() && ks.iter().all(|k| map.contains_key(k)) => {
                write_row(w, ks.iter().map(|k| &map[k]), f, true)?
            }
            _ => return fail(format!("Wanted object for table row, found {val}")),
        }
    }
    Ok(())
}

fn write_arrays<'a>(
    w: &mut dyn std::io::Write,
    f: &impl Fn(&mut dyn std::io::Write, bool, &Val) -> Result<(), std::io::Error>,
    records: impl Iterator<Item = &'a Val>,
) -> Result<(), std::io::Error> {
    let mut newline = false;
    for val in records {
        match val {
            Val::Arr(fields) => {
                write_row(w, fields.iter(), f, newline)?;
                newline = true;
            }
            _ => return fail(format!("Expected array of tabular data, got {val}")),
        }
    }
    Ok(())
}

/// Using a specified field writer, write tabular data to output.
pub fn write(
    w: &mut dyn std::io::Write,
    f: &impl Fn(&mut dyn std::io::Write, bool, &Val) -> Result<(), std::io::Error>,
    v: &Val,
) -> Result<(), std::io::Error> {
    match v {
        Val::Arr(vals) => match vals.get(0) {
            Some(Val::Obj(map)) => write_objects(w, f, map, vals.iter()),
            Some(Val::Arr(_)) => write_arrays(w, f, vals.iter()),
            Some(_) => write_arrays(w, f, iter::once(v)),
            None => Ok(()),
        },
        Val::Obj(map) => write_objects(w, f, map, iter::once(v)),
        _ => fail(format!(
            "Expected an array of records or single record for tabular output, got {v}"
        )),
    }
}
