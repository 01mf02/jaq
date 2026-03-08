//! CSV support.
use crate::invalid_data;
use core::fmt::{self, Display, Formatter};
use jaq_json::Val;

/// Serialisation error.
#[derive(Debug)]
pub enum Error {
    /// asked to serialise non-array
    NotArray(Val),
    /// record wasn't an array or object
    RowScalar(Val),
    /// record wasn't an object with the correct keys
    RowNotObject(Vec<Val>, Val),
    /// record wasn't an object with the correct keys
    RowWrongLength(usize, Val),
    /// field had wrong type
    FieldType(Val),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::NotArray(val) => write!(f, "CSV data must be an array, got: {val}"),
            Self::RowScalar(val) => write!(f, "CSV rows must be an array or object, found: {val}"),
            Self::FieldType(val) => {
                write!(
                    f,
                    "CSV fields must be bool, number, or string, found: {val}"
                )
            }
            Error::RowNotObject(keys, val) => {
                write!(f, "CSV field expected object with keys [")?;
                let mut delimiter = "";
                for key in keys {
                    write!(f, "{delimiter}{key}")?;
                    delimiter = ", ";
                }
                write!(f, "], found {val}")
            }
            Error::RowWrongLength(len, val) => {
                write!(f, "CSV field expected array of length {len}, found {val}")
            }
        }
    }
}

fn fail<T>(e: Error) -> Result<T, std::io::Error> {
    Err(invalid_data(e.to_string()))
}

fn write_field<'a>(w: &mut dyn std::io::Write, field: &Val) -> Result<(), std::io::Error> {
    match field {
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
        _ => fail(Error::FieldType(field.clone())),
    }
}

fn write_row<'a>(
    w: &mut dyn std::io::Write,
    fields: impl Iterator<Item = &'a Val>,
    newline: bool,
) -> Result<(), std::io::Error> {
    if newline {
        writeln!(w);
    }
    let mut delim = "";
    for field in fields {
        write!(w, "{delim}")?;
        write_field(w, field)?;
        delim = ",";
    }
    Ok(())
}

/// Write a value as CBOR.
pub fn write(w: &mut dyn std::io::Write, v: &Val) -> Result<(), std::io::Error> {
    match v {
        Val::Arr(vals) => match vals.get(0) {
            Some(Val::Obj(map)) => {
                let ks: Vec<Val> = map.keys().cloned().collect();
                write_row(w, ks.iter(), false)?;
                for val in vals.iter() {
                    match val {
                        Val::Obj(map)
                            if map.len() == ks.len() && ks.iter().all(|k| map.contains_key(k)) =>
                        {
                            write_row(w, ks.iter().map(|k| &map[k]), true)?
                        }
                        _ => return fail(Error::RowNotObject(ks, val.clone())),
                    }
                }
                Ok(())
            }
            Some(Val::Arr(vs)) => {
                let length = vs.len();
                let mut newline = false;
                for val in vals.iter() {
                    match val {
                        Val::Arr(fields) if fields.len() == length => {
                            write_row(w, fields.iter(), newline)?;
                            newline = true;
                        }
                        _ => return fail(Error::RowWrongLength(length, val.clone())),
                    }
                }
                Ok(())
            }
            Some(v) => fail(Error::RowScalar(v.clone())),
            _ => Ok(()),
        },
        _ => fail(Error::NotArray(v.clone()).into()),
    }
}
