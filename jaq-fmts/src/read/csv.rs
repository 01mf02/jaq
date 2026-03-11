//! CSV and TSV support.
use core::iter::{Fuse, FusedIterator, Iterator};
use jaq_json::{Map, Num, Val};
use num_bigint::BigInt;
use std::io::{Error, ErrorKind};
use std::str::FromStr;

fn convert_field(b: &[u8]) -> Val {
    if let Ok(s) = str::from_utf8(b) {
        if let Ok(i) = s.parse() {
            Val::Num(Num::Int(i))
        } else if let Ok(bi) = BigInt::from_str(s) {
            Val::Num(Num::BigInt(bi.into()))
        } else if let Ok(f) = s.parse() {
            Val::Num(Num::Float(f))
        } else if s == "true" {
            Val::Bool(true)
        } else if s == "false" {
            Val::Bool(false)
        } else {
            Val::utf8_str(Box::from(b))
        }
    } else {
        Val::byte_str(Box::from(b))
    }
}

/// The boolean indicates whether this field was the last on the given line.
type FieldItem = Result<(bool, Val), Error>;

struct CSVFieldReader<T> {
    buffer: Vec<u8>,
    input: Fuse<T>,
}
impl<T: Iterator<Item = Result<u8, Error>>> FusedIterator for CSVFieldReader<T> {}
impl<T: Iterator<Item = Result<u8, Error>>> CSVFieldReader<T> {
    fn new(it: T) -> Self {
        Self {
            buffer: vec![],
            input: it.fuse(),
        }
    }
}
impl<T: Iterator<Item = Result<u8, Error>>> Iterator for CSVFieldReader<T> {
    type Item = FieldItem;
    fn next(&mut self) -> Option<Self::Item> {
        self.buffer.clear();
        let first = match self.input.next() {
            Some(Ok(b)) => b,
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        };
        if first == b'"' {
            while let Some(byte) = self.input.next() {
                match byte {
                    Ok(b'"') => match self.input.next() {
                        Some(Ok(b',')) => return Some(Ok((false, convert_field(&self.buffer)))),
                        None | Some(Ok(b'\n')) => {
                            return Some(Ok((true, convert_field(&self.buffer))))
                        }
                        Some(Ok(b'"')) => self.buffer.push(b'"'),
                        Some(Ok(b)) => {
                            return Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                format!("Unexpected character '{}' after '\"'", char::from(b)),
                            )))
                        }
                        Some(Err(e)) => return Some(Err(e)),
                    },
                    Ok(byte) => self.buffer.push(byte),
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Err(ErrorKind::UnexpectedEof.into()))
        } else {
            self.buffer.push(first);
            loop {
                match self.input.next() {
                    Some(Ok(b',')) => return Some(Ok((false, convert_field(&self.buffer)))),
                    Some(Ok(b'\n')) => return Some(Ok((true, convert_field(&self.buffer)))),
                    Some(Ok(byte)) => self.buffer.push(byte),
                    Some(Err(e)) => return Some(Err(e)),
                    None => {
                        return if self.buffer.is_empty() {
                            None
                        } else {
                            Some(Ok((true, convert_field(&self.buffer))))
                        }
                    }
                }
            }
        }
    }
}
struct TSVFieldReader<T> {
    buffer: Vec<u8>,
    input: Fuse<T>,
}
impl<T: Iterator<Item = Result<u8, Error>>> FusedIterator for TSVFieldReader<T> {}
impl<T: Iterator<Item = Result<u8, Error>>> TSVFieldReader<T> {
    fn new(it: T) -> Self {
        Self {
            buffer: vec![],
            input: it.fuse(),
        }
    }
}
impl<T: Iterator<Item = Result<u8, Error>>> Iterator for TSVFieldReader<T> {
    type Item = FieldItem;
    fn next(&mut self) -> Option<Self::Item> {
        self.buffer.clear();
        loop {
            match self.input.next() {
                Some(Ok(b'\\')) => match self.input.next() {
                    None => return Some(Ok((true, convert_field(&self.buffer)))),
                    Some(Ok(b'n')) => self.buffer.push(b'\n'),
                    Some(Ok(b't')) => self.buffer.push(b'\t'),
                    Some(Ok(b'r')) => self.buffer.push(b'\r'),
                    Some(Ok(b'0')) => self.buffer.push(b'\0'),
                    Some(Ok(b'\\')) => self.buffer.push(b'\\'),
                    Some(Ok(byte)) => {
                        self.buffer.push(b'\\');
                        self.buffer.push(byte);
                    }
                    Some(Err(e)) => return Some(Err(e)),
                },
                Some(Ok(b'\t')) => return Some(Ok((false, convert_field(&self.buffer)))),
                Some(Ok(b'\n')) => return Some(Ok((true, convert_field(&self.buffer)))),
                None => {
                    return if self.buffer.is_empty() {
                        None
                    } else {
                        Some(Ok((true, convert_field(&self.buffer))))
                    }
                }
                Some(Ok(byte)) => self.buffer.push(byte),
                Some(Err(e)) => return Some(Err(e)),
            }
        }
    }
}

struct LinesReader<T> {
    buffer: Vec<Val>,
    input: Fuse<T>,
}
impl<T: Iterator<Item = FieldItem>> FusedIterator for LinesReader<T> {}
impl<T: Iterator<Item = FieldItem>> LinesReader<T> {
    fn new(it: T) -> Self {
        Self {
            buffer: vec![],
            input: it.fuse(),
        }
    }
    fn read(&mut self) -> Option<Result<&[Val], Error>> {
        self.buffer.clear();
        loop {
            match self.input.next() {
                Some(Ok((true, val))) => {
                    self.buffer.push(val);
                    return Some(Ok(&self.buffer));
                }
                Some(Ok((false, val))) => self.buffer.push(val),
                Some(Err(e)) => return Some(Err(e)),
                None => {
                    if self.buffer.len() == 0 {
                        return None;
                    } else {
                        return Some(Ok(&self.buffer));
                    }
                }
            }
        }
    }
}
impl<T: Iterator<Item = FieldItem>> Iterator for LinesReader<T> {
    type Item = Result<Val, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        self.read().map(|x| x.map(|y| Val::Arr(y.to_vec().into())))
    }
}

struct RecordReader<T> {
    header: Vec<Val>,
    input: LinesReader<T>,
}
impl<T: Iterator<Item = FieldItem>> FusedIterator for RecordReader<T> {}
impl<T: Iterator<Item = FieldItem>> RecordReader<T> {
    fn new(it: T) -> Self {
        Self {
            header: vec![],
            input: LinesReader::new(it),
        }
    }
}
impl<'a, T: Iterator<Item = FieldItem>> Iterator for RecordReader<T> {
    type Item = Result<Val, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.header.is_empty() {
            match self.input.read() {
                Some(Ok(line)) => self.header = line.into(),
                Some(Err(e)) => return Some(Err(e)),
                None => return None,
            }
        }
        match self.input.read() {
            Some(Ok(line)) => {
                if line.len() == self.header.len() {
                    Some(Ok(Val::Obj(
                        self.header
                            .iter()
                            .cloned()
                            .zip(line.iter().cloned())
                            .collect::<Map>()
                            .into(),
                    )))
                } else {
                    Some(Err(Error::new(
                        ErrorKind::InvalidData,
                        format!("CSV or TSV had incorrect field count"),
                    )))
                }
            }
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        }
    }
}

/// Read a stream of CSV data to fields.
pub fn csv<'a, T: 'a + Iterator<Item = Result<u8, Error>>>(
    bytes: T,
) -> impl 'a + Iterator<Item = FieldItem> {
    CSVFieldReader::new(bytes)
}
/// Read a stream of TSV data to fields.
pub fn tsv<'a, T: 'a + Iterator<Item = Result<u8, Error>>>(
    bytes: T,
) -> impl 'a + Iterator<Item = FieldItem> {
    TSVFieldReader::new(bytes)
}
/// Convert a list of fields into line objects.
pub fn lines<'a, T: 'a + Iterator<Item = FieldItem>>(
    fields: T,
) -> impl 'a + Iterator<Item = Result<Val, Error>> {
    LinesReader::new(fields)
}

/// Convert a list of fields into line objects.
pub fn records<'a, T: 'a + Iterator<Item = FieldItem>>(fields: T) -> Result<Val, Error> {
    Ok(Val::Arr(
        RecordReader::new(fields)
            .collect::<Result<Vec<_>, _>>()?
            .into(),
    ))
}
