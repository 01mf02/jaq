//! logfmt support.
//!
//! For more details about the format see:
//! https://www.brandur.org/logfmt
//! https://grafana.com/docs/loki/latest/query/log_queries/#logfmt
//! https://docs.appsignal.com/logging/formatting/logfmt

use jaq_json::{read::parse_single_num, Val};

#[derive(Default)]
struct KeyVal {
    key: Vec<u8>,
    val: Option<Vec<u8>>,
}

type Row = Vec<KeyVal>;

struct Reader<'a, E, I: Iterator<Item = Result<u8, E>>> {
    iter: &'a mut I,
    open_quote: bool,
    done: bool,
}

impl<'a, E, I: Iterator<Item = Result<u8, E>>> Reader<'a, E, I> {
    fn new(iter: &'a mut I) -> Self {
        Reader {
            iter,
            open_quote: false,
            done: false,
        }
    }

    fn field(&mut self) -> Result<Option<KeyVal>, E> {
        let mut kv = KeyVal::default();

        if self.open_quote && !quoted(&mut self.iter, &mut kv.key)? {
            return Ok(None);
        }

        loop {
            let Some(c) = self.iter.next().transpose()? else {
                if kv.val.is_none() {
                    return Ok(None);
                } else {
                    break;
                }
            };

            match c {
                b'"' if kv.val.as_ref().is_some_and(|v| !v.is_empty()) => {
                    // quote in value
                    self.open_quote = true;
                    break;
                }
                b'"' => {
                    quoted(&mut self.iter, kv.val.as_mut().unwrap_or(&mut kv.key))?;
                }
                b'=' => kv.val = Some(Vec::new()),
                b'\n' => {
                    self.done = true;
                    break;
                }
                c if c.is_ascii_whitespace() => {
                    if kv.val.is_none() {
                        kv.key.clear();
                    } else {
                        break;
                    }
                }
                c => kv.val.as_mut().unwrap_or(&mut kv.key).push(c),
            }
        }
        Ok(Some(kv))
    }

    /// Read single log row.
    fn row(&mut self) -> Option<Result<Row, E>> {
        let mut row = Vec::new();
        while !self.done {
            let pair = match self.field() {
                Ok(Some(pair)) => pair,
                Ok(None) => break,
                Err(msg) => return Some(Err(msg)),
            };
            if self.done && pair.val.is_none() {
                if row.is_empty() {
                    self.done = false;
                    continue;
                }
                break;
            }
            row.push(pair);
        }
        if row.is_empty() {
            return None;
        }
        Some(Ok(row))
    }
}

fn quoted<E>(iter: &mut impl Iterator<Item = Result<u8, E>>, acc: &mut Vec<u8>) -> Result<bool, E> {
    acc.clear();
    let mut escaped = false;
    while let Some(c) = iter.next().transpose()? {
        match c {
            b'\\' if core::mem::take(&mut escaped) => acc.push(b'\\'),
            b'\\' => escaped = true,
            b'\n' => return Ok(false),
            b'"' if !escaped => return Ok(true),
            _ => acc.push(c),
        }
    }
    Ok(false)
}

fn parse_val(val: Vec<u8>) -> Val {
    if val.is_empty() {
        Val::Null
    } else if val == b"true" {
        Val::Bool(true)
    } else if val == b"false" {
        Val::Bool(false)
    } else if let Some(num) = parse_single_num(&val) {
        Val::Num(num)
    } else {
        Val::utf8_str(val)
    }
}

/// Read rows of a logfmt file.
pub fn read_logfmt<E>(
    mut iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    let from_pair = |pair: KeyVal| {
        let key = Val::utf8_str(pair.key);
        // TODO: what to do if val is None here?
        let val = parse_val(pair.val.unwrap());
        (key, val)
    };
    let from_row = move |row: Row| Val::obj(row.into_iter().map(from_pair).collect());
    core::iter::from_fn(move || Reader::new(&mut iter).row().map(|row| row.map(from_row)))
}

#[cfg(test)]
mod tests {
    use super::Reader;
    use std::io;

    fn chars(s: &[u8]) -> impl Iterator<Item = io::Result<u8>> + '_ {
        s.iter().cloned().map(io::Result::Ok)
    }

    #[test]
    fn simple() {
        let input = b"the_key=val";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"the_key");
        assert_eq!(kv.val.unwrap(), b"val");
    }

    #[test]
    fn with_spaces() {
        let input = b"\t\t   key=val \t\t  ";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val.unwrap(), b"val");
    }

    #[test]
    fn with_garbage() {
        let input = b" \"not this\"  WRONG PI=3.1415 also_incorrect  ";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"PI");
        assert_eq!(kv.val.unwrap(), b"3.1415");
    }

    #[test]
    fn quoted_value() {
        let input = b"message=\"hello, world!\"";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"message");
        assert_eq!(kv.val.unwrap(), b"hello, world!");
    }

    #[test]
    fn quoted_key() {
        let input = b"\"first name\"=Tim";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"first name");
        assert_eq!(kv.val.unwrap(), b"Tim");
    }

    #[test]
    fn mixed_quotes() {
        let input = b"WRONG\"key\"=val\"INCORRECT\"";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val.unwrap(), b"val");
    }

    #[test]
    fn missing_spaces() {
        // edge case: quoted fields are mixed with non-quoted and delimiting spaces are missing
        let input = b"\"key1\"=val1\"key2\"=val2";
        let row = Reader::new(&mut chars(input)).row().unwrap().unwrap();
        let mut kvs = row.into_iter();
        let kv = kvs.next().unwrap();
        assert_eq!(kv.key, b"key1");
        assert_eq!(kv.val.unwrap(), b"val1");
        let kv = kvs.next().unwrap();
        assert_eq!(kv.key, b"key2");
        assert_eq!(kv.val.unwrap(), b"val2");
    }

    #[test]
    fn empty_key() {
        // edge case
        let input = b"\"\"=val";
        let kv = Reader::new(&mut chars(input)).field().unwrap().unwrap();
        assert_eq!(kv.key, b"");
        assert_eq!(kv.val.unwrap(), b"val");
    }
}
