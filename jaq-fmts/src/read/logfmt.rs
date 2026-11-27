//! logfmt support.
//!
//! For more details about the format see:
//! https://www.brandur.org/logfmt
//! https://grafana.com/docs/loki/latest/query/log_queries/#logfmt
//! https://docs.appsignal.com/logging/formatting/logfmt

use jaq_json::{read::parse_single_num, Map, Val};
use std::rc::Rc;

#[derive(Default)]
struct KeyVal {
    key: Vec<u8>,
    val: Vec<u8>,
    /// is the key/value pair valid
    ok: bool,
}

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

    fn quoted(&mut self) -> Result<Option<Vec<u8>>, E> {
        let mut acc = Vec::new();
        let mut escaped = false;
        while let Some(c) = self.iter.next().transpose()? {
            match c {
                b'\\' if core::mem::take(&mut escaped) => acc.push(b'\\'),
                b'\\' => escaped = true,
                b'\n' => return Ok(None),
                b'"' if !escaped => return Ok(Some(acc)),
                _ => acc.push(c),
            }
        }
        Ok(None)
    }

    fn field(&mut self) -> Result<Option<KeyVal>, E> {
        let mut kv = KeyVal::default();
        let mut acc = Vec::new();
        let mut key = true;

        if self.open_quote {
            if let Some(v) = self.quoted()? {
                acc = v;
            } else {
                return Ok(None);
            }
        }

        loop {
            let Some(c) = self.iter.next().transpose()? else {
                if key {
                    return Ok(None);
                } else {
                    break;
                }
            };

            match c {
                b'"' => {
                    if !key && !acc.is_empty() {
                        // quote in value
                        self.open_quote = true;
                        break;
                    } else if let Some(v) = self.quoted()? {
                        acc = v;
                    } else {
                        continue;
                    }
                }
                b'=' => {
                    std::mem::swap(&mut kv.key, &mut acc);
                    key = false;
                }
                b'\n' => {
                    self.done = true;
                    if key {
                        return Ok(Some(kv));
                    }
                    break;
                }
                c if c.is_ascii_whitespace() => {
                    if key {
                        acc.clear();
                        // instead we could do:
                        //
                        //   std::mem::swap(&mut kv.key, &mut acc);
                        //   return Ok(Some(kv));
                        //
                        // so kv.ok remains false, and handle the garbage values downstream
                    } else {
                        break;
                    }
                }
                c => {
                    acc.push(c);
                }
            }
        }
        kv.val = acc;
        kv.ok = true;
        Ok(Some(kv))
    }

    /// Read single log row.
    fn row(&mut self) -> Option<Result<Val, E>> {
        let mut map = Map::default();
        while !self.done {
            let pair = match self.field() {
                Ok(Some(pair)) => pair,
                Ok(None) => break,
                Err(msg) => return Some(Err(msg)),
            };
            if self.done && !pair.ok {
                if map.is_empty() {
                    self.done = false;
                    continue;
                }
                break;
            }
            let key = Val::utf8_str(pair.key);
            let val = if pair.val.is_empty() {
                Val::Null
            } else if pair.val == b"true" {
                Val::Bool(true)
            } else if pair.val == b"false" {
                Val::Bool(false)
            } else if let Some(num) = parse_single_num(&pair.val) {
                Val::Num(num)
            } else {
                Val::utf8_str(pair.val)
            };
            map.insert(key, val);
        }
        if map.is_empty() {
            return None;
        }
        Some(Ok(Val::Obj(Rc::new(map))))
    }
}

/// Read rows of a logfmt file.
pub fn read_logfmt<E>(
    mut iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    core::iter::from_fn(move || Reader::new(&mut iter).row())
}

#[cfg(test)]
mod tests {
    use super::Reader;
    use std::io;

    #[test]
    fn simple() {
        let mut iter = b"the_key=val".iter().cloned().map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"the_key");
        assert_eq!(kv.val, b"val");
    }

    #[test]
    fn with_spaces() {
        let mut iter = b"\t\t   key=val \t\t  ".iter().cloned().map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val, b"val");
    }

    #[test]
    fn with_garbage() {
        let mut iter = b" \"not this\"  WRONG PI=3.1415 also_incorrect  "
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"PI");
        assert_eq!(kv.val, b"3.1415");
    }

    #[test]
    fn quoted_value() {
        let mut iter = b"message=\"hello, world!\""
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"message");
        assert_eq!(kv.val, b"hello, world!");
    }

    #[test]
    fn quoted_key() {
        let mut iter = b"\"first name\"=Tim".iter().cloned().map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"first name");
        assert_eq!(kv.val, b"Tim");
    }

    #[test]
    fn mixed_quotes() {
        let mut iter = b"WRONG\"key\"=val\"INCORRECT\""
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val, b"val");
    }

    #[test]
    fn missing_spaces() {
        // edge case: quoted fields are mixed with non-quoted and delimiting spaces are missing
        let mut iter = b"\"key1\"=val1\"key2\"=val2"
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let row = Reader::new(&mut iter).row().unwrap().unwrap();
        assert_eq!(row.to_string(), "{\"key1\":\"val1\",\"key2\":\"val2\"}")
    }

    #[test]
    fn empty_key() {
        // edge case
        let mut iter = b"\"\"=val".iter().cloned().map(io::Result::Ok);
        let kv = Reader::new(&mut iter).field().unwrap().unwrap();
        assert_eq!(kv.key, b"");
        assert_eq!(kv.val, b"val");
    }
}
