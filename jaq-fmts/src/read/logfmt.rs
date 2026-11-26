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
    eol: bool,
}

fn field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Option<KeyVal>, E> {
    let mut kv = KeyVal::default();
    let mut acc = Vec::new();
    let mut key = true;
    let mut quoted = false;
    let mut escaped = false;

    loop {
        let Some(c) = iter.next().transpose()? else {
            if key {
                return Ok(None);
            } else {
                break;
            }
        };

        if escaped {
            if c != b'"' {
                acc.push(b'\\');
            }
            acc.push(c);
            escaped = false;
            continue;
        }

        match c {
            b'"' => {
                if quoted {
                    if key {
                        quoted = false;
                        escaped = false;
                    } else {
                        break;
                    }
                } else if !key && !acc.is_empty() {
                    break;
                } else {
                    acc.clear();
                    quoted = true;
                }
            }
            b'\\' if quoted => {
                escaped = true;
            }
            b'=' if !quoted => {
                std::mem::swap(&mut kv.key, &mut acc);
                key = false;
            }
            b'\n' if !quoted => {
                kv.eol = true;
                if key {
                    return Ok(Some(kv));
                }
                break;
            }
            _ if c.is_ascii_whitespace() => {
                if quoted {
                    acc.push(c);
                } else if key {
                    acc.clear();
                } else {
                    break;
                }
            }
            _ => acc.push(c),
        }
    }
    std::mem::swap(&mut kv.val, &mut acc);
    Ok(Some(kv))
}

/// Read single table row.
fn row<E, I: Iterator<Item = Result<u8, E>>>(iter: &mut I) -> Option<Result<Val, E>> {
    let mut map = Map::default();
    loop {
        let pair = match field(iter) {
            Ok(Some(pair)) => pair,
            Ok(None) => break,
            Err(msg) => return Some(Err(msg)),
        };
        if pair.eol && pair.key.is_empty() {
            if map.is_empty() {
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
        if pair.eol {
            break;
        }
    }
    if map.is_empty() {
        return None;
    }
    Some(Ok(Val::Obj(Rc::new(map))))
}

/// Read rows of a logfmt file.
pub fn read_logfmt<E>(
    mut iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    core::iter::from_fn(move || row(&mut iter))
}

#[cfg(test)]
mod tests {
    use super::field;
    use std::io;

    #[test]
    fn simple() {
        let mut iter = b"the_key=val".iter().cloned().map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"the_key");
        assert_eq!(kv.val, b"val");
    }

    #[test]
    fn with_spaces() {
        let mut iter = b"\t\t   key=val \t\t  ".iter().cloned().map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val, b"val");
    }

    #[test]
    fn with_garbage() {
        let mut iter = b" \"not this\"  WRONG PI=3.1415 also_incorrect  "
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"PI");
        assert_eq!(kv.val, b"3.1415");
    }

    #[test]
    fn quoted_value() {
        let mut iter = b"message=\"hello, world!\""
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"message");
        assert_eq!(kv.val, b"hello, world!");
    }

    #[test]
    fn quoted_key() {
        let mut iter = b"\"first name\"=Tim".iter().cloned().map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"first name");
        assert_eq!(kv.val, b"Tim");
    }

    #[test]
    fn mixed_quotes() {
        let mut iter = b"WRONG\"key\"=val\"INCORRECT\""
            .iter()
            .cloned()
            .map(io::Result::Ok);
        let kv = field(&mut iter).unwrap().unwrap();
        assert_eq!(kv.key, b"key");
        assert_eq!(kv.val, b"val");
    }
}
