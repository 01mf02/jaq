//! logfmt support.
//!
//! For more details about the format see:
//! https://www.brandur.org/logfmt
//! https://grafana.com/docs/loki/latest/query/log_queries/#logfmt
//! https://docs.appsignal.com/logging/formatting/logfmt

use jaq_json::{Map, Val, read::parse_single_num};
use std::rc::Rc;

#[derive(Default)]
struct Pair {
    key: Vec<u8>,
    val: Vec<u8>,
    valid: bool,
}

fn field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Option<Pair>, E> {
    let mut key = Vec::new();
    let mut val = Vec::new();
    let mut valid = true;

    // key
    loop {
        match iter.next().transpose()? {
            Some(c) if c.is_ascii_whitespace() => {}
            Some(c) => {
                key.push(c);
                if c == b'"' || c == b'=' {
                    valid = false;
                }
                break;
            }
            None => return Ok(None),
        }
    }
    loop {
        match iter.next().transpose()? {
            Some(b'=') => break,
            Some(c) if c.is_ascii_whitespace() || c == b'"' => {
                valid = false;
                break;
            }
            Some(c) => {
                key.push(c);
            }
            None => {
                valid = false;
                break;
            }
        }
    }
    if !valid {
        return Ok(Some(Pair { key, val, valid }));
    }

    // val
    let quoted = match iter.next().transpose()? {
        Some(c) if c.is_ascii_whitespace() => return Ok(Some(Pair { key, val, valid })),
        None => return Ok(Some(Pair { key, val, valid })),
        Some(b'"') => true,
        Some(c) => {
            val.push(c);
            false
        }
    };

    let mut escape = false;
    loop {
        match iter.next().transpose()? {
            None => break,
            Some(c) if c.is_ascii_whitespace() => {
                if quoted {
                    val.push(c);
                } else {
                    break
                }
            },
            Some(c @ b'\\') => {
                if escape {
                    val.push(c);
                }
                escape = !escape
            }
            Some(c) => {
                if c == b'"' {
                    if quoted {
                        if escape {
                            escape = false;
                        } else {
                            break;
                        }
                    }
                } else {
                    if escape {
                        val.push(b'\\');
                        escape = false;
                    }
                }
                val.push(c);
            }
        }
    }
    Ok(Some(Pair { key, val, valid }))
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
        if !pair.valid {
            continue;
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

/// Read rows of a logfmt file.
pub fn read_logfmt<E>(
    mut iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    core::iter::from_fn(move || row(&mut iter))
}
