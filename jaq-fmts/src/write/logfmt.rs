//! logfmt support.
//!
//! For more details about the format see:
//! https://www.brandur.org/logfmt
//! https://grafana.com/docs/loki/latest/query/log_queries/#logfmt
//! https://docs.appsignal.com/logging/formatting/logfmt

use crate::invalid_data;
use jaq_json::Val;
use std::io;

pub(crate) fn write_logfmt(w: &mut dyn io::Write, val: &Val) -> io::Result<()> {
    match val {
        Val::Obj(map) => {
            let s = map
                .iter()
                .map(|(k, v)| {
                    let k = val_to_string(k);
                    let v = val_to_string(v);
                    format!("{k}={v}")
                })
                .collect::<Vec<String>>()
                .join(" ");
            write!(w, "{s}")
        }
        v => Err(invalid_data(format!("expected table row (map), found {v}"))),
    }
}

fn val_to_string(val: &Val) -> String {
    match val {
        Val::Null | Val::Num(_) | Val::Bool(_) => val.to_string(),
        Val::TStr(v) | Val::BStr(v) => {
            let s = String::from_utf8_lossy(v);
            if v.iter()
                .any(|c| *c == b'"' || *c == b'=' || c.is_ascii_whitespace())
            {
                format!("\"{}\"", s.replace("\"", "\\\""))
            } else {
                s.to_string()
            }
        }
        other => format!("\"{}\"", other.to_string().replace("\"", "\\\"")),
    }
}

#[cfg(test)]
mod tests {
    use super::val_to_string;
    use jaq_json::Val;
    use jaq_std::ValT;

    #[test]
    fn number_to_string() {
        let input = Val::from_utf8_bytes(b"3.14");
        let result = val_to_string(&input);
        assert_eq!(result, "3.14")
    }

    #[test]
    fn null_to_string() {
        let input = Val::from_utf8_bytes(b"null");
        let result = val_to_string(&input);
        assert_eq!(result, "null")
    }

    #[test]
    fn bool_to_string() {
        let input = Val::from_utf8_bytes(b"true");
        let result = val_to_string(&input);
        assert_eq!(result, "true")
    }

    #[test]
    fn unquoted_str_to_string() {
        let input = Val::from_utf8_bytes(b"fooBar_baz");
        let result = val_to_string(&input);
        assert_eq!(result, "fooBar_baz")
    }

    #[test]
    fn quoted_str_to_string() {
        let input = Val::from_utf8_bytes(b"hello, world!");
        let result = val_to_string(&input);
        assert_eq!(result, "\"hello, world!\"")
    }

    #[test]
    fn equal_in_str_to_string() {
        let input = Val::from_utf8_bytes(b"2=2");
        let result = val_to_string(&input);
        assert_eq!(result, "\"2=2\"")
    }

    #[test]
    fn obj_to_string() {
        let input = Val::from_utf8_bytes(b"{a: 1, b: \"hello\", c: [1,2,3]}");
        let result = val_to_string(&input);
        assert_eq!(result, "\"{a: 1, b: \\\"hello\\\", c: [1,2,3]}\"")
    }

    #[test]
    fn quotes_in_str_to_string() {
        let input = Val::from_utf8_bytes(b"message=\"hello\"");
        let result = val_to_string(&input);
        assert_eq!(result, "\"message=\\\"hello\\\"\"")
    }

    #[test]
    fn url_to_string() {
        let input = Val::from_utf8_bytes(b"https://github.com/01mf02/jaq#installation");
        let result = val_to_string(&input);
        assert_eq!(result, "https://github.com/01mf02/jaq#installation")
    }
}
