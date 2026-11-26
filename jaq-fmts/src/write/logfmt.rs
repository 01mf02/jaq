//! logfmt support.
//!
//! For more details about the format see:
//! https://www.brandur.org/logfmt
//! https://grafana.com/docs/loki/latest/query/log_queries/#logfmt
//! https://docs.appsignal.com/logging/formatting/logfmt

use crate::invalid_data;
use jaq_json::Val;
use std::io;

fn pair_to_string(k: &Val, v: &Val) -> io::Result<String> {
    let k = val_to_string(k);
    let v = val_to_string(v);
    Ok(format!("{k}={v}"))
}

fn val_to_string(val: &Val) -> String {
    match val {
        Val::Null | Val::Num(_) | Val::Bool(_) => val.to_string(),
        Val::TStr(v) | Val::BStr(v) => {
            let s = String::from_utf8_lossy(v);
            if v.iter().any(|c| *c == b'"' || *c == b'=') {
                format!("\"{}\"", s.replace("\"", "\\\""))
            } else {
                s.to_string()
            }
        }
        other => format!("\"{}\"", other.to_string().replace("\"", "\\\"")),
    }
}

pub(crate) fn write_logfmt(w: &mut dyn io::Write, val: &Val) -> io::Result<()> {
    match val {
        Val::Obj(map) => {
            let s = map
                .iter()
                .map(|(k, v)| pair_to_string(k, v))
                .collect::<io::Result<Vec<String>>>()?
                .join(" ");
            write!(w, "{s}")
        }
        v => Err(invalid_data(format!("expected table row (map), found {v}"))),
    }
}
