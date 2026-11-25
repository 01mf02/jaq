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
    let k = if let Val::TStr(k) = k
        && k.iter().all(|c| c.is_ascii_alphanumeric())
    {
        String::from_utf8_lossy(k)
    } else {
        return Err(invalid_data(format!(
            "expected alphanumeric key, found {k}"
        )));
    };
    let mut v = v.to_string();
    if v.starts_with('"') && v.chars().all(|c| c != '=' && !c.is_whitespace()) {
        v.remove(0);
        v.pop();
    }
    Ok(format!("{k}={v}"))
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
