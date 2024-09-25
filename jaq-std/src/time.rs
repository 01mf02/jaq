use crate::{Error, ValR, ValT};
use alloc::string::{String, ToString};
use chrono::DateTime;

/// Parse an ISO 8601 timestamp string to a number holding the equivalent UNIX timestamp
/// (seconds elapsed since 1970/01/01).
///
/// Actually, this parses RFC 3339; see
/// <https://ijmacd.github.io/rfc3339-iso8601/> for differences.
/// jq also only parses a very restricted subset of ISO 8601.
pub fn from_iso8601<V: ValT>(s: &str) -> ValR<V> {
    let dt = DateTime::parse_from_rfc3339(s)
        .map_err(|e| Error::str(format_args!("cannot parse {s} as ISO-8601 timestamp: {e}")))?;
    if s.contains('.') {
        Ok((dt.timestamp_micros() as f64 / 1e6).into())
    } else {
        let seconds = dt.timestamp();
        isize::try_from(seconds)
            .map(V::from)
            .or_else(|_| V::from_num(&seconds.to_string()))
    }
}

/// Format a number as an ISO 8601 timestamp string.
pub fn to_iso8601<V: ValT>(v: &V) -> Result<String, Error<V>> {
    let fail = || Error::str(format_args!("cannot format {v} as ISO-8601 timestamp"));
    if let Some(i) = v.as_isize() {
        let dt = DateTime::from_timestamp(i as i64, 0).ok_or_else(fail)?;
        Ok(dt.format("%Y-%m-%dT%H:%M:%SZ").to_string())
    } else {
        let f = v.as_f64()?;
        let dt = DateTime::from_timestamp_micros((f * 1e6) as i64).ok_or_else(fail)?;
        Ok(dt.format("%Y-%m-%dT%H:%M:%S%.6fZ").to_string())
    }
}
