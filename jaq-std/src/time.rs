use crate::{Error, ValR, ValT};
use alloc::string::{String, ToString};
use alloc::vec;
use chrono::{DateTime, Local, Datelike, Timelike};

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

/// Format a number using strftime
pub fn strftime<V: ValT>(v: &V, fmt: &str) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot parse {v} as epoch timestamp"));
    let val = if let Some(i) = v.as_isize() {
        (i * 1000000) as i64
    } else {
        (v.as_f64()? * 1000000.0) as i64
    };

    let dt = DateTime::from_timestamp_micros(val).ok_or_else(fail)?;
    Ok(dt.format(fmt).to_string().into())
}

/// Format a number using strftime in the local timezone
pub fn strflocaltime<V: ValT>(v: &V, fmt: &str) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot parse {v} as epoch timestamp"));
    let val = if let Some(i) = v.as_isize() {
        (i * 1000000) as i64
    } else {
        (v.as_f64()? * 1000000.0) as i64
    };

    let dt: DateTime<Local> = DateTime::from_timestamp_micros(val).ok_or_else(fail)?.into();
    Ok(dt.format(fmt).to_string().into())
}


/// Convert an epoch timestamp to a "broken down time" array
pub fn gmtime<V: ValT>(v: &V) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot parse {v} as epoch timestamp"));
    let val = if let Some(i) = v.as_isize() {
        (i * 1000000) as i64
    } else {
        (v.as_f64()? * 1000000.0) as i64
    };
    let dt = DateTime::from_timestamp_micros(val).ok_or_else(fail)?;

    let rv:Vec<isize> = vec!(
    	dt.year() as isize,
    	dt.month0() as isize,
    	dt.day() as isize,
    	dt.hour() as isize,
    	dt.minute() as isize,
    	dt.second() as isize,
    	dt.weekday().num_days_from_sunday() as isize,
    	dt.ordinal0() as isize,
    );
    // convert from isize to ValR<V>, and also somehow convert the Vec<isize>
    // into a ValR<V> ?
    let rv:ValR<V> = rv.iter().map( |&v|->ValR<V> { Ok(v.into()) } ).collect();
    rv
}
