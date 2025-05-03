use crate::{Error, ValR, ValT};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use chrono::{DateTime, Local, Datelike, Timelike, Utc};

fn epoch_to_datetime<V: ValT>(v: &V) -> Result<DateTime<Utc>, Error<V>> {
    let fail = || Error::str(format_args!("cannot parse {v} as epoch timestamp"));
    let val = if let Some(i) = v.as_isize() {
        (i * 1000000) as i64
    } else {
        (v.as_f64()? * 1000000.0) as i64
    };

    DateTime::from_timestamp_micros(val).ok_or_else(fail)
}

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

/// Format a number using strftime, possibly using the local timezone
pub fn strftime<V: ValT>(v: &V, fmt: &str, local: bool) -> ValR<V> {
    let dt = epoch_to_datetime(v)?;

    let dt = if local {
        dt.with_timezone(&Local).fixed_offset()
    } else {
        dt.with_timezone(&Utc).fixed_offset()
    };

    Ok(dt.format(fmt).to_string().into())
}


/// Convert an epoch timestamp to a "broken down time" array
pub fn gmtime<V: ValT>(v: &V) -> ValR<V> {
    let dt = epoch_to_datetime(v)?;

    let mut rv:Vec<ValR<V>> = Vec::new();
    rv.push(Ok(V::from(dt.year() as isize)));
    rv.push(Ok(V::from(dt.month0() as isize)));
    rv.push(Ok(V::from(dt.day() as isize)));
    rv.push(Ok(V::from(dt.hour() as isize)));
    rv.push(Ok(V::from(dt.minute() as isize)));
    if dt.nanosecond() > 0
    {
        rv.push(Ok(V::from((dt.second() as f64 * 1e6+dt.timestamp_subsec_micros() as f64)/1e6)));
    } else
    {
        rv.push(Ok(V::from(dt.second() as isize)));
    }
    rv.push(Ok(V::from(dt.weekday().num_days_from_sunday() as isize)));
    rv.push(Ok(V::from(dt.ordinal0() as isize)));
    // somehow this converts from Vec<ValR<V>> to ValR<V> ?
    let rv:ValR<V> = rv.into_iter().collect();

    // std::println!("{}", std::any::type_name_of_val(&rv));
    rv
}
