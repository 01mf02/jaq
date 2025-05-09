use crate::{Error, ValR, ValT, ValTx};
use alloc::string::{String, ToString};
use chrono::{DateTime, Datelike, FixedOffset, NaiveDateTime, TimeZone, Timelike, Utc};

/// Convert a UNIX epoch timestamp with optional fractions.
fn epoch_to_datetime<V: ValT>(v: &V) -> Result<DateTime<Utc>, Error<V>> {
    let fail = || Error::str(format_args!("cannot parse {v} as epoch timestamp"));
    let val = match v.as_isize() {
        Some(i) => i as i64 * 1000000,
        None => (v.as_f64()? * 1000000.0) as i64,
    };
    DateTime::from_timestamp_micros(val).ok_or_else(fail)
}

/// Convert a date-time pair to a UNIX epoch timestamp.
fn datetime_to_epoch<Tz: TimeZone, V: ValT>(dt: DateTime<Tz>, frac: bool) -> ValR<V> {
    if frac {
        Ok((dt.timestamp_micros() as f64 / 1e6).into())
    } else {
        let seconds = dt.timestamp();
        isize::try_from(seconds)
            .map(V::from)
            .or_else(|_| V::from_num(&seconds.to_string()))
    }
}

/// Parse a "broken down time" array.
fn array_to_datetime<V: ValT>(v: &[V]) -> Option<DateTime<Utc>> {
    let [year, month, day, hour, min, sec]: &[V; 6] = v.get(..6)?.try_into().ok()?;
    let sec = sec.as_f64().ok()?;
    let u32 = |v: &V| -> Option<u32> { v.as_isize()?.try_into().ok() };
    Utc.with_ymd_and_hms(
        year.as_isize()?.try_into().ok()?,
        u32(month)? + 1,
        u32(day)?,
        u32(hour)?,
        u32(min)?,
        // the `as i8` cast saturates, returning a number in the range [-128, 128]
        (sec.floor() as i8).try_into().ok()?,
    )
    .single()?
    .with_nanosecond((sec.fract() * 1e9) as u32)
}

/// Convert a DateTime<FixedOffset> to a "broken down time" array
fn datetime_to_array<V: ValT>(dt: DateTime<FixedOffset>) -> [V; 8] {
    [
        V::from(dt.year() as isize),
        V::from(dt.month0() as isize),
        V::from(dt.day() as isize),
        V::from(dt.hour() as isize),
        V::from(dt.minute() as isize),
        if dt.nanosecond() > 0 {
            V::from(dt.second() as f64 + dt.timestamp_subsec_micros() as f64 / 1e6)
        } else {
            V::from(dt.second() as isize)
        },
        V::from(dt.weekday().num_days_from_sunday() as isize),
        V::from(dt.ordinal0() as isize),
    ]
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
    datetime_to_epoch(dt, s.contains('.'))
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

/// Format a date (either number or array) in a given timezone.
pub fn strftime<V: ValT>(v: &V, fmt: &str, tz: impl TimeZone) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot convert {v} to time"));
    let dt = match v.clone().into_vec() {
        Ok(v) => array_to_datetime(&v).ok_or_else(fail),
        Err(_) => epoch_to_datetime(v),
    }?;
    let dt = dt.with_timezone(&tz).fixed_offset();
    Ok(dt.format(fmt).to_string().into())
}

/// Convert an epoch timestamp to a "broken down time" array.
pub fn gmtime<V: ValT>(v: &V, tz: impl TimeZone) -> ValR<V> {
    let dt = epoch_to_datetime(v)?;
    let dt = dt.with_timezone(&tz).fixed_offset();
    datetime_to_array(dt).into_iter().map(Ok).collect()
}

/// Parse a string into a "broken down time" array.
pub fn strptime<V: ValT>(s: &str, fmt: &str) -> ValR<V> {
    let dt = NaiveDateTime::parse_from_str(s, fmt)
        .map_err(|e| Error::str(format_args!("cannot parse {s} using {fmt}: {e}")))?;
    let dt = dt.and_utc().fixed_offset();
    datetime_to_array(dt).into_iter().map(Ok).collect()
}

/// Parse an array into a UNIX epoch timestamp.
pub fn mktime<V: ValT>(v: &V) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot convert {v} to time"));
    let dt = array_to_datetime(&v.clone().into_vec()?).ok_or_else(fail)?;
    datetime_to_epoch(dt, dt.timestamp_subsec_micros() > 0)
}
