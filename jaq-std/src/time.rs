use crate::{Error, ValR, ValT, ValTx};
use alloc::string::{String, ToString};
use jiff::{civil::DateTime, fmt::strtime, tz, Timestamp};

/// Convert a UNIX epoch timestamp with optional fractions.
fn epoch_to_timestamp<V: ValT>(v: &V) -> Result<Timestamp, Error<V>> {
    let val = match v.as_isize() {
        Some(i) => i as i64 * 1000000,
        None => (v.try_as_f64()? * 1000000.0) as i64,
    };
    Timestamp::from_microsecond(val).map_err(Error::str)
}

/// Convert a date-time pair to a UNIX epoch timestamp.
fn timestamp_to_epoch<V: ValT>(ts: Timestamp, frac: bool) -> ValR<V> {
    if frac {
        Ok((ts.as_microsecond() as f64 / 1e6).into())
    } else {
        let seconds = ts.as_second();
        isize::try_from(seconds)
            .map(V::from)
            .or_else(|_| V::from_num(&seconds.to_string()))
    }
}

fn array_to_datetime<V: ValT>(v: &[V]) -> Option<DateTime> {
    let [year, month, day, hour, min, sec]: &[V; 6] = v.get(..6)?.try_into().ok()?;
    let sec = sec.as_f64()?;
    let i8 = |v: &V| -> Option<i8> { v.as_isize()?.try_into().ok() };
    DateTime::new(
        year.as_isize()?.try_into().ok()?,
        i8(month)? + 1,
        i8(day)?,
        i8(hour)?,
        i8(min)?,
        // the `as i8` cast saturates, returning a number in the range [-128, 128]
        sec.floor() as i8,
        (sec.fract() * 1e9) as i32,
    )
    .ok()
}

/// Convert a `DateTime` to a "broken down time" array
fn datetime_to_array<V: ValT>(dt: DateTime) -> [V; 8] {
    [
        V::from(dt.year() as isize),
        V::from(dt.month() as isize - 1),
        V::from(dt.day() as isize),
        V::from(dt.hour() as isize),
        V::from(dt.minute() as isize),
        if dt.subsec_nanosecond() > 0 {
            V::from(dt.second() as f64 + dt.subsec_nanosecond() as f64 / 1e9)
        } else {
            V::from(dt.second() as isize)
        },
        V::from(dt.weekday().to_sunday_zero_offset() as isize),
        V::from(dt.day_of_year() as isize - 1),
    ]
}

/// Parse an ISO 8601 timestamp string to a number holding the equivalent UNIX timestamp
/// (seconds elapsed since 1970/01/01).
///
/// Actually, this parses RFC 3339; see
/// <https://ijmacd.github.io/rfc3339-iso8601/> for differences.
/// jq also only parses a very restricted subset of ISO 8601.
pub fn from_iso8601<V: ValT>(s: &str) -> ValR<V> {
    timestamp_to_epoch(s.parse().map_err(Error::str)?, s.contains('.'))
}

/// Format a number as an ISO 8601 timestamp string.
pub fn to_iso8601<V: ValT>(v: &V) -> Result<String, Error<V>> {
    let ts = if let Some(i) = v.as_isize() {
        Timestamp::from_second(i as i64)
    } else {
        Timestamp::from_microsecond((v.try_as_f64()? * 1e6) as i64)
    };
    Ok(ts.map_err(Error::str)?.to_string())
}

/// Format a date (either number or array) in a given timezone.
pub fn strftime<V: ValT>(v: &V, fmt: &str, tz: tz::TimeZone) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot convert {v} to time"));
    let bdt: strtime::BrokenDownTime = match v.clone().into_vec() {
        Ok(v) => array_to_datetime(&v).ok_or_else(fail)?.into(),
        Err(_) => (&epoch_to_timestamp(v)?.to_zoned(tz)).into(),
    };
    strtime::format(fmt, bdt).map(V::from).map_err(Error::str)
}

/// Convert an epoch timestamp to a "broken down time" array.
pub fn gmtime<V: ValT>(v: &V, tz: tz::TimeZone) -> ValR<V> {
    let dt = epoch_to_timestamp(v)?.to_zoned(tz).into();
    datetime_to_array(dt).into_iter().map(Ok).collect()
}

/// Parse a string into a "broken down time" array.
pub fn strptime<V: ValT>(s: &str, fmt: &str) -> ValR<V> {
    let mut bdt = strtime::BrokenDownTime::parse(fmt, s).map_err(Error::str)?;
    if (bdt.offset(), bdt.iana_time_zone()) == (None, None) {
        bdt.set_offset(Some(tz::Offset::UTC));
    }
    let dt = bdt.to_zoned().map_err(Error::str)?.into();
    datetime_to_array(dt).into_iter().map(Ok).collect()
}

/// Parse an array into a UNIX epoch timestamp.
pub fn mktime<V: ValT>(v: &V) -> ValR<V> {
    let fail = || Error::str(format_args!("cannot convert {v} to time"));
    let dt = array_to_datetime(&v.clone().into_vec()?).ok_or_else(fail)?;
    let ts = dt.in_tz("UTC").map_err(Error::str)?.timestamp();
    timestamp_to_epoch(ts, ts.subsec_nanosecond() > 0)
}
