//! Core filters.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "math")]
mod math;
#[cfg(feature = "regex")]
mod regex;
#[cfg(feature = "time")]
mod time;

use alloc::string::{String, ToString};
use alloc::{borrow::ToOwned, boxed::Box, rc::Rc, vec, vec::Vec};
use jaq_interpret::results::{box_once, run_if_ok, then};
use jaq_interpret::{Ctx, FilterT, Native, RunPtr, UpdatePtr};
use jaq_interpret::{Error, Val, ValR, ValRs};

/// Return the minimal set of named filters available in jaq
/// which are implemented as native filters, such as `length`, `keys`, ...,
/// but not `now`, `debug`, `fromdateiso8601`, ...
///
/// Does not return filters from the standard library, such as `map`.
pub fn minimal() -> impl Iterator<Item = (String, usize, Native)> {
    run(CORE_RUN).chain(upd(CORE_UPDATE))
}

/// Return those named filters available by default in jaq
/// which are implemented as native filters, such as `length`, `keys`, ...,
/// but also `now`, `debug`, `fromdateiso8601`, ...
///
/// Does not return filters from the standard library, such as `map`.
#[cfg(all(
    feature = "std",
    feature = "format",
    feature = "log",
    feature = "math",
    feature = "regex",
    feature = "time",
))]
pub fn core() -> impl Iterator<Item = (String, usize, Native)> {
    minimal()
        .chain(run(STD))
        .chain(run(FORMAT))
        .chain(upd(LOG))
        .chain(run(MATH))
        .chain(run(REGEX))
        .chain(run(TIME))
}

fn run<'a>(fs: &'a [(&str, usize, RunPtr)]) -> impl Iterator<Item = (String, usize, Native)> + 'a {
    fs.iter()
        .map(|(name, arity, f)| (name.to_string(), *arity, Native::new(*f)))
}

fn upd<'a>(
    fs: &'a [(&str, usize, RunPtr, UpdatePtr)],
) -> impl Iterator<Item = (String, usize, Native)> + 'a {
    fs.iter().map(|(name, arity, run, update)| {
        (name.to_string(), *arity, Native::with_update(*run, *update))
    })
}

// This might be included in the Rust standard library:
// <https://github.com/rust-lang/rust/issues/93610>
fn rc_unwrap_or_clone<T: Clone>(a: Rc<T>) -> T {
    Rc::try_unwrap(a).unwrap_or_else(|a| (*a).clone())
}

/// Sort array by the given function.
fn sort_by<'a>(xs: &mut [Val], f: impl Fn(Val) -> ValRs<'a>) -> Result<(), Error> {
    // Some(e) iff an error has previously occurred
    let mut err = None;
    xs.sort_by_cached_key(|x| run_if_ok(x.clone(), &mut err, &f));
    err.map_or(Ok(()), Err)
}

/// Group an array by the given function.
fn group_by<'a>(xs: Vec<Val>, f: impl Fn(Val) -> ValRs<'a>) -> ValR {
    let mut err = None;
    let mut yx = xs
        .into_iter()
        .map(|x| (run_if_ok(x.clone(), &mut err, &f), x))
        .collect::<Vec<(Vec<Val>, Val)>>();
    if let Some(err) = err {
        return Err(err);
    }

    yx.sort_by(|(y1, _), (y2, _)| y1.cmp(y2));

    use itertools::Itertools;
    let grouped = yx
        .into_iter()
        .group_by(|(y, _)| y.clone())
        .into_iter()
        .map(|(_y, yxs)| Val::arr(yxs.map(|(_y, x)| x).collect()))
        .collect();
    Ok(Val::arr(grouped))
}

/// Get the minimum or maximum element from an array according to the given function.
fn cmp_by<'a, R>(xs: Vec<Val>, f: impl Fn(Val) -> ValRs<'a>, replace: R) -> ValR
where
    R: Fn(&Vec<Val>, &Vec<Val>) -> bool,
{
    let iter = xs.into_iter();
    let mut iter = iter.map(|x: Val| (x.clone(), f(x).collect::<Result<_, _>>()));
    let (mut mx, mut my) = if let Some((x, y)) = iter.next() {
        (x, y?)
    } else {
        return Ok(Val::Null);
    };
    for (x, y) in iter {
        let y = y?;
        if replace(&my, &y) {
            (mx, my) = (x, y);
        }
    }
    Ok(mx)
}

/// Split a string by a given separator string.
fn split(s: &str, sep: &str) -> Vec<Val> {
    if sep.is_empty() {
        // Rust's `split` function with an empty separator ("")
        // yields an empty string as first and last result
        // to prevent this, we are using `chars` instead
        s.chars().map(|s| Val::str(s.to_string())).collect()
    } else {
        s.split(sep).map(|s| Val::str(s.to_string())).collect()
    }
}

fn strip<F>(s: &Rc<String>, other: &str, f: F) -> Rc<String>
where
    F: for<'a> Fn(&'a str, &str) -> Option<&'a str>,
{
    f(s, other).map_or_else(|| s.clone(), |stripped| Rc::new(stripped.into()))
}

fn fmt_row(v: &Val, f: impl Fn(&str) -> String) -> Result<String, Error> {
    let fail = || alloc::format!("invalid value in a table row: {v}");
    Ok(match v {
        Val::Null => "".to_owned(),
        Val::Str(s) => f(s),
        Val::Arr(_) | Val::Obj(_) => return Err(Error::from_any(fail())),
        v => v.to_string(),
    })
}

fn to_csv(vs: &[Val]) -> Result<String, Error> {
    let fr = |v| fmt_row(v, |s| alloc::format!("\"{}\"", s.replace('"', "\"\"")));
    Ok(vs.iter().map(fr).collect::<Result<Vec<_>, _>>()?.join(","))
}

const CORE_RUN: &[(&str, usize, RunPtr)] = &[
    ("inputs", 0, |_, cv| {
        Box::new(cv.0.inputs().map(|r| r.map_err(Error::Parse)))
    }),
    ("length", 0, |_, cv| box_once(cv.1.len())),
    ("keys_unsorted", 0, |_, cv| {
        box_once(cv.1.keys_unsorted().map(Val::arr))
    }),
    ("floor", 0, |_, cv| box_once(cv.1.round(|f| f.floor()))),
    ("round", 0, |_, cv| box_once(cv.1.round(|f| f.round()))),
    ("ceil", 0, |_, cv| box_once(cv.1.round(|f| f.ceil()))),
    ("fromjson", 0, |_, cv| box_once(cv.1.from_json())),
    ("tojson", 0, |_, cv| {
        box_once(Ok(Val::str(cv.1.to_string())))
    }),
    ("utf8bytelength", 0, |_, cv| {
        then(cv.1.as_str(), |s| box_once(Ok(Val::Int(s.len() as isize))))
    }),
    ("explode", 0, |_, cv| box_once(cv.1.explode().map(Val::arr))),
    ("implode", 0, |_, cv| box_once(cv.1.implode().map(Val::str))),
    ("ascii_downcase", 0, |_, cv| {
        box_once(cv.1.mutate_str(|s| s.make_ascii_lowercase()))
    }),
    ("ascii_upcase", 0, |_, cv| {
        box_once(cv.1.mutate_str(|s| s.make_ascii_uppercase()))
    }),
    ("reverse", 0, |_, cv| {
        box_once(cv.1.mutate_arr(|a| a.reverse()))
    }),
    ("sort", 0, |_, cv| box_once(cv.1.mutate_arr(|a| a.sort()))),
    ("sort_by", 1, |args, cv| {
        box_once(cv.1.try_mutate_arr(|arr| sort_by(arr, |v| args.get(0).run((cv.0.clone(), v)))))
    }),
    ("group_by", 1, |args, cv| {
        then(cv.1.into_arr().map(rc_unwrap_or_clone), |arr| {
            box_once(group_by(arr, |v| args.get(0).run((cv.0.clone(), v))))
        })
    }),
    ("min_by", 1, |args, cv| {
        let f = |v| args.get(0).run((cv.0.clone(), v));
        then(cv.1.into_arr().map(rc_unwrap_or_clone), |arr| {
            box_once(cmp_by(arr, f, |my, y| y < my))
        })
    }),
    ("max_by", 1, |args, cv| {
        let f = |v| args.get(0).run((cv.0.clone(), v));
        then(cv.1.into_arr().map(rc_unwrap_or_clone), |arr| {
            box_once(cmp_by(arr, f, |my, y| y >= my))
        })
    }),
    ("has", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.has(&k?)?))))
    }),
    ("contains", 1, |args, cv| {
        let vals = args.get(0).run(cv.clone());
        Box::new(vals.map(move |y| Ok(Val::Bool(cv.1.contains(&y?)))))
    }),
    ("split", 1, |args, cv| {
        let seps = args.get(0).run(cv.clone());
        Box::new(seps.map(move |sep| Ok(Val::arr(split(cv.1.as_str()?, sep?.as_str()?)))))
    }),
    ("first", 1, |args, cv| Box::new(args.get(0).run(cv).take(1))),
    ("last", 1, |args, cv| {
        let last = args.get(0).run(cv).try_fold(None, |_, x| Ok(Some(x?)));
        then(last, |y| Box::new(y.map(Ok).into_iter()))
    }),
    ("limit", 2, |args, cv| {
        let n = args.get(0).run(cv.clone()).map(|n| n?.as_int());
        let f = move |n| args.get(1).run(cv.clone()).take(n);
        let pos = |n: isize| n.try_into().unwrap_or(0usize);
        Box::new(n.flat_map(move |n| then(n, |n| Box::new(f(pos(n))))))
    }),
    // `range(min; max)` returns all integers `n` with `min <= n < max`.
    //
    // This implements a ~10x faster version of:
    // ~~~ text
    // range(min; max):
    //   min as $min | max as $max | $min | select(. < $max) |
    //   recurse(.+1 | select(. < $max))
    // ~~~
    ("range", 2, |args, cv| {
        let prod = args.get(0).cartesian(args.get(1), cv);
        let ranges = prod.map(|(l, u)| Ok((l?.as_int()?, u?.as_int()?)));
        let f = |(l, u)| (l..u).map(|i| Ok(Val::Int(i)));
        Box::new(ranges.flat_map(move |range| then(range, |lu| Box::new(f(lu)))))
    }),
    ("recurse_inner", 1, |args, cv| {
        args.get(0).recurse(true, false, cv)
    }),
    ("recurse_outer", 1, |args, cv| {
        args.get(0).recurse(false, true, cv)
    }),
    ("startswith", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.as_str()?.starts_with(&**k?.as_str()?)))))
    }),
    ("endswith", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.as_str()?.ends_with(&**k?.as_str()?)))))
    }),
    ("ltrimstr", 1, |args, cv| {
        Box::new(args.get(0).run(cv.clone()).map(move |pre| {
            Ok(Val::Str(strip(cv.1.as_str()?, &pre?.to_str()?, |s, o| {
                s.strip_prefix(o)
            })))
        }))
    }),
    ("rtrimstr", 1, |args, cv| {
        Box::new(args.get(0).run(cv.clone()).map(move |suf| {
            Ok(Val::Str(strip(cv.1.as_str()?, &suf?.to_str()?, |s, o| {
                s.strip_suffix(o)
            })))
        }))
    }),
    ("@text", 0, |_, cv| {
        box_once(Ok(Val::str(cv.1.to_string_or_clone())))
    }),
    ("@json", 0, |_, cv| box_once(Ok(Val::str(cv.1.to_string())))),
    ("@csv", 0, |_, cv| {
        box_once(cv.1.as_arr().and_then(|a| to_csv(a)).map(Val::str))
    }),
];

#[cfg(feature = "std")]
fn now() -> Result<f64, Error> {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|x| x.as_secs_f64())
        .map_err(|e| Error::Custom(e.to_string()))
}

#[cfg(feature = "std")]
const STD: &[(&str, usize, RunPtr)] = &[("now", 0, |_, _| box_once(now().map(Val::Float)))];

#[cfg(feature = "format")]
fn to_tsv(vs: &[Val]) -> Result<String, Error> {
    let fs = |s: &str| {
        let patterns = &["\n", "\r", "\t", "\\"];
        let replace_with = &["\\n", "\\r", "\\t", "\\\\"];
        let ac = aho_corasick::AhoCorasick::new(patterns).unwrap();
        ac.replace_all(s, replace_with)
    };
    let fr = |v| fmt_row(v, fs);
    Ok(vs.iter().map(fr).collect::<Result<Vec<_>, _>>()?.join("\t"))
}

#[cfg(feature = "format")]
const FORMAT: &[(&str, usize, RunPtr)] = &[
    ("@tsv", 0, |_, cv| {
        box_once(cv.1.as_arr().and_then(|a| to_tsv(a)).map(Val::str))
    }),
    ("@html", 0, |_, cv| {
        box_once(Ok(Val::str(
            html_escape::encode_safe(&cv.1.to_string_or_clone()).into_owned(),
        )))
    }),
    ("@uri", 0, |_, cv| {
        box_once(Ok(Val::str(
            urlencoding::encode(&cv.1.to_string_or_clone()).into_owned(),
        )))
    }),
    ("@urid", 0, |_, cv| {
        box_once(
            urlencoding::decode(&cv.1.to_string_or_clone())
                .map_err(Error::from_any)
                .map(|s| Val::str(s.into_owned())),
        )
    }),
    ("@sh", 0, |_, cv| {
        box_once(Ok(Val::str(
            shell_escape::escape(cv.1.to_string_or_clone().into()).into_owned(),
        )))
    }),
    ("@base64", 0, |_, cv| {
        use base64::{engine::general_purpose, Engine as _};
        box_once(Ok(Val::str(
            general_purpose::STANDARD.encode(cv.1.to_string_or_clone()),
        )))
    }),
    ("@base64d", 0, |_, cv| {
        use base64::{engine::general_purpose, Engine as _};
        box_once(
            general_purpose::STANDARD
                .decode(cv.1.to_string_or_clone())
                .map_err(Error::from_any)
                .and_then(|d| {
                    std::str::from_utf8(&d)
                        .map_err(Error::from_any)
                        .map(|s| Val::str(s.to_owned()))
                }),
        )
    }),
];

#[cfg(feature = "math")]
const MATH: &[(&str, usize, RunPtr)] = &[
    math::f_f!(acos),
    math::f_f!(acosh),
    math::f_f!(asin),
    math::f_f!(asinh),
    math::f_f!(atan),
    math::f_f!(atanh),
    math::f_f!(cbrt),
    math::f_f!(cos),
    math::f_f!(cosh),
    math::f_f!(erf),
    math::f_f!(erfc),
    math::f_f!(exp),
    math::f_f!(exp10),
    math::f_f!(exp2),
    math::f_f!(expm1),
    math::f_f!(fabs),
    math::f_fi!(frexp),
    math::f_i!(ilogb),
    math::f_f!(j0),
    math::f_f!(j1),
    math::f_f!(lgamma),
    math::f_f!(log),
    math::f_f!(log10),
    math::f_f!(log1p),
    math::f_f!(log2),
    // logb is implemented in jaq-std
    math::f_ff!(modf),
    math::f_f!("nearbyint", round),
    // pow10 is implemented in jaq-std
    math::f_f!(rint),
    // significand is implemented in jaq-std
    math::f_f!(sin),
    math::f_f!(sinh),
    math::f_f!(sqrt),
    math::f_f!(tan),
    math::f_f!(tanh),
    math::f_f!(tgamma),
    math::f_f!(trunc),
    math::f_f!(y0),
    math::f_f!(y1),
    math::ff_f!(atan2),
    math::ff_f!(copysign),
    // drem is implemented in jaq-std
    math::ff_f!(fdim),
    math::ff_f!(fmax),
    math::ff_f!(fmin),
    math::ff_f!(fmod),
    math::ff_f!(hypot),
    math::if_f!(jn),
    math::fi_f!(ldexp),
    math::ff_f!(nextafter),
    // nexttoward is implemented in jaq-std
    math::ff_f!(pow),
    math::ff_f!(remainder),
    // scalb is implemented in jaq-std
    math::fi_f!("scalbln", scalbn),
    math::if_f!(yn),
    math::fff_f!(fma),
];

#[cfg(feature = "regex")]
fn re<'a, F: FilterT<'a>>(re: F, flags: F, s: bool, m: bool, cv: (Ctx<'a>, Val)) -> ValRs<'a> {
    let re_flags = re.cartesian(flags, (cv.0, cv.1.clone()));

    Box::new(re_flags.map(move |(re, flags)| {
        Ok(Val::arr(regex::regex(
            cv.1.as_str()?,
            re?.as_str()?,
            flags?.as_str()?,
            (s, m),
        )?))
    }))
}

#[cfg(feature = "regex")]
const REGEX: &[(&str, usize, RunPtr)] = &[
    ("matches", 2, |args, cv| {
        re(args.get(0), args.get(1), false, true, cv)
    }),
    ("split_matches", 2, |args, cv| {
        re(args.get(0), args.get(1), true, true, cv)
    }),
    ("split_", 2, |args, cv| {
        re(args.get(0), args.get(1), true, false, cv)
    }),
];

#[cfg(feature = "time")]
const TIME: &[(&str, usize, RunPtr)] = &[
    ("fromdateiso8601", 0, |_, cv| {
        then(cv.1.as_str(), |s| box_once(time::from_iso8601(s)))
    }),
    ("todateiso8601", 0, |_, cv| {
        box_once(time::to_iso8601(&cv.1).map(Val::str))
    }),
];

const CORE_UPDATE: &[(&str, usize, RunPtr, UpdatePtr)] = &[
    (
        "empty",
        0,
        |_, _| Box::new(core::iter::empty()),
        |_, cv, _| box_once(Ok(cv.1)),
    ),
    (
        "error",
        0,
        |_, cv| box_once(Err(Error::Val(cv.1))),
        |_, cv, _| box_once(Err(Error::Val(cv.1))),
    ),
    (
        "recurse",
        1,
        |args, cv| args.get(0).recurse(true, true, cv),
        |args, cv, f| args.get(0).recurse_update(cv, f),
    ),
];

#[cfg(feature = "log")]
fn debug<T: core::fmt::Display>(x: T) -> T {
    log::debug!("{}", x);
    x
}

#[cfg(feature = "log")]
const LOG: &[(&str, usize, RunPtr, UpdatePtr)] = &[(
    "debug",
    0,
    |_, cv| box_once(Ok(debug(cv.1))),
    |_, cv, f| f(debug(cv.1)),
)];
