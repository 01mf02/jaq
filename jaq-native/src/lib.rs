//! Core filters.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod regex;
mod time;

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use jaq_core::results::{box_once, then};
use jaq_core::{Ctx, FilterT, Native, RunPtr, UpdatePtr};
use jaq_core::{Error, Val, ValRs};

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
#[cfg(all(feature = "std", feature = "log", feature = "regex", feature = "time"))]
pub fn core() -> impl Iterator<Item = (String, usize, Native)> {
    minimal()
        .chain(run(STD))
        .chain(upd(LOG))
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
    ("utf8bytelength", 0, |_, cv| box_once(cv.1.byte_len())),
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
        box_once(cv.1.sort_by(|v| args.get(0).run((cv.0.clone(), v))))
    }),
    ("group_by", 1, |args, cv| {
        box_once(cv.1.group_by(|v| args.get(0).run((cv.0.clone(), v))))
    }),
    ("min_by", 1, |args, cv| {
        box_once(cv.1.cmp_by(|v| args.get(0).run((cv.0.clone(), v)), |my, y| y < my))
    }),
    ("max_by", 1, |args, cv| {
        box_once(cv.1.cmp_by(|v| args.get(0).run((cv.0.clone(), v)), |my, y| y >= my))
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
        Box::new(seps.map(move |sep| Ok(Val::arr(cv.1.split(&sep?)?))))
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
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.starts_with(&k?)?))))
    }),
    ("endswith", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.ends_with(&k?)?))))
    }),
    ("ltrimstr", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Str(cv.1.strip_prefix(&k?)?))))
    }),
    ("rtrimstr", 1, |args, cv| {
        let keys = args.get(0).run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Str(cv.1.strip_suffix(&k?)?))))
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

#[cfg(feature = "regex")]
fn re<'a, F: FilterT<'a>>(re: F, flags: F, s: bool, m: bool, cv: (Ctx<'a>, Val)) -> ValRs<'a> {
    let flags_re = flags.cartesian(re, (cv.0, cv.1.clone()));

    Box::new(flags_re.map(move |(flags, re)| {
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
