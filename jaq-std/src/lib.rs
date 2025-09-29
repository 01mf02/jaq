//! Standard library for the jq language.
//!
//! The standard library provides a set of filters.
//! These filters are either implemented as definitions or as functions.
//! For example, the standard library provides the `map(f)` filter,
//! which is defined using the more elementary filter `[.[] | f]`.
//!
//! If you want to use the standard library in jaq, then
//! you'll likely only need [`funs`] and [`defs`].
//! Most other functions are relevant if you
//! want to implement your own native filters.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub mod input;
#[cfg(feature = "math")]
mod math;
#[cfg(feature = "regex")]
mod regex;
#[cfg(feature = "time")]
mod time;

use alloc::string::{String, ToString};
use alloc::{boxed::Box, vec::Vec};
use bstr::{BStr, ByteSlice};
use jaq_core::box_iter::{box_once, then, BoxIter};
use jaq_core::{load, Bind, Cv, DataT, Error, Exn, Native, RunPtr, ValR, ValT as _, ValX, ValXs};

/// Definitions of the standard library.
pub fn defs() -> impl Iterator<Item = load::parse::Def<&'static str>> {
    load::parse(include_str!("defs.jq"), |p| p.defs())
        .unwrap()
        .into_iter()
}

/// Name, arguments, and implementation of a filter.
pub type Filter<F> = (&'static str, Box<[Bind]>, F);

/// Named filters available by default in jaq
/// which are implemented as native filters, such as `length`, `keys`, ...,
/// but also `now`, `debug`, `fromdateiso8601`, ...
///
/// This is the combination of [`base_funs`] and [`extra_funs`].
/// It does not include filters implemented by definition, such as `map`.
#[cfg(all(
    feature = "std",
    feature = "format",
    feature = "log",
    feature = "math",
    feature = "regex",
    feature = "time",
))]
pub fn funs<D: DataT>() -> impl Iterator<Item = Filter<Native<D>>>
where
    for<'a> D::V<'a>: ValT,
{
    base_funs().chain(extra_funs())
}

/// Minimal set of filters that are generic over the value type.
/// Return the minimal set of named filters available in jaq
/// which are implemented as native filters, such as `length`, `keys`, ...,
/// but not `now`, `debug`, `fromdateiso8601`, ...
///
/// Does not return filters from the standard library, such as `map`.
pub fn base_funs<D: DataT>() -> impl Iterator<Item = Filter<Native<D>>>
where
    for<'a> D::V<'a>: ValT,
{
    let base_run = base_run().into_vec().into_iter().map(run);
    let base_paths = base_paths().into_vec().into_iter().map(paths);
    base_run.chain(base_paths).chain([upd(error())])
}

/// Supplementary set of filters that are generic over the value type.
#[cfg(all(
    feature = "std",
    feature = "format",
    feature = "log",
    feature = "math",
    feature = "regex",
    feature = "time",
))]
pub fn extra_funs<D: DataT>() -> impl Iterator<Item = Filter<Native<D>>>
where
    for<'a> D::V<'a>: ValT,
{
    [std(), format(), math(), regex(), time()]
        .into_iter()
        .flat_map(|fs| fs.into_vec().into_iter().map(run))
        .chain([debug(), stderr()].map(upd))
}

/// Values that the standard library can operate on.
pub trait ValT: jaq_core::ValT + Ord + From<f64> + From<usize> {
    /// Convert an array into a sequence.
    ///
    /// This returns the original value as `Err` if it is not an array.
    fn into_seq<S: FromIterator<Self>>(self) -> Result<S, Self>;

    /// True if the value is integer.
    fn is_int(&self) -> bool;

    /// Use the value as machine-sized integer.
    ///
    /// If this function returns `Some(_)`, then [`Self::is_int`] must return true.
    /// However, the other direction must not necessarily be the case, because
    /// there may be integer values that are not representable by `isize`.
    fn as_isize(&self) -> Option<isize>;

    /// Use the value as floating-point number.
    ///
    /// This succeeds for all numeric values,
    /// rounding too large/small ones to +/- Infinity.
    fn as_f64(&self) -> Option<f64>;

    /// True if the value is interpreted as UTF-8 string.
    fn is_utf8_str(&self) -> bool;

    /// If the value is a string (whatever its interpretation), return its bytes.
    fn as_bytes(&self) -> Option<&[u8]>;

    /// If the value is interpreted as UTF-8 string, return its bytes.
    fn as_utf8_bytes(&self) -> Option<&[u8]> {
        self.is_utf8_str().then(|| self.as_bytes()).flatten()
    }

    /// If the value is a string (whatever its interpretation), return its bytes, else fail.
    fn try_as_bytes(&self) -> Result<&[u8], Error<Self>> {
        self.as_bytes().ok_or_else(|| self.fail_str())
    }

    /// If the value is interpreted as UTF-8 string, return its bytes, else fail.
    fn try_as_utf8_bytes(&self) -> Result<&[u8], Error<Self>> {
        self.as_utf8_bytes().ok_or_else(|| self.fail_str())
    }

    /// If the value is a string and `sub` points to a slice of the string,
    /// shorten the string to `sub`, else panic.
    fn as_sub_str(&self, sub: &[u8]) -> Self;

    /// Interpret bytes as UTF-8 string value.
    fn from_utf8_bytes(b: impl AsRef<[u8]> + Send + 'static) -> Self;
}

/// Convenience trait for implementing the core functions.
trait ValTx: ValT + Sized {
    fn into_vec(self) -> Result<Vec<Self>, Error<Self>> {
        self.into_seq().map_err(|v| Error::typ(v, "array"))
    }

    fn try_as_isize(&self) -> Result<isize, Error<Self>> {
        self.as_isize()
            .ok_or_else(|| Error::typ(self.clone(), "integer"))
    }

    #[cfg(feature = "math")]
    /// Use as an i32 to be given as an argument to a libm function.
    fn try_as_i32(&self) -> Result<i32, Error<Self>> {
        self.try_as_isize()?.try_into().map_err(Error::str)
    }

    fn try_as_f64(&self) -> Result<f64, Error<Self>> {
        self.as_f64()
            .ok_or_else(|| Error::typ(self.clone(), "number"))
    }

    /// Apply a function to an array.
    fn mutate_arr(self, f: impl FnOnce(&mut Vec<Self>)) -> ValR<Self> {
        let mut a = self.into_vec()?;
        f(&mut a);
        Ok(Self::from_iter(a))
    }

    /// Apply a function to an array.
    fn try_mutate_arr<F>(self, f: F) -> ValX<Self>
    where
        F: FnOnce(&mut Vec<Self>) -> Result<(), Exn<Self>>,
    {
        let mut a = self.into_vec()?;
        f(&mut a)?;
        Ok(Self::from_iter(a))
    }

    fn round(self, f: impl FnOnce(f64) -> f64) -> ValR<Self> {
        Ok(if self.is_int() {
            self
        } else {
            let f = f(self.try_as_f64()?);
            if f.is_finite() {
                if isize::MIN as f64 <= f && f <= isize::MAX as f64 {
                    Self::from(f as isize)
                } else {
                    // print floating-point number without decimal places,
                    // i.e. like an integer
                    Self::from_num(&alloc::format!("{f:.0}"))?
                }
            } else {
                Self::from(f)
            }
        })
    }

    /// If the value is interpreted as UTF-8 string,
    /// return its `str` representation.
    fn try_as_str(&self) -> Result<&str, Error<Self>> {
        self.try_as_utf8_bytes()
            .and_then(|s| core::str::from_utf8(s).map_err(Error::str))
    }

    fn map_utf8_str<B>(self, f: impl FnOnce(&[u8]) -> B) -> ValR<Self>
    where
        B: AsRef<[u8]> + Send + 'static,
    {
        Ok(Self::from_utf8_bytes(f(self.try_as_utf8_bytes()?)))
    }

    fn trim_utf8_with(&self, f: impl FnOnce(&[u8]) -> &[u8]) -> ValR<Self> {
        Ok(self.as_sub_str(f(self.try_as_utf8_bytes()?)))
    }

    /// Helper function to strip away the prefix or suffix of a string.
    fn strip_fix<F>(self, fix: &Self, f: F) -> Result<Self, Error<Self>>
    where
        F: for<'a> FnOnce(&'a [u8], &[u8]) -> Option<&'a [u8]>,
    {
        Ok(match f(self.try_as_bytes()?, fix.try_as_bytes()?) {
            Some(sub) => self.as_sub_str(sub),
            None => self,
        })
    }

    fn fail_str(&self) -> Error<Self> {
        Error::typ(self.clone(), "string")
    }
}
impl<T: ValT> ValTx for T {}

/// Convert a filter with a run pointer to a native filter.
pub fn run<D: DataT>((name, arity, run): Filter<RunPtr<D>>) -> Filter<Native<D>> {
    (name, arity, Native::new(run))
}

type RunPathsPtr<D> = (RunPtr<D>, jaq_core::PathsPtr<D>);
type RunPathsUpdatePtr<D> = (RunPtr<D>, jaq_core::PathsPtr<D>, jaq_core::UpdatePtr<D>);

/// Convert a filter with a run and an update pointer to a native filter.
fn paths<D: DataT>((name, arity, (run, paths)): Filter<RunPathsPtr<D>>) -> Filter<Native<D>> {
    (name, arity, Native::new(run).with_paths(paths))
}

/// Convert a filter with a run, a paths, and an update pointer to a native filter.
fn upd<D: DataT>((name, arity, (r, p, u)): Filter<RunPathsUpdatePtr<D>>) -> Filter<Native<D>> {
    (name, arity, Native::new(r).with_paths(p).with_update(u))
}

/// Return all path-value pairs `($p, $v)`, such that `getpath($p) = $v`.
fn path_values<'a, V: ValT + 'a>(v: V, path: Vec<V>) -> BoxIter<'a, (V, V)> {
    let head = (path.iter().cloned().collect(), v.clone());
    let f = move |k| path.iter().cloned().chain([k]).collect();
    let kvs = v.key_values().flatten();
    let kvs: Vec<_> = kvs.map(|(k, v)| (k, v.clone())).collect();
    let tail = kvs.into_iter().flat_map(move |(k, v)| path_values(v, f(k)));
    Box::new(core::iter::once(head).chain(tail))
}

/// Sort array by the given function.
fn sort_by<'a, V: ValT>(xs: &mut [V], f: impl Fn(V) -> ValXs<'a, V>) -> Result<(), Exn<V>> {
    // Some(e) iff an error has previously occurred
    let mut err = None;
    xs.sort_by_cached_key(|x| {
        if err.is_some() {
            return Vec::new();
        };
        match f(x.clone()).collect() {
            Ok(y) => y,
            Err(e) => {
                err = Some(e);
                Vec::new()
            }
        }
    });
    err.map_or(Ok(()), Err)
}

/// Group an array by the given function.
fn group_by<'a, V: ValT>(xs: Vec<V>, f: impl Fn(V) -> ValXs<'a, V>) -> ValX<V> {
    let mut yx: Vec<(Vec<V>, V)> = xs
        .into_iter()
        .map(|x| Ok((f(x.clone()).collect::<Result<_, _>>()?, x)))
        .collect::<Result<_, Exn<_>>>()?;

    yx.sort_by(|(y1, _), (y2, _)| y1.cmp(y2));

    let mut grouped = Vec::new();
    let mut yx = yx.into_iter();
    if let Some((mut group_y, first_x)) = yx.next() {
        let mut group = Vec::from([first_x]);
        for (y, x) in yx {
            if group_y != y {
                grouped.push(V::from_iter(core::mem::take(&mut group)));
                group_y = y;
            }
            group.push(x);
        }
        if !group.is_empty() {
            grouped.push(V::from_iter(group));
        }
    }

    Ok(V::from_iter(grouped))
}

/// Get the minimum or maximum element from an array according to the given function.
fn cmp_by<'a, V: Clone, F, R>(xs: Vec<V>, f: F, replace: R) -> Result<Option<V>, Exn<V>>
where
    F: Fn(V) -> ValXs<'a, V>,
    R: Fn(&[V], &[V]) -> bool,
{
    let iter = xs.into_iter();
    let mut iter = iter.map(|x| (x.clone(), f(x).collect::<Result<Vec<_>, _>>()));
    let (mut mx, mut my) = if let Some((x, y)) = iter.next() {
        (x, y?)
    } else {
        return Ok(None);
    };
    for (x, y) in iter {
        let y = y?;
        if replace(&my, &y) {
            (mx, my) = (x, y);
        }
    }
    Ok(Some(mx))
}

/// Convert a string into an array of its Unicode codepoints (with negative integers representing UTF-8 errors).
fn explode<V: ValT>(s: &[u8]) -> impl Iterator<Item = ValR<V>> + '_ {
    let invalid = [].iter();
    Explode { s, invalid }.map(|r| match r {
        Err(b) => Ok((-(b as isize)).into()),
        // conversion from u32 to isize may fail on 32-bit systems for high values of c
        Ok(c) => Ok(isize::try_from(c as u32).map_err(Error::str)?.into()),
    })
}

struct Explode<'a> {
    s: &'a [u8],
    invalid: core::slice::Iter<'a, u8>,
}
impl Iterator for Explode<'_> {
    type Item = Result<char, u8>;
    fn next(&mut self) -> Option<Self::Item> {
        self.invalid.next().map(|next| Err(*next)).or_else(|| {
            let (c, size) = bstr::decode_utf8(self.s);
            let (consumed, rest) = self.s.split_at(size);
            self.s = rest;
            c.map(Ok).or_else(|| {
                // invalid UTF-8 sequence, emit all invalid bytes
                self.invalid = consumed.iter();
                self.invalid.next().map(|next| Err(*next))
            })
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let max = self.s.len();
        let min = self.s.len() / 4;
        let inv = self.invalid.as_slice().len();
        (min + inv, Some(max + inv))
    }
}

/// Convert an array of Unicode codepoints (with negative integers representing UTF-8 errors) into a string.
fn implode<V: ValT>(xs: &[V]) -> Result<Vec<u8>, Error<V>> {
    let mut v = Vec::with_capacity(xs.len());
    for x in xs {
        // on 32-bit systems, some high u32 values cannot be represented as isize
        let i = x.try_as_isize()?;
        if let Ok(b) = u8::try_from(-i) {
            v.push(b)
        } else {
            // may fail e.g. on `[1114112] | implode`
            let c = u32::try_from(i).ok().and_then(char::from_u32);
            let c = c.ok_or_else(|| Error::str(format_args!("cannot use {i} as character")))?;
            v.extend(c.encode_utf8(&mut [0; 4]).as_bytes())
        }
    }
    Ok(v)
}

/// This implements a ~10x faster version of:
/// ~~~ text
/// def range($from; $to; $by): $from |
///    if $by > 0 then while(.  < $to; . + $by)
///  elif $by < 0 then while(.  > $to; . + $by)
///    else            while(. != $to; . + $by)
///    end;
/// ~~~
fn range<V: ValT>(mut from: ValX<V>, to: V, by: V) -> impl Iterator<Item = ValX<V>> {
    use core::cmp::Ordering::{Equal, Greater, Less};
    let cmp = by.partial_cmp(&V::from(0usize)).unwrap_or(Equal);
    core::iter::from_fn(move || match from.clone() {
        Ok(x) => match cmp {
            Greater => x < to,
            Less => x > to,
            Equal => x != to,
        }
        .then(|| core::mem::replace(&mut from, (x + by.clone()).map_err(Exn::from))),
        e @ Err(_) => {
            // return None after the error
            from = Ok(to.clone());
            Some(e)
        }
    })
}

fn byte_offset<V: ValT>(fixed: V, loose: V) -> ValR<V> {
    let range = |v: &V| {
        let b = v.try_as_bytes()?;
        let start = b.as_ptr() as usize;
        Ok(start..start + b.len())
    };
    let f = range(&fixed)?;
    let l = range(&loose)?;
    if f.start <= l.end && l.start <= f.end {
        V::from(l.start) - V::from(f.start)
    } else {
        Err(Error::str(format_args!(
            "{fixed} does not overlap with {loose}"
        )))
    }
}

fn once_or_empty<'a, T: 'a, E: 'a>(r: Result<Option<T>, E>) -> BoxIter<'a, Result<T, E>> {
    Box::new(r.transpose().into_iter())
}

/// Box Once and Map Errors to exceptions.
fn bome<'a, V: 'a>(r: ValR<V>) -> ValXs<'a, V> {
    box_once(r.map_err(Exn::from))
}

/// Create a filter that takes a single variable argument and whose output is given by
/// the function `f` that takes the input value and the value of the variable.
pub fn unary<'a, D: DataT>(
    mut cv: Cv<'a, D>,
    f: impl Fn(D::V<'a>, D::V<'a>) -> ValR<D::V<'a>> + 'a,
) -> ValXs<'a, D::V<'a>> {
    bome(f(cv.1, cv.0.pop_var()))
}

/// Creates `n` variable arguments.
pub fn v(n: usize) -> Box<[Bind]> {
    core::iter::repeat(Bind::Var(())).take(n).collect()
}

#[allow(clippy::unit_arg)]
fn base_run<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    let f = || [Bind::Fun(())].into();
    Box::new([
        ("path", f(), |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let cvp = (fc, (cv.1, Default::default()));
            Box::new(f.paths(cvp).map(|vp| {
                vp.map(|(_v, path)| {
                    let mut path: Vec<_> = path.iter().cloned().collect();
                    path.reverse();
                    path.into_iter().collect()
                })
            }))
        }),
        ("floor", v(0), |cv| bome(cv.1.round(f64::floor))),
        ("round", v(0), |cv| bome(cv.1.round(f64::round))),
        ("ceil", v(0), |cv| bome(cv.1.round(f64::ceil))),
        ("utf8bytelength", v(0), |cv| {
            bome(cv.1.try_as_utf8_bytes().map(|s| (s.len() as isize).into()))
        }),
        ("explode", v(0), |cv| {
            bome(cv.1.try_as_utf8_bytes().and_then(|s| explode(s).collect()))
        }),
        ("implode", v(0), |cv| {
            let implode = |s: Vec<_>| implode(&s);
            bome(cv.1.into_vec().and_then(implode).map(D::V::from_utf8_bytes))
        }),
        ("ascii_downcase", v(0), |cv| {
            bome(cv.1.map_utf8_str(ByteSlice::to_ascii_lowercase))
        }),
        ("ascii_upcase", v(0), |cv| {
            bome(cv.1.map_utf8_str(ByteSlice::to_ascii_uppercase))
        }),
        ("byteoffset", v(2), |mut cv| {
            bome(byte_offset(cv.0.pop_var(), cv.0.pop_var()))
        }),
        ("reverse", v(0), |cv| bome(cv.1.mutate_arr(|a| a.reverse()))),
        ("keys_unsorted", v(0), |cv| {
            bome(cv.1.key_values().map(|kv| kv.map(|(k, _v)| k)).collect())
        }),
        ("path_values", v(0), |cv| {
            let pair = |(p, v)| Ok([p, v].into_iter().collect());
            Box::new(path_values(cv.1, Vec::new()).skip(1).map(pair))
        }),
        ("paths", v(0), |cv| {
            Box::new(path_values(cv.1, Vec::new()).skip(1).map(|(p, _v)| Ok(p)))
        }),
        ("sort", v(0), |cv| bome(cv.1.mutate_arr(|a| a.sort()))),
        ("sort_by", f(), |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let f = move |v| f.run((fc.clone(), v));
            box_once(cv.1.try_mutate_arr(|a| sort_by(a, f)))
        }),
        ("group_by", f(), |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let f = move |v| f.run((fc.clone(), v));
            box_once((|| group_by(cv.1.into_vec()?, f))())
        }),
        ("min_by_or_empty", f(), |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let f = move |a| cmp_by(a, |v| f.run((fc.clone(), v)), |my, y| y < my);
            once_or_empty(cv.1.into_vec().map_err(Exn::from).and_then(f))
        }),
        ("max_by_or_empty", f(), |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let f = move |a| cmp_by(a, |v| f.run((fc.clone(), v)), |my, y| y >= my);
            once_or_empty(cv.1.into_vec().map_err(Exn::from).and_then(f))
        }),
        ("range", v(3), |mut cv| {
            let by = cv.0.pop_var();
            let to = cv.0.pop_var();
            let from = cv.0.pop_var();
            Box::new(range(Ok(from), to, by))
        }),
        ("startswith", v(1), |cv| {
            unary(cv, |v, s| {
                Ok(v.try_as_bytes()?.starts_with(s.try_as_bytes()?).into())
            })
        }),
        ("endswith", v(1), |cv| {
            unary(cv, |v, s| {
                Ok(v.try_as_bytes()?.ends_with(s.try_as_bytes()?).into())
            })
        }),
        ("ltrimstr", v(1), |cv| {
            unary(cv, |v, pre| v.strip_fix(&pre, <[u8]>::strip_prefix))
        }),
        ("rtrimstr", v(1), |cv| {
            unary(cv, |v, suf| v.strip_fix(&suf, <[u8]>::strip_suffix))
        }),
        ("trim", v(0), |cv| {
            bome(cv.1.trim_utf8_with(ByteSlice::trim))
        }),
        ("ltrim", v(0), |cv| {
            bome(cv.1.trim_utf8_with(ByteSlice::trim_start))
        }),
        ("rtrim", v(0), |cv| {
            bome(cv.1.trim_utf8_with(ByteSlice::trim_end))
        }),
        ("escape_csv", v(0), |cv| {
            bome(
                cv.1.try_as_utf8_bytes()
                    .map(|s| ValT::from_utf8_bytes(s.replace(b"\"", b"\"\""))),
            )
        }),
        ("escape_sh", v(0), |cv| {
            bome(
                cv.1.try_as_utf8_bytes()
                    .map(|s| ValT::from_utf8_bytes(s.replace(b"'", b"'\\''"))),
            )
        }),
    ])
}

macro_rules! first {
    ( $run:ident ) => {
        |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            Box::new(f.$run((fc, cv.1)).next().into_iter())
        }
    };
}
macro_rules! last {
    ( $run:ident ) => {
        |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            once_or_empty(f.$run((fc, cv.1)).try_fold(None, |_, x| x.map(Some)))
        }
    };
}
macro_rules! limit {
    ( $run:ident ) => {
        |mut cv| {
            let (f, fc) = cv.0.pop_fun();
            let n = cv.0.pop_var();
            let pos = |n: isize| n.try_into().unwrap_or(0usize);
            then(n.try_as_isize().map_err(Exn::from), |n| match pos(n) {
                0 => Box::new(core::iter::empty()),
                n => Box::new(f.$run((fc, cv.1)).take(n)),
            })
        }
    };
}

fn base_paths<D: DataT>() -> Box<[Filter<RunPathsPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    let f = || [Bind::Fun(())].into();
    let vf = || [Bind::Var(()), Bind::Fun(())].into();
    Box::new([
        ("first", f(), (first!(run), first!(paths))),
        ("last", f(), (last!(run), last!(paths))),
        ("limit", vf(), (limit!(run), limit!(paths))),
    ])
}

#[cfg(feature = "std")]
fn now<V: From<String>>() -> Result<f64, Error<V>> {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|x| x.as_secs_f64())
        .map_err(Error::str)
}

#[cfg(feature = "std")]
fn std<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    use std::env::vars;
    Box::new([
        ("env", v(0), |_| {
            bome(D::V::from_map(
                vars().map(|(k, v)| (D::V::from(k), D::V::from(v))),
            ))
        }),
        ("now", v(0), |_| bome(now().map(D::V::from))),
        ("halt", v(0), |_| std::process::exit(0)),
        ("halt_error", v(1), |mut cv| {
            bome(cv.0.pop_var().try_as_isize().map(|exit_code| {
                if let Some(s) = cv.1.as_utf8_bytes() {
                    std::print!("{}", BStr::new(s));
                } else {
                    std::println!("{}", cv.1);
                }
                std::process::exit(exit_code as i32)
            }))
        }),
    ])
}

#[cfg(feature = "format")]
fn replace(s: &[u8], patterns: &[&str], replacements: &[&str]) -> Vec<u8> {
    let ac = aho_corasick::AhoCorasick::new(patterns).unwrap();
    ac.replace_all_bytes(s, replacements)
}

#[cfg(feature = "format")]
fn format<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    Box::new([
        ("escape_html", v(0), |cv| {
            let pats = ["<", ">", "&", "\'", "\""];
            let reps = ["&lt;", "&gt;", "&amp;", "&apos;", "&quot;"];
            bome(cv.1.map_utf8_str(|s| replace(s, &pats, &reps)))
        }),
        ("escape_tsv", v(0), |cv| {
            let pats = ["\n", "\r", "\t", "\\", "\0"];
            let reps = ["\\n", "\\r", "\\t", "\\\\", "\\0"];
            bome(cv.1.map_utf8_str(|s| replace(s, &pats, &reps)))
        }),
        ("encode_uri", v(0), |cv| {
            bome(cv.1.map_utf8_str(|s| urlencoding::encode_binary(s).to_string()))
        }),
        ("decode_uri", v(0), |cv| {
            bome(cv.1.map_utf8_str(|s| urlencoding::decode_binary(s).to_vec()))
        }),
        ("encode_base64", v(0), |cv| {
            use base64::{engine::general_purpose::STANDARD, Engine};
            bome(cv.1.map_utf8_str(|s| STANDARD.encode(s)))
        }),
        ("decode_base64", v(0), |cv| {
            use base64::{engine::general_purpose::STANDARD, Engine};
            bome(cv.1.try_as_utf8_bytes().and_then(|s| {
                STANDARD
                    .decode(s)
                    .map_err(Error::str)
                    .map(ValT::from_utf8_bytes)
            }))
        }),
    ])
}

#[cfg(feature = "math")]
fn math<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    let rename = |name, (_name, arity, f): Filter<RunPtr<D>>| (name, arity, f);
    Box::new([
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
        rename("nearbyint", math::f_f!(round)),
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
        rename("scalbln", math::fi_f!(scalbn)),
        math::if_f!(yn),
        math::fff_f!(fma),
    ])
}

#[cfg(feature = "regex")]
fn re<'a, D: DataT>(s: bool, m: bool, mut cv: Cv<'a, D>) -> ValR<D::V<'a>>
where
    D::V<'a>: ValT,
{
    let flags = cv.0.pop_var();
    let re = cv.0.pop_var();

    use crate::regex::Part::{Matches, Mismatch};
    let fail_flag = |e| Error::str(format_args!("invalid regex flag: {e}"));
    let fail_re = |e| Error::str(format_args!("invalid regex: {e}"));

    let flags = regex::Flags::new(flags.try_as_str()?).map_err(fail_flag)?;
    let re = flags.regex(re.try_as_str()?).map_err(fail_re)?;
    let out = regex::regex(cv.1.try_as_utf8_bytes()?, &re, flags, (s, m));
    let sub = |s| cv.1.as_sub_str(s);
    let out = out.into_iter().map(|out| match out {
        Matches(ms) => ms
            .into_iter()
            .map(|m| D::V::from_map(m.fields(sub)))
            .collect(),
        Mismatch(s) => Ok(sub(s)),
    });
    out.collect()
}

#[cfg(feature = "regex")]
fn regex<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    let vv = || [Bind::Var(()), Bind::Var(())].into();
    Box::new([
        ("matches", vv(), |cv| bome(re(false, true, cv))),
        ("split_matches", vv(), |cv| bome(re(true, true, cv))),
        ("split_", vv(), |cv| bome(re(true, false, cv))),
    ])
}

#[cfg(feature = "time")]
fn time<D: DataT>() -> Box<[Filter<RunPtr<D>>]>
where
    for<'a> D::V<'a>: ValT,
{
    use chrono::{Local, Utc};
    Box::new([
        ("fromdateiso8601", v(0), |cv| {
            bome(cv.1.try_as_str().and_then(time::from_iso8601))
        }),
        ("todateiso8601", v(0), |cv| {
            bome(time::to_iso8601(&cv.1).map(D::V::from))
        }),
        ("strftime", v(1), |cv| {
            unary(cv, |v, fmt| time::strftime(&v, fmt.try_as_str()?, Utc))
        }),
        ("strflocaltime", v(1), |cv| {
            unary(cv, |v, fmt| time::strftime(&v, fmt.try_as_str()?, Local))
        }),
        ("gmtime", v(0), |cv| bome(time::gmtime(&cv.1, Utc))),
        ("localtime", v(0), |cv| bome(time::gmtime(&cv.1, Local))),
        ("strptime", v(1), |cv| {
            unary(cv, |v, fmt| {
                time::strptime(v.try_as_str()?, fmt.try_as_str()?)
            })
        }),
        ("mktime", v(0), |cv| bome(time::mktime(&cv.1))),
    ])
}

fn error<D: DataT>() -> Filter<RunPathsUpdatePtr<D>> {
    (
        "error",
        v(0),
        (
            |cv| bome(Err(Error::new(cv.1))),
            |cv| box_once(Err(Exn::from(Error::new(cv.1 .0)))),
            |cv, _| bome(Err(Error::new(cv.1))),
        ),
    )
}

#[cfg(feature = "log")]
/// Construct a filter that applies an effect function before returning its input.
macro_rules! id_with {
    ( $eff:expr ) => {
        (
            |cv| {
                $eff(&cv.1);
                box_once(Ok(cv.1))
            },
            |cv| {
                $eff(&cv.1 .0);
                box_once(Ok(cv.1))
            },
            |cv, f| {
                $eff(&cv.1);
                f(cv.1)
            },
        )
    };
}

#[cfg(feature = "log")]
fn debug<D: DataT>() -> Filter<RunPathsUpdatePtr<D>> {
    ("debug", v(0), id_with!(|x| log::debug!("{x}")))
}

#[cfg(feature = "log")]
fn stderr<D: DataT>() -> Filter<RunPathsUpdatePtr<D>>
where
    for<'a> D::V<'a>: ValT,
{
    fn eprint_raw<V: ValT>(v: &V) {
        if let Some(s) = v.as_utf8_bytes() {
            log::error!("{}", BStr::new(s))
        } else {
            log::error!("{v}")
        }
    }
    ("stderr", v(0), id_with!(eprint_raw))
}
