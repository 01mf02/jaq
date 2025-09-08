use crate::{cbor, json, toml, xml, yaml};
use crate::{Error, Num, Tag, Type, Val, ValR, ValX};
use alloc::{boxed::Box, string::ToString, vec::Vec};
use bstr::ByteSlice;
use bytes::{BufMut, Bytes, BytesMut};
use core::fmt;
use jaq_core::box_iter::{box_once, then, BoxIter};
use jaq_core::{DataT, Exn, Native, RunPtr};
use jaq_std::{run, unary, v, Filter, ValT as _};

impl Val {
    /// Return 0 for null, the absolute value for numbers, and
    /// the length for strings, arrays, and objects.
    ///
    /// Fail on booleans.
    fn length(&self) -> ValR {
        match self {
            Val::Null => Ok(Val::from(0usize)),
            Val::Num(n) => Ok(Val::Num(n.length())),
            Val::Str(s, Tag::Utf8) => Ok(Val::from(s.chars().count() as isize)),
            Val::Str(b, Tag::Bytes | Tag::Raw) => Ok(Val::from(b.len() as isize)),
            Val::Arr(a) => Ok(Val::from(a.len() as isize)),
            Val::Obj(o) => Ok(Val::from(o.len() as isize)),
            Val::Bool(_) => Err(Error::str(format_args!("{self} has no length"))),
        }
    }

    /// Return the indices of `y` in `self`.
    fn indices<'a>(&'a self, y: &'a Val) -> Result<Box<dyn Iterator<Item = usize> + 'a>, Error> {
        match (self, y) {
            (Val::Str(_, tag @ (Tag::Bytes | Tag::Utf8)), Val::Str(y, tag_))
                if tag == tag_ && y.is_empty() =>
            {
                Ok(Box::new(core::iter::empty()))
            }
            (Val::Arr(_), Val::Arr(y)) if y.is_empty() => Ok(Box::new(core::iter::empty())),
            (Val::Str(x, Tag::Utf8), Val::Str(y, Tag::Utf8)) => {
                let index = |(i, _, _)| x.get(i..i + y.len());
                let iw = x.char_indices().map_while(index).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == *y).then_some(i))))
            }
            (Val::Str(x, tag @ Tag::Bytes), Val::Str(y, tag_)) if tag == tag_ => {
                let iw = x.windows(y.len()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == *y).then_some(i))))
            }
            (Val::Arr(x), Val::Arr(y)) => {
                let iw = x.windows(y.len()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == **y).then_some(i))))
            }
            (Val::Arr(x), y) => {
                let ix = x.iter().enumerate();
                Ok(Box::new(ix.filter_map(move |(i, x)| (x == y).then_some(i))))
            }
            (x, y) => Err(Error::index(x.clone(), y.clone())),
        }
    }

    /// Return true if `value | .[key]` is defined.
    ///
    /// Fail on values that are neither binaries, arrays nor objects.
    fn has(&self, key: &Self) -> Result<bool, Error> {
        match (self, key) {
            (Self::Str(a, Tag::Bytes), Self::Num(Num::Int(i))) if *i >= 0 => {
                Ok((*i as usize) < a.len())
            }
            (Self::Arr(a), Self::Num(Num::Int(i))) if *i >= 0 => Ok((*i as usize) < a.len()),
            (a @ (Self::Str(_, Tag::Bytes) | Self::Arr(_)), Self::Num(Num::BigInt(i))) => {
                a.has(&Self::from(crate::bigint_to_int_saturated(i)))
            }
            (Self::Obj(o), k) => Ok(o.contains_key(k)),
            _ => Err(Error::index(self.clone(), key.clone())),
        }
    }

    /// `a` contains `b` iff either
    /// * the string `b` is a substring of `a`,
    /// * every element in the array `b` is contained in some element of the array `a`,
    /// * for every key-value pair `k, v` in `b`,
    ///   there is a key-value pair `k, v'` in `a` such that `v'` contains `v`, or
    /// * `a` equals `b`.
    fn contains(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l, tag), Self::Str(r, tag_)) if tag == tag_ => l.contains_str(r),
            (Self::Arr(l), Self::Arr(r)) => r.iter().all(|r| l.iter().any(|l| l.contains(r))),
            (Self::Obj(l), Self::Obj(r)) => r
                .iter()
                .all(|(k, r)| l.get(k).is_some_and(|l| l.contains(r))),
            _ => self == other,
        }
    }

    fn to_bytes(&self) -> Result<Bytes, Self> {
        match self {
            Val::Num(n) => n
                .as_isize()
                .and_then(|i| u8::try_from(i).ok())
                .map(|u| Bytes::from(Vec::from([u])))
                .ok_or_else(|| self.clone()),
            Val::Str(b, _) => Ok(b.clone()),
            Val::Arr(a) => {
                let mut buf = BytesMut::new();
                for x in a.iter() {
                    buf.put(Val::to_bytes(x)?);
                }
                Ok(buf.into())
            }
            _ => Err(self.clone()),
        }
    }

    fn as_bytes_owned(&self) -> Option<Bytes> {
        if let Self::Str(b, _) = self {
            Some(b.clone())
        } else {
            None
        }
    }

    fn as_utf8_bytes_owned(&self) -> Option<Bytes> {
        self.is_utf8_str().then(|| self.as_bytes_owned()).flatten()
    }

    fn try_as_bytes_owned(&self) -> Result<Bytes, Error> {
        self.as_bytes_owned()
            .ok_or_else(|| Error::typ(self.clone(), Type::Str.as_str()))
    }

    fn try_as_utf8_bytes_owned(&self) -> Result<Bytes, Error> {
        self.as_utf8_bytes_owned()
            .ok_or_else(|| Error::typ(self.clone(), Type::Str.as_str()))
    }
}

/// Box Once, Map Error.
fn bome<'a>(r: ValR) -> BoxIter<'a, ValX> {
    box_once(r.map_err(Exn::from))
}

/// Box Map, Map Error.
fn bmme<'a>(iter: BoxIter<'a, ValR>) -> BoxIter<'a, ValX> {
    Box::new(iter.map(|r| r.map_err(Exn::from)))
}

/// Functions of the standard library.
#[cfg(feature = "parse")]
pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Filter<Native<D>>> {
    base_funs().chain(parse_funs().into_vec().into_iter().map(run))
}

/// Minimal set of filters.
pub fn base_funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Filter<Native<D>>> {
    base().into_vec().into_iter().map(run)
}

fn base<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        ("tojson", v(0), |cv| {
            let mut buf = Vec::new();
            json::write(&mut buf, &cv.1).unwrap();
            box_once(Ok(Val::from_utf8_bytes(buf)))
        }),
        ("tobytes", v(0), |cv| {
            let pass = |b| Val::Str(b, Tag::Bytes);
            let fail = |v| Error::str(format_args!("cannot convert {v} to bytes"));
            bome(cv.1.to_bytes().map(pass).map_err(fail))
        }),
        ("torawstring", v(0), |cv| {
            box_once(Ok(match cv.1 {
                Val::Str(s, _) => Val::Str(s, Tag::Raw),
                v => Val::Str(v.to_string().into(), Tag::Raw),
            }))
        }),
        ("length", v(0), |cv| bome(cv.1.length())),
        ("contains", v(1), |cv| {
            unary(cv, |x, y| Ok(Val::from(x.contains(&y))))
        }),
        ("has", v(1), |cv| unary(cv, |v, k| v.has(&k).map(Val::from))),
        ("indices", v(1), |cv| {
            let to_int = |i: usize| Val::from(i as isize);
            unary(cv, move |x, v| {
                x.indices(&v).map(|idxs| idxs.map(to_int).collect())
            })
        }),
        ("bsearch", v(1), |cv| {
            let to_idx = |r: Result<_, _>| r.map_or_else(|i| -1 - i as isize, |i| i as isize);
            unary(cv, move |a, x| {
                a.as_arr().map(|a| Val::from(to_idx(a.binary_search(&x))))
            })
        }),
    ])
}

fn parse_fail(i: &impl fmt::Display, fmt: &str, e: impl fmt::Display) -> Error {
    Error::str(format_args!("cannot parse {i} as {fmt}: {e}"))
}

fn serialise_fail(i: &impl fmt::Display, fmt: &str, e: impl fmt::Display) -> Error {
    Error::str(format_args!("cannot serialise {i} as {fmt}: {e}"))
}

self_cell::self_cell!(
    struct BytesValRs {
        owner: Bytes,

        #[not_covariant]
        dependent: ValRs,
    }
);

impl Iterator for BytesValRs {
    type Item = ValR;
    fn next(&mut self) -> Option<Self::Item> {
        self.with_dependent_mut(|_owner, iter| iter.next())
    }
}

type ValRs<'a> = BoxIter<'a, ValR>;

fn parse_byte_str(b: Bytes, parse: impl FnOnce(&str) -> ValRs) -> ValRs<'static> {
    Box::new(BytesValRs::new(b, |b| {
        then(core::str::from_utf8(b).map_err(Error::str), parse)
    }))
}

fn parse_bytes(b: Bytes, parse: impl FnOnce(&[u8]) -> ValRs) -> ValRs<'static> {
    Box::new(BytesValRs::new(b, |b| parse(b)))
}

#[cfg(feature = "parse")]
fn parse_funs<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        ("fromjson", v(0), |cv| {
            bmme(then(cv.1.try_as_utf8_bytes_owned(), |s| {
                let fail = move |r: Result<_, _>| r.map_err(|e| parse_fail(&cv.1, "JSON", e));
                parse_bytes(s, |s| Box::new(json::parse_many(s).map(fail)))
            }))
        }),
        ("fromcbor", v(0), |cv| {
            bmme(then(cv.1.try_as_bytes_owned(), |s| {
                let fail = move |r: Result<_, _>| r.map_err(|e| parse_fail(&cv.1, "CBOR", e));
                parse_bytes(s, |s| Box::new(cbor::parse_many(s).map(fail)))
            }))
        }),
        ("fromyaml", v(0), |cv| {
            bmme(then(cv.1.try_as_utf8_bytes_owned(), |s| {
                let fail = move |r: Result<_, _>| r.map_err(|e| parse_fail(&cv.1, "YAML", e));
                parse_byte_str(s, |s| Box::new(yaml::parse_many(s).map(fail)))
            }))
        }),
        ("fromxml", v(0), |cv| {
            bmme(then(cv.1.try_as_utf8_bytes_owned(), |s| {
                let fail = move |r: Result<_, _>| r.map_err(|e| parse_fail(&cv.1, "XML", e));
                parse_byte_str(s, |s| Box::new(xml::parse_many(s).map(fail)))
            }))
        }),
        ("fromtoml", v(0), |cv| {
            let from_utf8 = |b| core::str::from_utf8(b).map_err(Error::str);
            let parse = |b| toml::parse(b).map_err(|e| parse_fail(&cv.1, "TOML", e));
            bome(cv.1.try_as_utf8_bytes().and_then(from_utf8).and_then(parse))
        }),
        ("tocbor", v(0), |cv| {
            let mut buf = Vec::new();
            cbor::write(&mut buf, &cv.1).unwrap();
            bome(Ok(Val::byte_str(buf)))
        }),
        ("toyaml", v(0), |cv| {
            let mut buf = Vec::new();
            yaml::write(&mut buf, &cv.1).unwrap();
            box_once(Ok(Val::from_utf8_bytes(buf)))
        }),
        ("totoml", v(0), |cv| {
            let ser = toml::serialise(&cv.1).map_err(|e| serialise_fail(&cv.1, "TOML", e));
            bome(ser.map(|ser| Val::utf8_str(ser.to_string())))
        }),
        ("toxml", v(0), |cv| {
            let ser = xml::serialise(&cv.1).map_err(|e| serialise_fail(&cv.1, "XML", e));
            bome(ser.map(|ser| Val::utf8_str(ser.to_string())))
        }),
    ])
}
