use crate::funs::bome;
use crate::{cbor, json, toml, xml, yaml};
use crate::{Error, Val, ValR, ValX};
use alloc::{boxed::Box, string::ToString, vec::Vec};
use bytes::Bytes;
use core::fmt;
use jaq_core::box_iter::{box_once, then, BoxIter};
use jaq_core::{DataT, Exn, RunPtr};
use jaq_std::{v, Filter, ValT as _};

impl Val {
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
            .ok_or_else(|| Error::typ(self.clone(), "string"))
    }

    fn try_as_utf8_bytes_owned(&self) -> Result<Bytes, Error> {
        self.as_utf8_bytes_owned()
            .ok_or_else(|| Error::typ(self.clone(), "string"))
    }
}

/// Box Map, Map Error.
fn bmme<'a>(iter: BoxIter<'a, ValR>) -> BoxIter<'a, ValX> {
    Box::new(iter.map(|r| r.map_err(Exn::from)))
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

pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
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
            box_once(Ok(Val::utf8_str(buf)))
        }),
        ("totoml", v(0), |cv| {
            let ser = toml::serialise(&cv.1).map_err(|e| serialise_fail(&cv.1, "TOML", e));
            bome(ser.map(|ser| Val::utf8_str(ser.to_string())))
        }),
        ("toxml", v(0), |cv| {
            let fail = |e| serialise_fail(&cv.1, "XML", e);
            bome(xml::XmlVal::try_from(&cv.1).map_err(fail).map(|v| {
                let mut buf = Vec::new();
                v.write(&mut buf).unwrap();
                Val::utf8_str(buf)
            }))
        }),
    ])
}
