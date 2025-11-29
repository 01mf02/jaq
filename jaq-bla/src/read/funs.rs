use super::{cbor, toml, xml, yaml};
use alloc::boxed::Box;
use bytes::Bytes;
use core::fmt;
use jaq_core::box_iter::{then, BoxIter};
use jaq_core::{DataT, Exn, RunPtr};
use jaq_json::parse_bytes;
use jaq_json::{Error, Val, ValR, ValX};
use jaq_std::{bome, v, Filter, ValT as _};

pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
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
    ])
}

/// Box Map, Map Error.
fn bmme<'a>(iter: BoxIter<'a, ValR>) -> BoxIter<'a, ValX> {
    Box::new(iter.map(|r| r.map_err(Exn::from)))
}

fn parse_fail(i: &impl fmt::Display, fmt: &str, e: impl fmt::Display) -> Error {
    Error::str(format_args!("cannot parse {i} as {fmt}: {e}"))
}

type ValRs<'a> = BoxIter<'a, ValR>;

fn parse_byte_str(b: Bytes, parse: impl FnOnce(&str) -> ValRs) -> ValRs<'static> {
    parse_bytes(b, |b| {
        then(core::str::from_utf8(b).map_err(Error::str), parse)
    })
}
