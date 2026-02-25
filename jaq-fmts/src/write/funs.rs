use super::{cbor, toml, xml, yaml};
use alloc::{boxed::Box, string::ToString, vec::Vec};
use core::fmt;
use jaq_core::box_iter::box_once;
use jaq_core::native::{bome, v, Filter};
use jaq_core::{DataT, RunPtr};
use jaq_json::{write::Pp, Error, Val};

/// Serialisation filters.
pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        ("tocbor", v(0), |cv| {
            let mut buf = Vec::new();
            cbor::write(&mut buf, &cv.1).unwrap();
            bome(Ok(Val::byte_str(buf)))
        }),
        ("toyaml", v(0), |cv| {
            let mut buf = Vec::new();
            let pp = Pp {
                sep_space: true,
                ..Pp::default()
            };
            yaml::write(&mut buf, &pp, 0, &cv.1).unwrap();
            box_once(Ok(Val::utf8_str(buf)))
        }),
        ("totoml", v(0), |cv| {
            let ser = toml::Root::try_from(&cv.1).map_err(|e| serialise_fail(&cv.1, "TOML", e));
            bome(ser.map(|ser| Val::utf8_str(ser.to_string())))
        }),
        ("toxml", v(0), |cv| {
            let fail = |e| serialise_fail(&cv.1, "XML", e);
            bome(xml::Xml::try_from(&cv.1).map_err(fail).map(|v| {
                let mut buf = Vec::new();
                v.write(&mut buf).unwrap();
                Val::utf8_str(buf)
            }))
        }),
    ])
}

fn serialise_fail(i: &impl fmt::Display, fmt: &str, e: impl fmt::Display) -> Error {
    Error::str(format_args!("cannot serialise {i} as {fmt}: {e}"))
}
