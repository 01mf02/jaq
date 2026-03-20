use super::{cbor, tabular, toml, xml, yaml};
use alloc::{boxed::Box, string::ToString, vec::Vec};
use core::fmt;
use jaq_core::box_iter::box_once;
use jaq_core::native::{bome, v, Filter};
use jaq_core::{DataT, RunPtr};
use jaq_json::{write::Pp, Error, Val};

/// Convert a value fallibly and write the result.
macro_rules! ser {
    ($cv: ident, $fmt: expr, $ty: ty, $f: ident) => {{
        let fail = |e| serialise_fail(&$cv.1, $fmt, e);
        bome(<$ty>::try_from(&$cv.1).map_err(fail).map(|v| {
            let mut buf = Vec::new();
            v.$f(&mut buf).unwrap();
            Val::utf8_str(buf)
        }))
    }};
}

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
        ("toxml", v(0), |cv| ser!(cv, "XML", xml::Xml<&[u8]>, write)),
        ("tocsv", v(0), |cv| ser!(cv, "CSV", tabular::Row, write_csv)),
        ("totsv", v(0), |cv| ser!(cv, "TSV", tabular::Row, write_tsv)),
        ("@csv", v(0), |cv| ser!(cv, "CSV", tabular::Row, write_csv)),
        ("@tsv", v(0), |cv| ser!(cv, "TSV", tabular::Row, write_tsv)),
    ])
}

fn serialise_fail(i: &impl fmt::Display, fmt: &str, e: impl fmt::Display) -> Error {
    Error::str(format_args!("cannot serialise {i} as {fmt}: {e}"))
}
