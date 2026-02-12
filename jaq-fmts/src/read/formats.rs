use super::{cbor, collect_if, json, toml, xml, yaml, Result};
use crate::{invalid_data, map_invalid_data, Format};
use bytes::Bytes;
use jaq_core::box_iter::{box_once, BoxIter};
use jaq_json::Val;
use std::io::{self, Read};

type Vals<'a> = BoxIter<'a, io::Result<Val>>;

/// Read input to string for certain formats.
///
/// This has to be synchronised with [`read`].
pub fn read_string(fmt: Format, read: impl Read) -> Result<String> {
    use Format::*;
    match fmt {
        Raw | Raw0 | Json | Cbor => Ok(String::new()),
        Toml | Xml | Yaml => io::read_to_string(read),
    }
}

/// Convert bytes to string for certain formats.
///
/// This has to be synchronised with [`parse`].
pub fn bytes_str(fmt: Format, bytes: &[u8]) -> Result<&str> {
    use Format::*;
    Ok(match fmt {
        Raw | Raw0 | Json | Cbor => "",
        Toml | Xml | Yaml => core::str::from_utf8(bytes).map_err(invalid_data)?,
    })
}

/// Read values from [`io::BufRead`] or [`&str`], depending on format.
pub fn read<'a>(fmt: Format, read: impl io::BufRead + 'a, s: &'a str, slurp: bool) -> Vals<'a> {
    use bstr::io::BufReadExt;
    let mut read = read;
    match fmt {
        Format::Raw if slurp => {
            let mut buf = Vec::new();
            let result = read.read_to_end(&mut buf);
            box_once(result.map(|_| Val::utf8_str(buf)))
        }
        Format::Raw => Box::new(read.byte_lines().map(|r| r.map(Val::utf8_str))),
        Format::Raw0 => collect_if(slurp, read.byte_records(0).map(|r| r.map(Val::utf8_str))),
        Format::Cbor => collect_if(slurp, cbor::read_many(read)),
        Format::Json => collect_if(slurp, json::read_many(read)),
        Format::Toml => box_once(toml::parse(s).map_err(invalid_data)),
        Format::Xml => collect_if(slurp, xml::parse_many(s).map(map_invalid_data)),
        Format::Yaml => collect_if(slurp, yaml::parse_many(s).map(map_invalid_data)),
    }
}

/// Parse values from [`Bytes`] or [`&str`], depending on format.
pub fn parse<'a>(fmt: Format, bytes: &'a Bytes, s: &'a str, slurp: bool) -> Vals<'a> {
    use bstr::ByteSlice;
    let nul_sep = |s: &'a [u8]| s.strip_suffix(b"\0").unwrap_or(bytes).split_str("\0");
    let slice_to_str = |s| Ok(Val::utf8_str(bytes.slice_ref(s)));
    match fmt {
        Format::Raw if slurp => box_once(Ok(Val::utf8_str(bytes.clone()))),
        Format::Raw => Box::new(bytes.lines().map(slice_to_str)),
        Format::Raw0 => collect_if(slurp, nul_sep(bytes).map(slice_to_str)),
        Format::Json => collect_if(slurp, json::parse_many(bytes).map(map_invalid_data)),
        Format::Cbor => collect_if(slurp, cbor::parse_many(bytes).map(map_invalid_data)),
        Format::Toml | Format::Xml | Format::Yaml => read(fmt, &[][..], s, slurp),
    }
}
