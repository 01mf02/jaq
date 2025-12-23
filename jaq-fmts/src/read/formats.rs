use super::{cbor, collect_if, json, toml, xml, yaml, Result};
use crate::{invalid_data, map_invalid_data, Format};
use bytes::Bytes;
use jaq_core::box_iter::{box_once, BoxIter};
use jaq_json::{Tag, Val};
use std::io::{self, BufRead, Read};

type Vals<'a> = BoxIter<'a, io::Result<Val>>;

/// Read input to string for certain formats.
///
/// This has to be synchronised with [`from_bufread`].
pub fn read_string(fmt: Format, read: impl Read) -> Result<String> {
    use Format::*;
    match fmt {
        Raw | Json | Cbor => Ok(String::new()),
        Toml | Xml | Yaml => io::read_to_string(read),
    }
}

/// Convert bytes to string for certain formats.
///
/// This has to be synchronised with [`from_bytes`].
pub fn bytes_str(fmt: Format, bytes: &[u8]) -> Result<&str> {
    use Format::*;
    Ok(match fmt {
        Raw | Json | Cbor => "",
        Toml | Xml | Yaml => core::str::from_utf8(bytes).map_err(invalid_data)?,
    })
}

/// Read value from [`BufRead`] or [`&str`], depending on format.
pub fn from_bufread<'a>(fmt: Format, read: impl BufRead + 'a, s: &'a str, slurp: bool) -> Vals<'a> {
    use bstr::io::BufReadExt;
    let mut read = read;
    match fmt {
        Format::Raw if slurp => {
            let mut buf = Vec::new();
            let result = read.read_to_end(&mut buf);
            box_once(result.map(|_| Val::utf8_str(buf)))
        }
        Format::Raw => Box::new(read.byte_lines().map(|r| r.map(Val::utf8_str))),
        Format::Cbor => collect_if(slurp, cbor::read_many(read)),
        Format::Json => collect_if(slurp, json::read_many(read)),
        Format::Toml => box_once(toml::parse(s).map_err(invalid_data)),
        Format::Xml => collect_if(slurp, xml::parse_many(s).map(map_invalid_data)),
        Format::Yaml => collect_if(slurp, yaml::parse_many(s).map(map_invalid_data)),
    }
}

/// Parse value from file or `s`, depending on format.
pub fn from_bytes<'a>(fmt: Format, bytes: &'a Bytes, s: &'a str, slurp: bool) -> Vals<'a> {
    use bstr::ByteSlice;
    match fmt {
        Format::Raw if slurp => box_once(Ok(Val::Str(bytes.clone(), Tag::Utf8))),
        Format::Raw => Box::new(
            ByteSlice::lines(&**bytes).map(|line| Ok(Val::Str(bytes.slice_ref(line), Tag::Utf8))),
        ),
        Format::Json => collect_if(slurp, json::parse_many(bytes).map(map_invalid_data)),
        Format::Cbor => collect_if(slurp, cbor::parse_many(bytes).map(map_invalid_data)),
        Format::Toml | Format::Xml | Format::Yaml => from_bufread(fmt, &[][..], s, slurp),
    }
}
