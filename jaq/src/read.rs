use crate::{invalid_data, BoxError, Format};
use bytes::Bytes;
use jaq_core::box_iter::{box_once, BoxIter};
use jaq_json::{cbor, json, toml, xml, yaml, Tag, Val};
use std::io::{self, Read};
use std::path::Path;

type Vals<'a> = BoxIter<'a, io::Result<Val>>;

/// Try to load file by memory mapping and fall back to regular loading if it fails.
pub fn load_file(path: impl AsRef<Path>) -> io::Result<Bytes> {
    let file = std::fs::File::open(path.as_ref())?;
    Ok(match unsafe { memmap2::Mmap::map(&file) } {
        Ok(mmap) => Bytes::from_owner(mmap),
        Err(_) => Bytes::from(std::fs::read(path)?),
    })
}

pub fn json_array(path: impl AsRef<Path>) -> io::Result<Val> {
    json::parse_many(&load_file(path.as_ref())?)
        .map(map_invalid_data)
        .collect()
}

/// Load standard input to string for certain formats.
///
/// This has to be synchronised with [`from_stdin`].
pub fn stdin_string(fmt: Format) -> io::Result<String> {
    use Format::*;
    Ok(match fmt {
        Raw | Json | Cbor => String::new(),
        Toml | Xml | Yaml => io::read_to_string(io::stdin().lock())?,
    })
}

/// Convert bytes to string for certain formats.
///
/// This has to be synchronised with [`from_file`].
pub fn file_str(fmt: Format, bytes: &[u8]) -> io::Result<&str> {
    use Format::*;
    Ok(match fmt {
        Raw | Json | Cbor => "",
        Toml | Xml | Yaml => core::str::from_utf8(bytes).map_err(invalid_data)?,
    })
}

pub fn from_stdin(fmt: Format, s: &str, slurp: bool) -> Vals<'_> {
    let stdin = || io::stdin().lock();
    use bstr::io::BufReadExt;
    match fmt {
        Format::Raw if slurp => {
            let mut buf = Vec::new();
            let result = stdin().read_to_end(&mut buf);
            box_once(result.map(|_| Val::utf8_str(buf)))
        }
        Format::Raw => Box::new(stdin().byte_lines().map(|r| r.map(Val::utf8_str))),
        Format::Cbor => collect_if(slurp, cbor::read_many(stdin())),
        Format::Json => collect_if(slurp, json::read_many(stdin())),
        Format::Toml => box_once(toml::parse(s).map_err(invalid_data)),
        Format::Xml => collect_if(slurp, xml::parse_many(s).map(map_invalid_data)),
        Format::Yaml => collect_if(slurp, yaml::parse_many(s).map(map_invalid_data)),
    }
}

pub fn from_file<'a>(fmt: Format, bytes: &'a Bytes, s: &'a str, slurp: bool) -> Vals<'a> {
    use bstr::ByteSlice;
    match fmt {
        Format::Raw if slurp => box_once(Ok(Val::Str(bytes.clone(), Tag::Utf8))),
        Format::Raw => Box::new(
            ByteSlice::lines(&**bytes).map(|line| Ok(Val::Str(bytes.slice_ref(line), Tag::Utf8))),
        ),
        Format::Json => collect_if(slurp, json::parse_many(bytes).map(map_invalid_data)),
        Format::Cbor => collect_if(slurp, cbor::parse_many(bytes).map(map_invalid_data)),
        Format::Toml | Format::Xml | Format::Yaml => from_stdin(fmt, s, slurp),
    }
}

fn collect_if<'a, T: FromIterator<T> + 'a, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
) -> BoxIter<'a, Result<T, E>> {
    if slurp {
        Box::new(core::iter::once(iter.collect()))
    } else {
        Box::new(iter)
    }
}

fn map_invalid_data<T>(r: Result<T, impl Into<BoxError>>) -> io::Result<T> {
    r.map_err(invalid_data)
}
