//! Read values in different formats.
#[cfg(feature = "cbor")]
pub mod cbor;
#[cfg(feature = "toml")]
pub mod toml;
#[cfg(feature = "xml")]
pub mod xml;
#[cfg(feature = "yaml")]
pub mod yaml;
pub use jaq_json::read as json;

#[cfg(feature = "formats")]
mod formats;
#[cfg(feature = "formats")]
mod funs;
#[cfg(feature = "formats")]
pub use formats::*;
#[cfg(feature = "formats")]
pub use funs::funs;

use bytes::Bytes;
use jaq_json::Val;
use std::{path::Path};

type Result<T, E = std::io::Error> = core::result::Result<T, E>;

/// Try to load file by memory mapping and fall back to regular loading if it fails.
pub fn load_file(path: impl AsRef<Path>) -> Result<Bytes> {
    let file = std::fs::File::open(path.as_ref())?;
    Ok(match unsafe { memmap2::Mmap::map(&file) } {
        Ok(mmap) => Bytes::from_owner(mmap),
        Err(_) => Bytes::from(std::fs::read(path)?),
    })
}

/// Read JSON values in a file to an array.
pub fn json_array(path: impl AsRef<Path>) -> Result<Val> {
    json::parse_many(&load_file(path.as_ref())?)
        .map(crate::map_invalid_data)
        .collect()
}

/// Collect iterator into a single array if `slurp`, else return iterator.
pub fn collect_if<'a, T: FromIterator<T> + 'a, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> {
    if slurp {
        Box::new(core::iter::once(iter.collect()))
    } else {
        Box::new(iter)
    }
}
