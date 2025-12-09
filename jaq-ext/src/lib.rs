//! Extra/External/Extended jaq.
//!
//! This crate contains all kind of functions and data types that
//! might be useful to integrate jaq into a real-world application.
//! It also contains support for various additional data formats.
//!
//! This crate is more opinionated than its dependencies
//! [`jaq-core`], [`jaq-std`], and [`jaq-json`] and
//! is expected to change more frequently.
//! However, because of its small amount of exposed functions,
//! it may be easier to stay up-to-date with it than with its dependencies.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod data;
pub mod load;
pub mod read;
pub mod write;

use data::DataKind;
use jaq_core::Native;
use jaq_std::{input, Filter};

type Fun<D = DataKind> = Filter<Native<D>>;

/// Functions from [`jaq_std`] and [`jaq_json`].
pub fn base_funs() -> impl Iterator<Item = Fun> {
    let run = jaq_std::run::<DataKind>;
    let std = jaq_std::funs::<DataKind>();
    let input = input::funs::<DataKind>().into_vec().into_iter().map(run);
    std.chain(jaq_json::funs()).chain(input)
}

#[cfg(feature = "formats")]
/// (De-)Serialisation filters.
pub fn rw_funs<D: for<'a> jaq_core::DataT<V<'a> = jaq_json::Val>>() -> impl Iterator<Item = Fun<D>>
{
    [read::funs::<D>(), write::funs::<D>()]
        .into_iter()
        .flat_map(move |funs| funs.into_vec().into_iter().map(jaq_std::run::<D>))
}

/// Input/Output format.
#[derive(Copy, Clone, Debug, Default)]
pub enum Format {
    /// Raw text string
    ///
    /// When the option `--slurp` is used additionally,
    /// then the whole input is read into a single string.
    Raw,
    /// JavaScript Object Notation
    #[default]
    Json,
    /// Concise Binary Object Representation
    Cbor,
    /// Tom's Obvious, Minimal Language
    Toml,
    /// Extensible Markup Language
    Xml,
    /// YAML Ain't Markup Language™
    Yaml,
}

/// List of all currently supported formats.
pub const FMTS: &str = "raw, json, cbor, yaml, toml, xml";

impl Format {
    /// Determine a file format from a path.
    pub fn determine(path: &std::path::Path) -> Option<Self> {
        match path.extension()?.to_str()? {
            "cbor" => Some(Format::Cbor),
            "toml" => Some(Format::Toml),
            "xml" | "xhtml" => Some(Format::Xml),
            "yml" | "yaml" => Some(Format::Yaml),
            "json" => Some(Format::Json),
            _ => None,
        }
    }

    /// Parse a format name.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "cbor" => Some(Format::Cbor),
            "raw" => Some(Format::Raw),
            "json" => Some(Format::Json),
            "toml" => Some(Format::Toml),
            "xml" => Some(Format::Xml),
            "yaml" => Some(Format::Yaml),
            _ => None,
        }
    }
}

/// Dynamic & thread-safe [`std::error::Error`].
type BoxError = Box<dyn std::error::Error + Send + Sync>;

/// Create an invalid data I/O error.
fn invalid_data(e: impl Into<BoxError>) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, e)
}

fn map_invalid_data<T>(r: Result<T, impl Into<BoxError>>) -> std::io::Result<T> {
    r.map_err(invalid_data)
}
