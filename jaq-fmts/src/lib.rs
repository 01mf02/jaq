//! Multi-format support for jaq.
//!
//! This crate contains functions to load and write various data formats,
//! such as JSON, CBOR, YAML, TOML, XML.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod read;
pub mod write;

#[cfg(feature = "all")]
/// (De-)Serialisation filters, such as `fromyaml`, `toxml`.
pub fn funs<D: for<'a> jaq_core::DataT<V<'a> = jaq_json::Val>>(
) -> impl Iterator<Item = jaq_std::Filter<jaq_core::Native<D>>> {
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

impl Format {
    /// List of all currently supported formats.
    pub const ALL: &str = "raw, json, cbor, yaml, toml, xml";

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
