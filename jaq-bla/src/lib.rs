//! jaq utilities.
//!
//! This crate contains all kind of functions and data types that
//! might be useful to integrate jaq into a real-world application.
#![warn(missing_docs)]

pub mod data;
pub mod load;
pub mod read;
pub mod write;

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
