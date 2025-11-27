pub mod data;
mod load;
pub mod read;
pub mod write;

use jaq_json::write::Pp;
pub use load::*;

#[derive(Default)]
pub struct Runner {
    pub null_input: bool,
    pub color_err: bool,
    pub writer: Writer,
}

#[derive(Default)]
pub struct Writer {
    pub format: Format,
    pub pp: Pp,
    pub join: bool,
}

impl Runner {
    pub fn color_stdout(&self) -> bool {
        !self.writer.pp.colors.reset.is_empty()
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub enum Format {
    /// When the option `--slurp` is used additionally,
    /// then the whole input is read into a single string.
    Raw,
    #[default]
    Json,
    Cbor,
    Toml,
    Xml,
    Yaml,
}

pub const FMTS: &str = "raw, json, cbor, toml, xml, yaml";

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
