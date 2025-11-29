//! jaq utilities.
//!
//! This crate contains all kind of functions and data types that
//! might be useful to integrate jaq into a real-world application.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod data;
pub mod load;
pub mod read;
pub mod write;

use data::Filter;
use jaq_core::{DataT, Native};
use jaq_json::Val;
use load::{compile_errors, load_errors, FileReports};

/// Compile a filter without access to external files.
pub fn compile<P: Clone + Default + Eq>(
    code: &str,
    vars: &[String],
) -> Result<Filter, Vec<FileReports<P>>> {
    use jaq_core::compile::Compiler;
    use jaq_core::load::{import, Arena, File, Loader};

    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let loader = Loader::new(jaq_std::defs().chain(jaq_json::defs()));
    let path = P::default();
    let modules = loader
        .load(&arena, File { path, code })
        .map_err(load_errors)?;

    import(&modules, |_path| Err("file loading not supported".into())).map_err(load_errors)?;

    let compiler = Compiler::default()
        .with_funs(data::funs())
        .with_global_vars(vars.iter().map(|v| &**v));
    let filter = compiler.compile(modules).map_err(compile_errors)?;
    Ok(filter)
}

pub fn rw_funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = jaq_std::Filter<Native<D>>>
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
