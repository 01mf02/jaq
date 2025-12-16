//! Extra/External/Extended jaq.
//!
//! This crate contains all kind of functions and data types that
//! might be useful to integrate jaq into a real-world application.
//! It also contains support for various additional data formats.
//!
//! This crate is more opinionated than its dependencies
//! `jaq-core`, `jaq-std`, and `jaq-json` and
//! is expected to change more frequently.
//! However, because of its small amount of exposed functions,
//! it may be easier to stay up-to-date with it than with its dependencies.
//!
//! Embedding jaq into your own application involves three steps:
//!
//! 1. Compile a filter.
//!    You can do this conveniently with
//!    [`data::compile`], or with
//!    [`compile_with`] if you need more flexibility.
//! 2. Read input data.
//!    You can do this with the functions in the [`read`] module, such as
//!    [`read::json::read_many`] or
//!    [`read::json::parse_many`].
//! 3. Run the filter.
//!    You can do this with [`data::run`].
//!    Inside this function, you may also want to print output values, for which
//!    you can use functions in the [`mod@write`] module, such as
//!    [`write::write`].
//!
//! The `main` example in this crate shows you how to do all of this.
//! You can run it with `cargo run --example main`.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod data;
pub mod load;
pub mod read;
pub mod write;

use jaq_core::load::{import, parse::Def, Arena, File, Loader};
use jaq_core::{compile::Compiler, DataT, Filter};
use load::{compile_errors, load_errors, FileReports};

type Fun<D> = jaq_std::Filter<jaq_core::Native<D>>;

/// Compile a filter without access to external files.
///
/// A simplified version of this function is [`data::compile`].
pub fn compile_with<D: DataT>(
    code: &str,
    defs: impl Iterator<Item = Def>,
    funs: impl Iterator<Item = Fun<D>>,
    vars: &[String],
) -> Result<Filter<D>, Vec<FileReports>> {
    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let loader = Loader::new(defs);
    let modules = loader
        .load(&arena, File { path: (), code })
        .map_err(load_errors)?;

    import(&modules, |_path| Err("file loading not supported".into())).map_err(load_errors)?;

    Compiler::default()
        .with_funs(funs)
        .with_global_vars(vars.iter().map(|v| &**v))
        .compile(modules)
        .map_err(compile_errors)
}

#[cfg(feature = "formats")]
/// (De-)Serialisation filters, such as `fromyaml`, `toxml`.
pub fn rw_funs<D: for<'a> DataT<V<'a> = jaq_json::Val>>() -> impl Iterator<Item = Fun<D>> {
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
