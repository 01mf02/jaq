//! Extra/External/Extended jaq.
//!
//! This crate aims to simplify embedding jaq into other applications.
//! To this end, it provides functionality to:
//!
//! - compile and run filters
//! - read and write data
//!
//! In contrast to its dependencies
//! `jaq-core`, `jaq-std`, and `jaq-json`, which aim for
//! minimal functionality and maximal flexibility, this crate aims for
//! maximal functionality and minimal flexibility.
//! This makes this crate easier to use, yet not cover all use cases.
//!
//! This crate is expected to change more frequently.
//! However, because of its small amount of exposed functions,
//! it may be easier to stay up-to-date with it than with its dependencies.
//!
//! Embedding jaq into your own application involves three steps:
//!
//! 1. Compile a filter.
//!    This takes a jq filter as string and parses & compiles it to
//!    a filter ready to be executed by jaq.
//!    You can do this with
//!    [`data::compile`], or with
//!    [`compile_with`] if you need more flexibility.
//!    If this fails, e.g. due to parse errors,
//!    you can use [`load::FileReportsDisp`] to pretty-print errors.
//! 2. Read input data.
//!    Every jq filter needs to be supplied with input, which may be
//!    read from an input stream ([`std::io::Read`]) or
//!    parsed from a [`&str`]/bytes.
//!    You can do this with the functions in the [`read`] module, such as
//!    [`read::json::read_many`] or
//!    [`read::json::parse_many`].
//! 3. Run the filter.
//!    You can do this with [`data::run`].
//! 4. Write output data.
//!    During or after filter execution, you may want to print output values.
//!    You can do this with the functions in the [`mod@write`] module, such as
//!    [`write::write`].
//!
//! The `main` example in this crate shows you how to do all of this.
//! You can run it with `cargo run --example main`.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod data;
pub mod load;
pub use jaq_fmts::{read, write, Format};

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
