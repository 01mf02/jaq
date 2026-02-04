//! Compile & run jq filters and read & write data.
//!
//! This crate provides an opinionated, high-level API to
//! integrate jaq into applications.
//! It combines the functionality of several crates, which it re-exports:
//!
//! - `jaq-core`: basic compilation/execution of jq filters
//! - `jaq-std`:  standard library
//! - `jaq-json`: type of values
//! - `jaq-fmts`: multi-format support
//!
//! In contrast to these crates, which aim for
//! minimal functionality and maximal flexibility, this crate aims for
//! maximal functionality and minimal flexibility.
//! This makes this crate easier to use,
//! at the price of not covering all use cases.
//! You could say that this crate is the "instant food" of jaq crates:
//! it is easy to use, but difficult to customise.
//!
//! This crate is expected to change more frequently.
//! However, because of its small amount of exposed functions,
//! it may be easier to stay up-to-date with it than with its dependencies.
//!
//! # Usage
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
//!    Every jq filter needs to be supplied with input.
//!    This may be
//!    read from an input stream ([`std::io::Read`]) or
//!    parsed from a [`&str`]/bytes.
//!    You can do this with the functions in the [`fmts::read`] module, such as
//!    [`fmts::read::json::read_many`] or
//!    [`fmts::read::json::parse_many`].
//! 3. Run the filter.
//!    You can do this with [`data::run`].
//! 4. Write output data (optional).
//!    During or after filter execution, you may want to print output values.
//!    You can do this with the functions in the [`fmts::write`] module, such as
//!    [`fmts::write::write`].
//!
//! The `main` example in this crate shows you how to do all of this.
//! You can run it with `cargo run --example main`.
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

pub mod data;
pub mod load;

pub use jaq_core;
pub use jaq_fmts as fmts;
pub use jaq_json as json;
pub use jaq_std;

use jaq_core::load::{import, parse::Def, Arena, File, Loader};
use jaq_core::{compile::Compiler, native::Fun, DataT, Filter};
use load::{compile_errors, load_errors, FileReports};

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

/// Definitions from `jaq_core`, `jaq_std` and `jaq_json`.
pub fn defs() -> impl Iterator<Item = Def> {
    jaq_core::defs()
        .chain(jaq_std::defs())
        .chain(jaq_json::defs())
}
