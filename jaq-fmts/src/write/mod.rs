//! Write values in different formats.
#[cfg(feature = "cbor")]
pub mod cbor;
#[cfg(feature = "toml")]
pub mod toml;
#[cfg(feature = "xml")]
pub mod xml;
#[cfg(feature = "yaml")]
pub mod yaml;
pub use jaq_json::write as json;

#[cfg(feature = "all")]
mod formats;
#[cfg(feature = "all")]
mod funs;
#[cfg(feature = "all")]
pub use formats::write;
#[cfg(feature = "all")]
pub use funs::funs;

use std::io::{self, IsTerminal, Write};

/// Write options.
#[derive(Default)]
pub struct Writer {
    /// output format
    pub format: crate::Format,
    /// pretty printer
    pub pp: json::Pp,
    /// concatenate outputs without newline
    pub join: bool,
}

/// Buffer writes if stdout is terminal, else just lock stdout.
pub fn with_stdout<T>(f: impl FnOnce(&mut dyn Write) -> T) -> T {
    let stdout = io::stdout();
    if stdout.is_terminal() {
        f(&mut stdout.lock())
    } else {
        f(&mut io::BufWriter::new(stdout.lock()))
    }
}
