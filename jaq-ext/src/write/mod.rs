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

#[cfg(feature = "formats")]
mod formats;
#[cfg(feature = "formats")]
mod funs;
#[cfg(feature = "formats")]
pub use formats::write;
#[cfg(feature = "formats")]
pub use funs::funs;

use std::io::{self, IsTerminal, Write};

/// Buffer writes if stdout is terminal, else just lock stdout.
pub fn with_stdout<T>(f: impl FnOnce(&mut dyn Write) -> T) -> T {
    let stdout = io::stdout();
    if stdout.is_terminal() {
        f(&mut stdout.lock())
    } else {
        f(&mut io::BufWriter::new(stdout.lock()))
    }
}
