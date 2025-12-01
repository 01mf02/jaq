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

mod funs;
pub use funs::funs;

use crate::data::Writer;
use crate::{invalid_data, Format};
use jaq_json::Val;
use std::io::{self, IsTerminal, Write};

type Result<T = (), E = io::Error> = core::result::Result<T, E>;

fn map_err_to_string<T, E: core::fmt::Display>(r: Result<T, E>) -> Result<T> {
    r.map_err(|e| invalid_data(e.to_string()))
}

/// Write value.
pub fn write(w: &mut dyn Write, writer: &Writer, val: &Val) -> Result {
    let Writer { format, pp, join } = writer;

    let yaml_doc = !join && matches!(format, Format::Yaml);
    if yaml_doc {
        // start of YAML document
        writeln!(w, "---")?
    }

    match (val, format) {
        (Val::Str(b, _), Format::Raw) => w.write_all(b)?,
        (_, Format::Cbor) => cbor::write(w, val)?,
        (_, Format::Json | Format::Raw) => jaq_json::write::write(w, pp, 0, val)?,
        (_, Format::Yaml) => yaml::write(w, pp, 0, val)?,
        (_, Format::Toml) => write!(w, "{}", map_err_to_string(toml::Toml::try_from(val))?)?,
        (_, Format::Xml) => map_err_to_string(xml::Xml::try_from(val))?.write(w)?,
    };

    if match format {
        Format::Cbor => false,
        Format::Yaml => true,
        _ => !join,
    } {
        // this flushes output, because stdout is line-buffered in Rust
        writeln!(w)?
    };

    // when running `jaq -jn '"prompt> " | (., input)'`,
    // this flush is necessary to make "prompt> " appear first
    w.flush()?;

    if yaml_doc {
        // end of YAML document
        writeln!(w, "...")?
    }
    Ok(())
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
