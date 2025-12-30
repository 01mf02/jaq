use super::*;
use crate::{invalid_data, Format};
use jaq_json::Val;
use std::io::{self, Write};
use bstr::B;

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
        (Val::Str(b, _), Format::Raw0) if b.contains(&b'\0') => return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "cannot dump a string containing NUL with `--to raw0` or `--raw-output0`",
        )),
        (Val::Str(b, _), Format::Raw | Format::Raw0) => w.write_all(b)?,
        (_, Format::Cbor) => cbor::write(w, val)?,
        (_, Format::Json | Format::Raw | Format::Raw0) => jaq_json::write::write(w, pp, 0, val)?,
        (_, Format::Yaml) => yaml::write(w, pp, 0, val)?,
        (_, Format::Toml) => write!(w, "{}", map_err_to_string(toml::Toml::try_from(val))?)?,
        (_, Format::Xml) => map_err_to_string(xml::Xml::try_from(val))?.write(w)?,
    };

    if let Some(terminator) = match format {
        Format::Cbor => None,
        Format::Raw0 => Some(B("\0")),
        Format::Yaml if *join => Some(B("\n")),
        Format::Yaml => Some(B("\n...\n")),
        _ if !join => Some(B("\n")),
        _ => None,
    } {
        w.write_all(terminator)?;
    };

    // when running `jaq -jn '"prompt> " | (., input)'`,
    // this flush is necessary to make "prompt> " appear first
    w.flush()?;

    Ok(())
}
