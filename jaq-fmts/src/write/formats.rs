use super::*;
use crate::{invalid_data, Format};
use jaq_json::Val;
use std::io::{self, Write};

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
        (Val::BStr(b) | Val::TStr(b), Format::Raw0) if b.contains(&b'\0') => {
            let nul_err = "cannot dump a string containing NUL with `--to raw0` or `--raw-output0`";
            return Err(io::Error::new(io::ErrorKind::InvalidData, nul_err));
        }
        (Val::BStr(b) | Val::TStr(b), Format::Raw | Format::Raw0) => w.write_all(b)?,
        (_, Format::Cbor) => cbor::write(w, val)?,
        (_, Format::Json | Format::Raw | Format::Raw0) => jaq_json::write::write(w, pp, 0, val)?,
        (_, Format::Yaml) => yaml::write(w, pp, 0, val)?,
        (_, Format::Toml) => write!(w, "{}", map_err_to_string(toml::Root::try_from(val))?)?,
        (_, Format::Xml) => map_err_to_string(xml::Xml::try_from(val))?.write(w)?,
    };

    w.write_all(match format {
        Format::Cbor | Format::Toml => b"",
        Format::Raw0 => b"\0",
        Format::Yaml => b"\n",
        _ if *join => b"",
        _ => b"\n",
    })?;

    if yaml_doc {
        // end of YAML document
        writeln!(w, "...")?;
    }

    // when running `jaq -jn '"prompt> " | (., input)'`,
    // this flush is necessary to make "prompt> " appear first
    w.flush()
}
