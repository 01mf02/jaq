//! Writing output.
use crate::{invalid_data, Cli, Format};
use jaq_json::write::{Colors, Pp};
use jaq_json::{cbor, toml, xml, yaml, Val};
use std::io::{self, IsTerminal, Write};

type Result<T = (), E = io::Error> = core::result::Result<T, E>;

fn map_err_to_string<T, E: core::fmt::Display>(r: Result<T, E>) -> Result<T> {
    r.map_err(|e| invalid_data(e.to_string()))
}

struct Writer {
    format: Format,
    pp: Pp,
    join: bool,
}

impl Writer {
    fn new(cli: &Cli) -> Self {
        let pp = Pp {
            indent: (!cli.compact_output).then(|| cli.indent()),
            sort_keys: cli.sort_keys,
            colors: if cli.color_stdout() {
                Colors::ansi()
            } else {
                Colors::default()
            },
        };

        let format = cli.to.unwrap_or(Format::Json);
        let join = cli.join_output;

        Self { pp, format, join }
    }
}

pub fn print(w: &mut dyn Write, cli: &Cli, val: &Val) -> Result {
    let Writer { format, pp, join } = &Writer::new(cli);

    if matches!(format, Format::Yaml) {
        // start of YAML document
        writeln!(w, "---")?;
    }

    match (val, format) {
        (Val::Str(b, _), Format::Raw) => w.write_all(b)?,
        (_, Format::Cbor) => cbor::write(w, val)?,
        (_, Format::Json | Format::Raw) => jaq_json::write::write(w, pp, 0, val)?,
        (_, Format::Yaml) => yaml::write(w, pp, 0, val)?,
        (_, Format::Toml) => write!(w, "{}", map_err_to_string(toml::Toml::try_from(val))?)?,
        (_, Format::Xml) => map_err_to_string(xml::Xml::try_from(val))?.write(w)?,
    };

    if *join || matches!(format, Format::Cbor) {
        // when running `jaq -jn '"prompt> " | (., input)'`,
        // this flush is necessary to make "prompt> " appear first
        w.flush()
    } else {
        // this also flushes output, because stdout is line-buffered in Rust
        writeln!(w)
    }?;

    if matches!(format, Format::Yaml) {
        // end of YAML document
        writeln!(w, "...")?;
    }
    Ok(())
}

pub fn with_stdout<T>(f: impl FnOnce(&mut dyn Write) -> T) -> T {
    let stdout = io::stdout();
    if stdout.is_terminal() {
        f(&mut stdout.lock())
    } else {
        f(&mut io::BufWriter::new(stdout.lock()))
    }
}
