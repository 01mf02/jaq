//! Writing output.
use crate::style::{Style, ANSI};
use crate::{invalid_data, Cli, Format};
use jaq_json::{cbor, toml, xml, yaml};
use jaq_json::{write_byte, write_bytes, write_utf8, Tag, Val};
use std::io::{self, IsTerminal, Write};

type Result<T = (), E = io::Error> = core::result::Result<T, E>;

/// Pretty printer.
struct Pp {
    compact: bool,
    indent: String,
    sort_keys: bool,
    style: Style,
}

impl Pp {
    fn indent(&self, w: &mut dyn Write, level: usize) -> Result {
        if !self.compact {
            write!(w, "{}", self.indent.repeat(level))?;
        }
        Ok(())
    }

    fn newline(&self, w: &mut dyn Write) -> Result {
        if !self.compact {
            writeln!(w)?;
        }
        Ok(())
    }

    fn write_seq<T, I, F>(&self, w: &mut dyn Write, level: usize, xs: I, f: F) -> Result
    where
        I: IntoIterator<Item = T>,
        F: Fn(&mut dyn Write, T) -> Result,
    {
        self.newline(w)?;
        let mut iter = xs.into_iter().peekable();
        while let Some(x) = iter.next() {
            self.indent(w, level + 1)?;
            f(w, x)?;
            if iter.peek().is_some() {
                write!(w, ",")?;
            }
            self.newline(w)?;
        }
        self.indent(w, level)
    }
}

type WriteFn = fn(&mut dyn Write, &Pp, usize, &Val) -> Result;

fn write_json(w: &mut dyn Write, pp: &Pp, level: usize, v: &Val) -> Result {
    write_rec(w, pp, level, v, write_json)
}

fn write_yaml(w: &mut dyn Write, pp: &Pp, level: usize, v: &Val) -> Result {
    let style = &pp.style;
    match v {
        Val::Str(_, Tag::Bytes) => style.write(w, style.green, |w| yaml::write(w, v)),
        // special handling for NaN & infinity
        Val::Num(_) => yaml::write(w, v),
        _ => write_rec(w, pp, level, v, write_yaml),
    }
}

fn write_rec(w: &mut dyn Write, pp: &Pp, level: usize, v: &Val, rec: WriteFn) -> Result {
    let style = &pp.style;
    let bold = |w: &mut dyn Write, c| style.write(w, style.bold, |w| write!(w, "{c}"));
    match v {
        Val::Null | Val::Bool(_) | Val::Num(_) => write!(w, "{v}"),
        Val::Str(s, Tag::Utf8) => style.write(w, style.green, |w| {
            write_utf8!(w, s, |part| w.write_all(part))
        }),
        Val::Str(b, Tag::Bytes) => style.write(w, style.red, |w| write_bytes!(w, b)),
        Val::Str(s, Tag::Raw) => w.write_all(s),
        Val::Arr(a) => {
            bold(w, '[')?;
            if !a.is_empty() {
                pp.write_seq(w, level, &**a, |w, x| rec(w, pp, level + 1, x))?;
            }
            bold(w, ']')
        }
        Val::Obj(o) => {
            bold(w, '{')?;
            let kv = |w: &mut dyn Write, (k, v)| {
                use jaq_std::ValT;
                rec(w, pp, level + 1, k)?;
                write!(w, ":")?;
                if !pp.compact || !k.is_utf8_str() {
                    write!(w, " ")?;
                }
                rec(w, pp, level + 1, v)
            };
            if !o.is_empty() {
                if pp.sort_keys {
                    let mut o: Vec<_> = o.iter().collect();
                    o.sort_by_key(|(k, _v)| *k);
                    pp.write_seq(w, level, o, kv)
                } else {
                    pp.write_seq(w, level, &**o, kv)
                }?
            }
            bold(w, '}')
        }
    }
}

fn map_err_to_string<T, E: core::fmt::Display>(r: Result<T, E>) -> Result<T> {
    r.map_err(|e| invalid_data(e.to_string()))
}

pub fn print(w: &mut dyn Write, cli: &Cli, val: &Val) -> Result {
    let pp = || Pp {
        compact: cli.compact_output,
        indent: if cli.tab {
            String::from("\t")
        } else {
            " ".repeat(cli.indent())
        },
        sort_keys: cli.sort_keys,
        style: ANSI.if_color(cli.color_stdout()),
    };

    let format = cli.to.unwrap_or(Format::Json);

    if matches!(format, Format::Yaml) {
        // start of YAML document
        writeln!(w, "---")?;
    }

    match (val, format) {
        (Val::Str(b, _), Format::Raw) => w.write_all(b)?,
        (_, Format::Cbor) => cbor::write(w, val)?,
        (_, Format::Json | Format::Raw) => write_json(w, &pp(), 0, val)?,
        (_, Format::Yaml) => write_yaml(w, &pp(), 0, val)?,
        (_, Format::Toml) => write!(w, "{}", map_err_to_string(toml::serialise(val))?)?,
        (_, Format::Xml) => write!(w, "{}", map_err_to_string(xml::serialise(val))?)?,
    };

    if cli.join_output || matches!(format, Format::Cbor) {
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
