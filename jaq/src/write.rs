use crate::style::{Style, ANSI};
use crate::{invalid_data, Cli, Format};
use jaq_json::{cbor, toml, xml, yaml};
use jaq_json::{write_byte, write_bytes, write_utf8, Tag, Val};
use std::io::{self, IsTerminal, Write};

type Result = io::Result<()>;

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

fn write_val(w: &mut dyn Write, pp: &Pp, level: usize, v: &Val) -> Result {
    let style = &pp.style;
    let rec = |w: &mut dyn Write, level, v| write_val(w, pp, level, v);
    let bold = |w: &mut dyn Write, c| style.write(w, style.bold, |w| write!(w, "{c}"));
    match v {
        Val::Null | Val::Bool(_) | Val::Num(_) => write!(w, "{v}"),
        Val::Str(s, Tag::Utf8) => style.write(w, style.green, |w| {
            write_utf8!(w, s, |part| w.write_all(part))
        }),
        Val::Str(b, Tag::Bytes) => style.write(w, style.red, |w| write_bytes!(w, b)),
        Val::Str(s, Tag::Inline) => w.write_all(s),
        Val::Arr(a) => {
            bold(w, '[')?;
            if !a.is_empty() {
                pp.write_seq(w, level, &**a, |w, x| rec(w, level + 1, x))?;
            }
            bold(w, ']')
        }
        Val::Obj(o) => {
            bold(w, '{')?;
            let kv = |w: &mut dyn Write, (k, v)| {
                rec(w, level + 1, k)?;
                write!(w, ":")?;
                if !pp.compact {
                    write!(w, " ")?;
                }
                rec(w, level + 1, v)
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
        // TODO: move this to fmt_val!
        (Val::Str(b, Tag::Bytes), Format::Yaml) => write!(w, "!!binary {}", yaml::encode_bin(b))?,
        (_, Format::Cbor) => cbor::write(val, &mut *w)?,
        (_, Format::Toml) => {
            let ser = toml::serialise(val).map_err(|e| invalid_data(e.to_string()))?;
            write!(w, "{ser}")?
        }
        (_, Format::Json | Format::Yaml | Format::Raw) => write_val(w, &pp(), 0, val)?,
        (_, Format::Xml) => {
            let ser = xml::serialise(val).map_err(|e| invalid_data(e.to_string()))?;
            write!(w, "{ser}")?
        }
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
