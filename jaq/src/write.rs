use crate::{Cli, Val};
use core::fmt::{self, Display, Formatter};
use is_terminal::IsTerminal;
use std::io::{self, Write};

struct FormatterFn<F>(F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatterFn<F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0(f)
    }
}

struct PpOpts {
    compact: bool,
    indent: String,
    sort_keys: bool,
}

impl PpOpts {
    fn indent(&self, f: &mut Formatter, level: usize) -> fmt::Result {
        if !self.compact {
            write!(f, "{}", self.indent.repeat(level))?;
        }
        Ok(())
    }

    fn newline(&self, f: &mut Formatter) -> fmt::Result {
        if !self.compact {
            writeln!(f)?;
        }
        Ok(())
    }
}

fn fmt_seq<T, I, F>(fmt: &mut Formatter, opts: &PpOpts, level: usize, xs: I, f: F) -> fmt::Result
where
    I: IntoIterator<Item = T>,
    F: Fn(&mut Formatter, T) -> fmt::Result,
{
    opts.newline(fmt)?;
    let mut iter = xs.into_iter().peekable();
    while let Some(x) = iter.next() {
        opts.indent(fmt, level + 1)?;
        f(fmt, x)?;
        if iter.peek().is_some() {
            write!(fmt, ",")?;
        }
        opts.newline(fmt)?;
    }
    opts.indent(fmt, level)
}

fn fmt_val(f: &mut Formatter, opts: &PpOpts, level: usize, v: &Val) -> fmt::Result {
    use yansi::Paint;
    match v {
        Val::Null | Val::Bool(_) | Val::Int(_) | Val::Float(_) | Val::Num(_) => v.fmt(f),
        Val::Str(_) => write!(f, "{}", v.green()),
        Val::Arr(a) => {
            '['.bold().fmt(f)?;
            if !a.is_empty() {
                fmt_seq(f, opts, level, &**a, |f, x| fmt_val(f, opts, level + 1, x))?;
            }
            ']'.bold().fmt(f)
        }
        Val::Obj(o) => {
            '{'.bold().fmt(f)?;
            let kv = |f: &mut Formatter, (k, val): (&std::rc::Rc<String>, &Val)| {
                write!(f, "{}:", Val::Str(k.clone()).bold())?;
                if !opts.compact {
                    write!(f, " ")?;
                }
                fmt_val(f, opts, level + 1, val)
            };
            if !o.is_empty() {
                if opts.sort_keys {
                    let mut o: Vec<_> = o.iter().collect();
                    o.sort_by_key(|(k, _v)| *k);
                    fmt_seq(f, opts, level, o, kv)
                } else {
                    fmt_seq(f, opts, level, &**o, kv)
                }?
            }
            '}'.bold().fmt(f)
        }
    }
}

pub fn print(w: &mut (impl Write + ?Sized), cli: &Cli, val: &Val) -> io::Result<()> {
    let f = |f: &mut Formatter| {
        let opts = PpOpts {
            compact: cli.compact_output,
            indent: if cli.tab {
                String::from("\t")
            } else {
                " ".repeat(cli.indent())
            },
            sort_keys: cli.sort_keys,
        };
        fmt_val(f, &opts, 0, val)
    };

    match val {
        Val::Str(s) if cli.raw_output || cli.join_output => write!(w, "{s}")?,
        _ => write!(w, "{}", FormatterFn(f))?,
    };

    if cli.join_output {
        // when running `jaq -jn '"prompt> " | (., input)'`,
        // this flush is necessary to make "prompt> " appear first
        w.flush()
    } else {
        // this also flushes output, because stdout is line-buffered in Rust
        writeln!(w)
    }
}

pub fn with_stdout<T>(f: impl FnOnce(&mut dyn Write) -> T) -> T {
    let stdout = io::stdout();
    if stdout.is_terminal() {
        f(&mut stdout.lock())
    } else {
        f(&mut io::BufWriter::new(stdout.lock()))
    }
}
