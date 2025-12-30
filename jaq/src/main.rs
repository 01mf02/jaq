mod cli;
mod filter;
mod funs;
mod tests;
#[cfg(target_os = "windows")]
mod windows;

use cli::{Cli, Format};
use core::fmt::{self, Formatter};
use filter::run;
use jaq_all::data::{Filter, Runner};
use jaq_all::fmts::read;
use jaq_all::fmts::write::{with_stdout, write, Writer};
use jaq_all::json::write::{Pp, Styles};
use jaq_all::json::Val;
use jaq_all::load::{Color, FileReports, FileReportsDisp};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{ExitCode, Termination};

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

extern crate alloc;

fn main() -> io::Result<ExitCode> {
    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().filter_or("LOG", "jaq=debug"))
        .format(|buf, record| match record.level() {
            // format error messages (yielded by `stderr`) without newline
            log::Level::Error => write!(buf, "{}", record.args()),
            // format debug messages such as `["DEBUG:", [1, 2, 3]]`
            level => writeln!(buf, "[\"{}:\", {}]", level, record.args()),
        })
        .init();

    let mut out = io::stdout();
    let mut err = io::stderr();

    let cli = match Cli::parse() {
        Ok(cli) => cli,
        Err(e) => {
            writeln!(err, "Error: {e}")?;
            return Ok(ExitCode::from(2));
        }
    };

    if cli.version {
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        writeln!(out, "{name} {version}")?;
        Ok(ExitCode::SUCCESS)
    } else if cli.help {
        writeln!(out, "{}", include_str!("help.txt"))?;
        Ok(ExitCode::SUCCESS)
    } else if let Some(test_files) = &cli.run_tests {
        match test_files.last() {
            Some(file) => tests::run(io::BufReader::new(std::fs::File::open(file)?)),
            None => tests::run(io::stdin().lock()),
        }
    } else {
        real_main(&cli).or_else(|e| {
            write!(err, "{}", ErrorColor::new(&e, cli.color_errors()))?;
            Ok(e.report())
        })
    }
}

impl Cli {
    fn runner(&self) -> Runner {
        Runner {
            null_input: self.null_input,
            color_err: self.color_errors(),
            writer: self.writer(),
        }
    }

    fn writer(&self) -> Writer {
        Writer {
            pp: self.pp(),
            format: self.to.unwrap_or_default(),
            join: self.join_output,
        }
    }

    fn pp(&self) -> Pp {
        Pp {
            indent: (!self.compact_output).then(|| self.indent()),
            sort_keys: self.sort_keys,
            styles: self.styles(),
            sep_space: !self.compact_output || matches!(self.to, Some(Format::Yaml)),
        }
    }

    fn styles(&self) -> Styles {
        self.color_output()
            .then(Styles::ansi)
            .map(|c| match std::env::var("JQ_COLORS") {
                Err(_) => c,
                Ok(s) => c.parse(&s),
            })
            .unwrap_or_default()
    }
}

fn real_main(cli: &Cli) -> Result<ExitCode, Error> {
    let (var_names, mut vars): (Vec<String>, Vec<Val>) = binds(cli)?.into_iter().unzip();

    let (var_vals, filter) = match &cli.filter {
        None => (Vec::new(), Filter::default()),
        Some(filter) => {
            let (path, code) = match filter {
                cli::Filter::FromFile(path) => (path.into(), std::fs::read_to_string(path)?),
                cli::Filter::Inline(filter) => ("<inline>".into(), filter.clone()),
            };
            filter::parse_compile(&path, &code, &var_names, &cli.library_path)
                .map_err(Error::Report)?
        }
    };
    vars.extend(var_vals);
    let vars = jaq_all::jaq_core::Vars::new(vars);
    //println!("Filter: {:?}", filter);

    let runner = &cli.runner();
    let writer = &runner.writer;

    let unwrap_or_json = |fmt: Option<Format>| fmt.unwrap_or_default();
    let last = if cli.files.is_empty() {
        let format = unwrap_or_json(cli.from);
        let s = read::read_string(format, io::stdin().lock())?;
        let inputs = read::from_bufread(format, io::stdin().lock(), &s, cli.slurp);
        with_stdout(|out| run(runner, &filter, vars, inputs, |v| write(out, writer, &v)))?
    } else {
        let mut last = None;
        for file in &cli.files {
            let path = Path::new(file);
            let bytes = read::load_file(path)
                .map_err(|e| Error::Io(Some(path.display().to_string()), e))?;
            let format = unwrap_or_json(cli.from.or_else(|| Format::determine(path)));
            let s = read::bytes_str(format, &bytes)?;
            let inputs = read::from_bytes(format, &bytes, s, cli.slurp);

            if cli.in_place {
                // create a temporary file where output is written to
                let location = path.parent().unwrap();
                let mut tmp = tempfile::Builder::new()
                    .prefix("jaq")
                    .tempfile_in(location)?;

                last = run(runner, &filter, vars.clone(), inputs, |output| {
                    write(tmp.as_file_mut(), writer, &output)
                })?;

                // replace the input file with the temporary file
                std::mem::drop(bytes);
                let perms = std::fs::metadata(path)?.permissions();
                tmp.persist(path).map_err(|e| Error::Io(None, e.into()))?;
                std::fs::set_permissions(path, perms)?;
            } else {
                last = with_stdout(|out| {
                    run(runner, &filter, vars.clone(), inputs, |v| {
                        write(out, writer, &v)
                    })
                })?;
            }
        }
        last
    };

    if cli.exit_status {
        last.map_or_else(
            || Err(Error::NoOutput),
            |b| b.then_some(ExitCode::SUCCESS).ok_or(Error::FalseOrNull),
        )
    } else {
        Ok(ExitCode::SUCCESS)
    }
}

fn binds(cli: &Cli) -> Result<Vec<(String, Val)>, Error> {
    let arg = cli.arg.iter().map(|(k, s)| {
        let s = s.to_owned();
        Ok((k.to_owned(), Val::utf8_str(s)))
    });
    let argjson = cli.argjson.iter().map(|(k, s)| {
        let err = |e| Error::Parse(format!("{e} (for value passed to `--argjson {k}`)"));
        let s = s.as_bytes();
        Ok((k.to_owned(), read::json::parse_single(s).map_err(err)?))
    });
    let rawfile = cli.rawfile.iter().map(|(k, path)| {
        let err = |e| Error::Io(Some(format!("{path:?}")), e);
        let s = read::load_file(path).map_err(err)?;
        Ok((k.to_owned(), Val::utf8_str(s)))
    });
    let slurpfile = cli.slurpfile.iter().map(|(k, path)| {
        let err = |e| Error::Io(Some(format!("{path:?}")), e);
        Ok((k.to_owned(), read::json_array(path).map_err(err)?))
    });

    let positional = cli.args.iter().cloned().map(|s| Ok(Val::from(s)));
    let positional = positional.collect::<Result<Vec<_>, Error>>()?;

    let var_val = arg.chain(rawfile).chain(slurpfile).chain(argjson);
    let mut var_val = var_val.collect::<Result<Vec<_>, Error>>()?;

    var_val.push(("ARGS".to_string(), args(&positional, &var_val)));
    let env = std::env::vars().map(|(k, v)| (k.into(), Val::from(v)));
    var_val.push(("ENV".to_string(), Val::obj(env.collect())));

    Ok(var_val)
}

fn args(positional: &[Val], named: &[(String, Val)]) -> Val {
    let key = |k: &str| k.to_string().into();
    let positional = positional.iter().cloned();
    let named = named.iter().map(|(var, val)| (key(var), val.clone()));
    let obj = [
        (key("positional"), positional.collect()),
        (key("named"), Val::obj(named.collect())),
    ];
    Val::obj(obj.into_iter().collect())
}

#[derive(Debug)]
enum Error {
    Io(Option<String>, io::Error),
    Report(Vec<FileReports<PathBuf>>),
    Parse(String),
    Jaq(jaq_all::json::Error),
    FalseOrNull,
    NoOutput,
}

struct ErrorColor<'e>(&'e Error, fn(Color, String) -> String);

impl<'e> ErrorColor<'e> {
    fn new(e: &'e Error, color: bool) -> Self {
        Self(e, if color { Color::ansi } else { |_, text| text })
    }
}

impl fmt::Display for ErrorColor<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self(error, color) = self;
        match error {
            Error::FalseOrNull | Error::NoOutput => Ok(()),
            Error::Io(prefix, e) => {
                write!(f, "Error: ")?;
                if let Some(p) = prefix {
                    write!(f, "{p}: ")?;
                }
                writeln!(f, "{e}")
            }
            Error::Report(reports) => reports.iter().try_for_each(|fr| {
                FileReportsDisp::new(fr)
                    .with_paint(*color)
                    .with_path(|p| format!("[{}]", p.display()))
                    .fmt(f)
            }),
            Error::Parse(e) => writeln!(f, "Error: failed to parse: {e}"),
            Error::Jaq(e) => writeln!(f, "Error: {e}"),
        }
    }
}

impl Termination for Error {
    fn report(self) -> ExitCode {
        ExitCode::from(match self {
            Self::FalseOrNull => 1,
            Self::Io(_, _) => 2,
            Self::Report(_) => 3,
            Self::NoOutput => 4,
            Self::Parse(_) | Self::Jaq(_) => 5,
        })
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(None, e)
    }
}
