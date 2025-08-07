mod cli;
mod filter;
mod funs;
mod read;
mod write;

use cli::{Cli, Format};
use core::fmt::{self, Display, Formatter};
use filter::{run, FileReports, Filter};
use is_terminal::IsTerminal;
use jaq_core::{load, unwrap_valr, Vars};
use jaq_json::{json, Val};
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::{ExitCode, Termination};
use write::{print, with_stdout};

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> ExitCode {
    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().filter_or("LOG", "jaq=debug"))
        .format(|buf, record| match record.level() {
            // format error messages (yielded by `stderr`) without newline
            log::Level::Error => write!(buf, "{}", record.args()),
            // format debug messages such as `["DEBUG:", [1, 2, 3]]`
            level => writeln!(buf, "[\"{}:\", {}]", level, record.args()),
        })
        .init();

    let cli = match Cli::parse() {
        Ok(cli) => cli,
        Err(e) => {
            eprintln!("Error: {e}");
            return ExitCode::from(2);
        }
    };

    if cli.version {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        return ExitCode::SUCCESS;
    } else if cli.help {
        println!("{}", include_str!("help.txt"));
        return ExitCode::SUCCESS;
    }

    let no_color = std::env::var("NO_COLOR").is_ok_and(|v| !v.is_empty());
    let set_color = |on| {
        if on {
            yansi::enable();
        } else {
            yansi::disable();
        }
    };

    set_color(!cli.in_place && cli.color_if(|| io::stdout().is_terminal() && !no_color));

    match real_main(&cli) {
        Ok(exit) => exit,
        Err(e) => {
            set_color(cli.color_if(|| io::stderr().is_terminal() && !no_color));
            eprint!("{e}");
            e.report()
        }
    }
}

fn real_main(cli: &Cli) -> Result<ExitCode, Error> {
    if let Some(test_files) = &cli.run_tests {
        return Ok(match test_files.last() {
            Some(file) => run_tests(io::BufReader::new(std::fs::File::open(file)?)),
            None => run_tests(io::stdin().lock()),
        });
    }

    let (vars, mut ctx): (Vec<String>, Vec<Val>) = binds(cli)?.into_iter().unzip();

    let (vals, filter) = match &cli.filter {
        None => (Vec::new(), Filter::default()),
        Some(filter) => {
            let (path, code) = match filter {
                cli::Filter::FromFile(path) => (path.into(), std::fs::read_to_string(path)?),
                cli::Filter::Inline(filter) => ("<inline>".into(), filter.clone()),
            };
            filter::parse_compile(&path, &code, &vars, &cli.library_path).map_err(Error::Report)?
        }
    };
    ctx.extend(vals);
    //println!("Filter: {:?}", filter);

    let unwrap_or_json = |fmt: Option<Format>| fmt.unwrap_or(Format::Json);
    let last = if cli.files.is_empty() {
        let format = unwrap_or_json(cli.from);
        let s = read::stdin_string(format)?;
        let inputs = read::from_stdin(format, &s, cli.slurp);
        with_stdout(|out| run(cli, &filter, ctx, inputs, |v| print(out, cli, &v)))?
    } else {
        let mut last = None;
        for file in &cli.files {
            let path = Path::new(file);
            let bytes = read::load_file(path)
                .map_err(|e| Error::Io(Some(path.display().to_string()), e))?;
            let format = unwrap_or_json(cli.from.or_else(|| Format::determine(path)));
            let s = read::file_str(format, &bytes)?;
            let inputs = read::from_file(format, &bytes, s, cli.slurp);

            if cli.in_place {
                // create a temporary file where output is written to
                let location = path.parent().unwrap();
                let mut tmp = tempfile::Builder::new()
                    .prefix("jaq")
                    .tempfile_in(location)?;

                last = run(cli, &filter, ctx.clone(), inputs, |output| {
                    print(tmp.as_file_mut(), cli, &output)
                })?;

                // replace the input file with the temporary file
                std::mem::drop(bytes);
                let perms = std::fs::metadata(path)?.permissions();
                tmp.persist(path).map_err(Error::Persist)?;
                std::fs::set_permissions(path, perms)?;
            } else {
                last = with_stdout(|out| {
                    run(cli, &filter, ctx.clone(), inputs, |v| print(out, cli, &v))
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
        use hifijson::token::Lex;
        let mut lexer = hifijson::SliceLexer::new(s.as_bytes());
        let err = |e| Error::Parse(format!("{e} (for value passed to `--argjson {k}`)"));
        Ok((k.to_owned(), lexer.exactly_one(json::parse).map_err(err)?))
    });
    let rawfile = cli.rawfile.iter().map(|(k, path)| {
        let s = std::fs::read_to_string(path).map_err(|e| Error::Io(Some(format!("{path:?}")), e));
        Ok((k.to_owned(), Val::utf8_str(s?)))
    });
    let slurpfile = cli.slurpfile.iter().map(|(k, path)| {
        let a = read::json_array(path).map_err(|e| Error::Io(Some(format!("{path:?}")), e));
        Ok((k.to_owned(), a?))
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

type BoxError = Box<dyn std::error::Error + Send + Sync>;

fn invalid_data(e: impl Into<BoxError>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

#[derive(Debug)]
enum Error {
    Io(Option<String>, io::Error),
    Report(Vec<FileReports>),
    Parse(String),
    Jaq(jaq_core::Error<Val>),
    Persist(tempfile::PersistError),
    FalseOrNull,
    NoOutput,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::FalseOrNull | Self::NoOutput => Ok(()),
            Self::Io(prefix, e) => {
                write!(f, "Error: ")?;
                if let Some(p) = prefix {
                    write!(f, "{p}: ")?;
                }
                writeln!(f, "{e}")
            }
            Self::Persist(e) => {
                writeln!(f, "Error: {e}")
            }
            Self::Report(reports) => reports.iter().try_for_each(|fr| write!(f, "{fr}")),
            Self::Parse(e) => writeln!(f, "Error: failed to parse: {e}"),
            Self::Jaq(e) => writeln!(f, "Error: {e}"),
        }
    }
}

impl Termination for Error {
    fn report(self) -> ExitCode {
        ExitCode::from(match self {
            Self::FalseOrNull => 1,
            Self::Io(_, _) | Self::Persist(_) => 2,
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

fn run_test(test: load::test::Test<String>) -> Result<(Val, Val), Error> {
    let (ctx, filter) =
        filter::parse_compile(&PathBuf::new(), &test.filter, &[], &[]).map_err(Error::Report)?;

    let vars = Vars::new(ctx);
    let cli = Cli::default();
    let inputs = &jaq_std::input::RcIter::new(Box::new(core::iter::empty()));
    let data = funs::Data::new(&cli, &filter.lut, inputs);
    let ctx = filter::Ctx::new(&data, vars);

    let json = |s: String| read::json_single(s.as_bytes());
    let input = json(test.input)?;
    let expect: Result<Val, _> = test.output.into_iter().map(json).collect();
    let obtain: Result<Val, _> = filter.id.run((ctx, input)).collect();
    Ok((expect?, unwrap_valr(obtain).map_err(Error::Jaq)?))
}

fn run_tests(read: impl BufRead) -> ExitCode {
    let lines = read.lines().map(Result::unwrap);
    let tests = load::test::Parser::new(lines);

    let (mut passed, mut total) = (0, 0);
    for test in tests {
        println!("Testing {}", test.filter);
        match run_test(test) {
            Err(e) => eprintln!("{e:?}"),
            Ok((expect, obtain)) if expect != obtain => {
                eprintln!("expected {expect}, obtained {obtain}",);
            }
            Ok(_) => passed += 1,
        }
        total += 1;
    }

    println!("{passed} out of {total} tests passed");

    if total > passed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
