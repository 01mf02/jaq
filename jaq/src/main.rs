use clap::Parser;
use colored_json::{ColorMode, ColoredFormatter, CompactFormatter, Output};
use jaq_core::{Definitions, Filter, Val};
use mimalloc::MiMalloc;
use std::io::Write;
use std::path::PathBuf;
use std::process::{ExitCode, Termination};
use std::rc::Rc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
struct Cli {
    /// Use null as single input value
    #[clap(short)]
    null: bool,

    /// Use the last output value as exit status code
    #[clap(short)]
    exit: bool,

    /// Read (slurp) all input values into one array
    #[clap(short)]
    slurp: bool,

    /// Overwrite input file with its output
    #[clap(short)]
    in_place: bool,

    /// Write strings without escaping them with quotes
    #[clap(short, long)]
    raw_output: bool,

    /// Print JSON compactly, omitting whitespace
    #[clap(short, long)]
    compact: bool,

    /// Do not print a newline after each value
    #[clap(short, long)]
    join_output: bool,

    /// Read filter from a file
    ///
    /// In this case, all arguments are interpreted as input files.
    #[clap(short, long, value_name = "FILE")]
    from_file: Option<PathBuf>,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

fn main() -> ExitCode {
    match real_main() {
        Ok(exit) => exit,
        Err(e) => e.report(),
    }
}

fn real_main() -> Result<ExitCode, Error> {
    let cli = Cli::parse();

    let mut args = cli.args.iter();
    let filter = match &cli.from_file {
        Some(file) => parse(&std::fs::read_to_string(file)?)?,
        None => args.next().map(parse).transpose()?.unwrap_or_default(),
    };
    //println!("Filter: {:?}", filter);
    let files: Vec<_> = args.collect();

    use std::iter::once;
    let last = if files.is_empty() {
        let stdin = std::io::stdin();
        let inputs = if cli.null {
            Box::new(once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>
        } else {
            Box::new(read_json(stdin.lock()))
        };
        let inputs = slurp(cli.slurp, inputs);
        run_and_print(&cli, &filter, inputs)?
    } else {
        let mut last = None;
        for file in files {
            let path = std::path::Path::new(file);
            let file =
                std::fs::File::open(file).map_err(|e| Error::Io(Some(file.to_string()), e))?;
            let file = std::io::BufReader::new(file);
            let inputs = read_json(file);
            let inputs = slurp(cli.slurp, inputs);
            if cli.in_place {
                // create a temporary file where output is written to
                let location = path.parent().unwrap();
                let mut tmp = tempfile::Builder::new()
                    .prefix("jaq")
                    .tempfile_in(location)?;

                last = run(&filter, inputs, |output| {
                    writeln!(tmp.as_file_mut(), "{}", output)
                })?;

                // replace the input file with the temporary file
                tmp.persist(path).map_err(Error::Persist)?;
            } else {
                last = run_and_print(&cli, &filter, inputs)?;
            }
        }
        last
    };

    Ok(if cli.exit {
        // return exit code 4 if no value is output
        ExitCode::from(last.map(|b| (!b).into()).unwrap_or(4))
    } else {
        ExitCode::SUCCESS
    })
}

fn parse(filter_str: &String) -> Result<Filter, Vec<ParseError>> {
    let mut errs = Vec::new();
    let mut defs = Definitions::core();
    jaq_std::std()
        .into_iter()
        .for_each(|def| defs.insert(def, &mut errs));
    assert!(errs.is_empty());

    let (main, mut errs) = jaq_core::parse::parse(filter_str, jaq_core::parse::main());

    let filter = main.map(|main| defs.finish(main, &mut errs));
    if errs.is_empty() {
        Ok(filter.unwrap())
    } else {
        Err(errs
            .into_iter()
            .map(|error| ParseError {
                error,
                filter: filter_str.clone(),
            })
            .collect())
    }
}

fn read_json(read: impl std::io::Read) -> impl Iterator<Item = Result<Val, serde_json::Error>> {
    let deserializer = serde_json::Deserializer::from_reader(read);
    let iter = deserializer.into_iter::<serde_json::Value>();
    iter.map(|r| r.map(Val::from))
}

fn slurp<'a>(
    slurp: bool,
    inputs: impl Iterator<Item = Result<Val, serde_json::Error>> + 'a,
) -> impl Iterator<Item = Result<Val, serde_json::Error>> + 'a {
    if slurp {
        let slurped: Result<Vec<_>, _> = inputs.collect();
        Box::new(core::iter::once(slurped.map(|v| Val::Arr(Rc::new(v)))))
    } else {
        Box::new(inputs) as Box<dyn Iterator<Item = _>>
    }
}

#[derive(Debug)]
struct ParseError {
    error: chumsky::error::Simple<String>,
    filter: String,
}

#[derive(Debug)]
enum Error {
    Io(Option<String>, std::io::Error),
    Chumsky(Vec<ParseError>),
    Serde(serde_json::Error),
    Jaq(jaq_core::Error),
    Persist(tempfile::PersistError),
}

impl Termination for Error {
    fn report(self) -> ExitCode {
        let exit = match self {
            Self::Io(prefix, e) => {
                eprint!("Error: ");
                prefix.into_iter().for_each(|p| eprint!("{}: ", p));
                eprintln!("{}", e);
                2
            }
            Self::Persist(e) => {
                eprintln!("Error: {}", e);
                2
            }
            Self::Chumsky(e) => {
                for err in e.into_iter() {
                    report(err.error)
                        .eprint(ariadne::Source::from(err.filter))
                        .unwrap();
                }
                3
            }
            Self::Serde(e) => {
                eprintln!("Error: failed to parse JSON: {}", e);
                4
            }
            Self::Jaq(e) => {
                eprintln!("Error: {}", e);
                5
            }
        };
        ExitCode::from(exit)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Io(None, e)
    }
}

impl From<Vec<ParseError>> for Error {
    fn from(e: Vec<ParseError>) -> Self {
        Self::Chumsky(e)
    }
}

fn run(
    filter: &Filter,
    iter: impl Iterator<Item = Result<Val, serde_json::Error>>,
    mut f: impl FnMut(Val) -> std::io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    for item in iter {
        let input = item.map_err(Error::Serde)?;
        //println!("Got {:?}", input);
        for output in filter.run(input) {
            let output = output.map_err(Error::Jaq)?;
            last = Some(output.as_bool());
            f(output)?;
        }
    }
    Ok(last)
}

fn run_and_print(
    cli: &Cli,
    filter: &Filter,
    iter: impl Iterator<Item = Result<Val, serde_json::Error>>,
) -> Result<Option<bool>, Error> {
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    let last = run(filter, iter, |output| {
        match output {
            Val::Str(s) if cli.raw_output => print!("{}", s),
            _ => {
                if cli.compact {
                    ColoredFormatter::new(CompactFormatter).write_colored_json(
                        &output.into(),
                        &mut stdout,
                        ColorMode::Auto(Output::StdOut),
                    )?
                } else {
                    colored_json::write_colored_json(&output.into(), &mut stdout)?
                }
            }
        };
        if !cli.join_output {
            println!()
        };
        Ok(())
    })?;

    stdout.flush()?;
    Ok(last)
}

fn report(e: chumsky::error::Simple<String>) -> ariadne::Report {
    use ariadne::{Color, Fmt, Label, Report, ReportKind};
    use chumsky::error::SimpleReason;

    let msg = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        let found = if e.found().is_some() {
            "Unexpected token"
        } else {
            "Unexpected end of input"
        };
        let when = if let Some(label) = e.label() {
            format!(" while parsing {}", label)
        } else {
            String::new()
        };
        let expected = if e.expected().len() == 0 {
            "something else".to_string()
        } else {
            e.expected()
                .map(|expected| match expected {
                    Some(expected) => expected.to_string(),
                    None => "end of input".to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ")
        };
        format!("{found}{when}, expected {expected}",)
    };

    let label = match e.reason() {
        SimpleReason::Custom(msg) => msg.clone(),
        _ => format!(
            "Unexpected {}",
            e.found()
                .map(|c| format!("token {}", c.fg(Color::Red)))
                .unwrap_or_else(|| "end of input".to_string())
        ),
    };

    let report = Report::build(ReportKind::Error, (), e.span().start)
        .with_message(msg)
        .with_label(
            Label::new(e.span())
                .with_message(label)
                .with_color(Color::Red),
        );

    let report = match e.reason() {
        SimpleReason::Unclosed { span, delimiter } => report.with_label(
            Label::new(span.clone())
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_color(Color::Yellow),
        ),
        SimpleReason::Unexpected => report,
        SimpleReason::Custom(_) => report,
    };

    report.finish()
}
