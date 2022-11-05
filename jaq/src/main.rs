use clap::{Parser, ValueEnum};
use jaq_core::{Ctx, Definitions, Filter, RcIter, Val};
use std::io::{BufRead, Write};
use std::path::PathBuf;
use std::process::{ExitCode, Termination};
use std::rc::Rc;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Just Another Query Tool
#[derive(Parser)]
#[command(version)]
struct Cli {
    /// Use null as single input value
    #[arg(short, long)]
    null_input: bool,

    /// Use the last output value as exit status code
    ///
    /// If there is some last output value `v`,
    /// then the exit status code is
    /// 1 if `v < true` (that is, if `v` is `false` or `null`) and
    /// 0 otherwise.
    /// If there is no output value, then the exit status code is 4.
    ///
    /// If any error occurs, then this option has no effect.
    #[arg(short, long)]
    exit_status: bool,

    /// Read (slurp) all input values into one array
    ///
    /// When input is read from files,
    /// jaq yields an array for each file, whereas
    /// jq produces only a single array.
    #[arg(short, long)]
    slurp: bool,

    /// Overwrite input file with its output
    #[arg(short, long)]
    in_place: bool,

    /// Write strings without escaping them with quotes
    #[arg(short, long)]
    raw_output: bool,

    /// Read lines of the input as sequence of strings
    ///
    /// When the option `--slurp` is used additionally,
    /// then the whole input is read into a single string.
    #[arg(short = 'R', long)]
    raw_input: bool,

    /// Print JSON compactly, omitting whitespace
    #[arg(short, long)]
    compact: bool,

    /// Do not print a newline after each value
    ///
    /// Unlike jq, this does not enable `--raw-output`.
    #[arg(short, long)]
    join_output: bool,

    /// Color output
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    color: Color,

    /// Read filter from a file
    ///
    /// In this case, all arguments are interpreted as input files.
    #[arg(short, long, value_name = "FILE")]
    from_file: Option<PathBuf>,

    /// Set variable `$<a>` to string "<v>"
    ///
    /// Unlike jq, this provides the variable *only* in the main filter.
    #[arg(long, value_names = &["a", "v"])]
    arg: Vec<String>,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

#[derive(Clone, ValueEnum)]
enum Color {
    Always,
    Auto,
    Never,
}

impl Cli {
    fn color_mode(&self) -> colored_json::ColorMode {
        use colored_json::{ColorMode, Output};
        match self.color {
            Color::Always => ColorMode::On,
            Color::Auto if self.in_place => ColorMode::Off,
            Color::Auto => ColorMode::Auto(Output::StdOut),
            Color::Never => ColorMode::Off,
        }
    }
}

fn main() -> ExitCode {
    match real_main() {
        Ok(exit) => exit,
        Err(e) => e.report(),
    }
}

fn real_main() -> Result<ExitCode, Error> {
    let cli = Cli::parse();

    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().filter_or("LOG", "debug"))
        // omit name of module that emitted log message
        .format_target(false)
        .init();

    let mut vars = Vec::new();
    let mut ctx = Vec::new();
    for arg_val in cli.arg.chunks(2) {
        if let [arg, val] = arg_val {
            vars.push(arg.clone());
            ctx.push(Val::Str(val.clone().into()));
        }
    }

    let mut args = cli.args.iter();
    let filter = match &cli.from_file {
        Some(file) => parse(&std::fs::read_to_string(file)?, vars)?,
        None => {
            if let Some(filter) = args.next() {
                parse(filter, vars)?
            } else {
                Default::default()
            }
        }
    };
    //println!("Filter: {:?}", filter);
    let files: Vec<_> = args.collect();

    let last = if files.is_empty() {
        let inputs = read_input(&cli, std::io::stdin().lock());
        run_and_print(&cli, &filter, ctx, inputs)?
    } else {
        let mut last = None;
        for file in files {
            let path = std::path::Path::new(file);
            let file =
                std::fs::File::open(file).map_err(|e| Error::Io(Some(file.to_string()), e))?;
            let file = std::io::BufReader::new(file);
            let inputs = read_input(&cli, file);
            if cli.in_place {
                // create a temporary file where output is written to
                let location = path.parent().unwrap();
                let mut tmp = tempfile::Builder::new()
                    .prefix("jaq")
                    .tempfile_in(location)?;

                let color = cli.color_mode();
                last = run(&cli, &filter, ctx.clone(), inputs, |output| {
                    print(&cli, color, output, tmp.as_file_mut())
                })?;

                // replace the input file with the temporary file
                tmp.persist(path).map_err(Error::Persist)?;
            } else {
                last = run_and_print(&cli, &filter, ctx.clone(), inputs)?;
            }
        }
        last
    };

    Ok(if cli.exit_status {
        // return exit code 4 if no value is output
        ExitCode::from(last.map(|b| (!b).into()).unwrap_or(4))
    } else {
        ExitCode::SUCCESS
    })
}

fn parse(filter_str: &str, vars: Vec<String>) -> Result<Filter, Vec<ParseError>> {
    let mut errs = Vec::new();
    let mut defs = Definitions::core();
    jaq_std::std()
        .into_iter()
        .for_each(|def| defs.insert(def, &mut errs));
    assert!(errs.is_empty());

    let (main, mut errs) = jaq_core::parse::parse(filter_str, jaq_core::parse::main());

    let filter = main.map(|main| defs.finish(main, vars, &mut errs));
    if errs.is_empty() {
        Ok(filter.unwrap())
    } else {
        Err(errs
            .into_iter()
            .map(|error| ParseError {
                error,
                filter: filter_str.to_owned(),
            })
            .collect())
    }
}

fn read_input<'a>(
    cli: &Cli,
    mut read: impl BufRead + 'a,
) -> Box<dyn Iterator<Item = Result<Val, std::io::Error>> + 'a> {
    if cli.raw_input {
        if cli.slurp {
            let mut buffer = String::new();
            read.read_to_string(&mut buffer).unwrap();
            Box::new(std::iter::once(Ok(Val::Str(buffer.into()))))
        } else {
            Box::new(read.lines().map(|line| Ok(Val::Str(line.unwrap().into()))))
        }
    } else {
        let inputs = read.lines().map(|line| {
            let line = line?;
            let parsed = json_deserializer::parse(line.as_bytes()).map_err(|e| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}: {}", e, line))
            })?;
            Ok(Val::from(parsed))
        });

        if cli.slurp {
            let slurped: Result<Vec<_>, _> = inputs.collect();
            Box::new(core::iter::once(slurped.map(|v| Val::Arr(Rc::new(v)))))
        } else {
            Box::new(inputs)
        }
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
    Serde(String),
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
    cli: &Cli,
    filter: &Filter,
    vars: Vec<Val>,
    iter: impl Iterator<Item = Result<Val, std::io::Error>>,
    mut f: impl FnMut(Val) -> std::io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    let iter = iter.map(|r| r.map_err(|e| e.to_string()));

    let iter = Box::new(iter) as Box<dyn Iterator<Item = _>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let iter = RcIter::new(iter);
    let null = RcIter::new(null);

    let ctx = Ctx::new(vars, &iter);

    for item in if cli.null_input { &null } else { &iter } {
        let input = item.map_err(Error::Serde)?;
        //println!("Got {:?}", input);
        for output in filter.run(ctx.clone(), input) {
            let output = output.map_err(Error::Jaq)?;
            last = Some(output.as_bool());
            f(output)?;
        }
    }
    Ok(last)
}

fn print(
    cli: &Cli,
    color: colored_json::ColorMode,
    val: Val,
    writer: &mut impl Write,
) -> std::io::Result<()> {
    use colored_json::{ColoredFormatter, CompactFormatter, PrettyFormatter};
    match val {
        Val::Str(s) if cli.raw_output => write!(writer, "{s}")?,
        _ => {
            // this looks ugly, but it is hard to abstract over the `Formatter` because
            // we cannot create a `Box<dyn Formatter>` because
            // Rust says that the `Formatter` trait is not "object safe"
            if cli.compact {
                ColoredFormatter::new(CompactFormatter).write_colored_json(
                    &val.into(),
                    writer,
                    color,
                )
            } else {
                ColoredFormatter::new(PrettyFormatter::new()).write_colored_json(
                    &val.into(),
                    writer,
                    color,
                )
            }?
        }
    };
    if !cli.join_output {
        writeln!(writer)?
    }
    Ok(())
}

fn run_and_print(
    cli: &Cli,
    filter: &Filter,
    vars: Vec<Val>,
    iter: impl Iterator<Item = Result<Val, std::io::Error>>,
) -> Result<Option<bool>, Error> {
    let mut stdout = std::io::stdout().lock();

    let color = cli.color_mode();

    let last = run(cli, filter, vars, iter, |output| {
        print(cli, color, output, &mut stdout)
    })?;

    stdout.flush()?;
    Ok(last)
}

fn report(e: chumsky::error::Simple<String>) -> ariadne::Report {
    use ariadne::{Color, Fmt, Label, Report, ReportKind};
    use chumsky::error::SimpleReason;

    // color error messages only if we are on a tty
    let isatty = atty::is(atty::Stream::Stderr);
    let (red, yellow) = if isatty {
        (Color::Red, Color::Yellow)
    } else {
        (Color::Unset, Color::Unset)
    };
    let config = ariadne::Config::default().with_color(isatty);

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
                .map(|c| format!("token {}", c.fg(red)))
                .unwrap_or_else(|| "end of input".to_string())
        ),
    };

    let report = Report::build(ReportKind::Error, (), e.span().start)
        .with_message(msg)
        .with_label(Label::new(e.span()).with_message(label).with_color(red));

    let report = match e.reason() {
        SimpleReason::Unclosed { span, delimiter } => report.with_label(
            Label::new(span.clone())
                .with_message(format!("Unclosed delimiter {}", delimiter.fg(yellow)))
                .with_color(yellow),
        ),
        SimpleReason::Unexpected => report,
        SimpleReason::Custom(_) => report,
    };

    report.with_config(config).finish()
}
