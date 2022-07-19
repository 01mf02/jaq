use clap::{ArgEnum, Parser};
use jaq_core::{Ctx, Definitions, Filter, Val};
use mimalloc::MiMalloc;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::{ExitCode, Termination};
use std::rc::Rc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// Just Another Query Tool
#[derive(Parser)]
struct Cli {
    /// Use null as single input value
    #[clap(short, long)]
    null_input: bool,

    /// Use the last output value as exit status code
    ///
    /// If there is some last output value `v`,
    /// then the exit status code is
    /// 1 if `v < true` (that is, if `v` is `false` or `null`) and
    /// 0 otherwise.
    /// If there is no output value, then the exit status code is 4.
    ///
    /// If any error occurs, then this option is ignored.
    #[clap(short, long)]
    exit_status: bool,

    /// Read (slurp) all input values into one array
    ///
    /// When input is read from files,
    /// jaq yields an array for each file, whereas
    /// jq produces only a single array.
    #[clap(short, long)]
    slurp: bool,

    /// Overwrite input file with its output
    #[clap(short, long)]
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

    /// Color output
    #[clap(long, value_name = "WHEN", arg_enum, default_value = "auto")]
    color: Color,

    /// Read filter from a file
    ///
    /// In this case, all arguments are interpreted as input files.
    #[clap(short, long, value_name = "FILE")]
    from_file: Option<PathBuf>,

    /// Set variable `$<a>` to string "<v>"
    ///
    /// Unlike jq, this provides the variable *only* in the main filter.
    #[clap(long, value_names = &["a", "v"])]
    arg: Vec<String>,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

#[derive(Clone, ArgEnum)]
enum Color {
    Always,
    Auto,
    Never,
}

fn main() -> ExitCode {
    match real_main() {
        Ok(exit) => exit,
        Err(e) => e.report(),
    }
}

fn real_main() -> Result<ExitCode, Error> {
    let cli = Cli::parse();

    let mut vars = Vec::new();
    let mut ctx = Ctx::new();
    for arg_val in cli.arg.chunks(2) {
        if let [arg, val] = arg_val {
            vars.push(arg.clone());
            ctx = Ctx::Cons(Val::Str(val.clone().into()), ctx.into());
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

    use std::iter::once;
    let last = if files.is_empty() {
        let stdin = std::io::stdin();
        let inputs = if cli.null_input {
            Box::new(once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>
        } else {
            Box::new(read_json(stdin.lock()))
        };
        let inputs = slurp(cli.slurp, inputs);
        run_and_print(&cli, &filter, ctx, inputs)?
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

                last = run(&filter, ctx.clone(), inputs, |output| {
                    writeln!(tmp.as_file_mut(), "{output}")
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

fn parse(filter_str: &String, vars: Vec<String>) -> Result<Filter, Vec<ParseError>> {
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
                filter: filter_str.clone(),
            })
            .collect())
    }
}

fn read_json(read: impl Read) -> impl Iterator<Item = Result<Val, serde_json::Error>> {
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
    ctx: Ctx,
    iter: impl Iterator<Item = Result<Val, serde_json::Error>>,
    mut f: impl FnMut(Val) -> std::io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    for item in iter {
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

fn run_and_print(
    cli: &Cli,
    filter: &Filter,
    ctx: Ctx,
    iter: impl Iterator<Item = Result<Val, serde_json::Error>>,
) -> Result<Option<bool>, Error> {
    let mut stdout = std::io::stdout().lock();

    use colored_json::{ColorMode, ColoredFormatter, CompactFormatter, Output, PrettyFormatter};

    let color = match cli.color {
        Color::Always => ColorMode::On,
        Color::Auto => ColorMode::Auto(Output::StdOut),
        Color::Never => ColorMode::Off,
    };

    let last = run(filter, ctx, iter, |output| {
        match output {
            Val::Str(s) if cli.raw_output || cli.join_output => print!("{}", s),
            _ => {
                // this looks ugly, but it is hard to abstract over the `Formatter` because
                // we cannot create a `Box<dyn Formatter>` because
                // Rust says that the `Formatter` trait is not "object safe"
                if cli.compact {
                    ColoredFormatter::new(CompactFormatter).write_colored_json(
                        &output.into(),
                        &mut stdout,
                        color,
                    )
                } else {
                    ColoredFormatter::new(PrettyFormatter::new()).write_colored_json(
                        &output.into(),
                        &mut stdout,
                        color,
                    )
                }?
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
