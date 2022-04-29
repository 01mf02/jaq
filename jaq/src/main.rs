use clap::Parser;
use jaq_core::{Definitions, Filter, Val};
use mimalloc::MiMalloc;
use std::fmt;
use std::io::Write;
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

    /// Do not print a newline after each value
    #[clap(short, long)]
    join_output: bool,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

fn main() {
    if let Err(e) = real_main() {
        println!("Error: {}", e);
        std::process::exit(e.exit_code());
    }
}

fn real_main() -> Result<(), Error> {
    let cli = Cli::parse();

    let mut args = cli.args.iter();
    let filter = args.next().map(parse).unwrap_or_default();
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

    if cli.exit {
        std::process::exit(last.map(|b| (!b).into()).unwrap_or(4));
    }
    Ok(())
}

fn parse(filter_str: &String) -> Filter {
    let mut errs = Vec::new();
    let mut defs = Definitions::core();
    jaq_std::std()
        .into_iter()
        .for_each(|def| defs.insert(def, &mut errs));
    assert!(errs.is_empty());

    let (main, mut errs) = jaq_core::parse::parse(filter_str, jaq_core::parse::main());

    let filter = main.map(|main| defs.finish(main, &mut errs));
    if errs.is_empty() {
        filter.unwrap()
    } else {
        for err in errs {
            report(err)
                .eprint(ariadne::Source::from(filter_str))
                .unwrap();
        }
        std::process::exit(3);
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
enum Error {
    Io(Option<String>, std::io::Error),
    Serde(serde_json::Error),
    Jaq(jaq_core::Error),
    Persist(tempfile::PersistError),
}

impl Error {
    fn exit_code(&self) -> i32 {
        match self {
            Self::Io(..) | Self::Persist(_) => 2,
            Self::Serde(_) => 4,
            Self::Jaq(_) => 5,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Io(None, e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Io(None, e) => e.fmt(f),
            Self::Io(Some(s), e) => write!(f, "{}: {}", s, e),
            Self::Serde(e) => write!(f, "failed to parse JSON: {}", e),
            Self::Jaq(e) => e.fmt(f),
            Self::Persist(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

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
            _ => colored_json::write_colored_json(&output.into(), &mut stdout)?,
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
