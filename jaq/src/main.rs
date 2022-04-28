use clap::Parser;
use jaq_core::{Definitions, Filter, Val};
use mimalloc::MiMalloc;
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

    #[clap(short)]
    /// Read (slurp) all input values into one array
    slurp: bool,

    #[clap(short, long)]
    /// Write strings without escaping them with quotes
    raw_output: bool,

    #[clap(short, long)]
    /// Do not print a newline after each value
    join_output: bool,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let mut errs = Vec::new();
    let mut defs = Definitions::core();
    jaq_std::std()
        .into_iter()
        .for_each(|def| defs.insert(def, &mut errs));
    assert!(errs.is_empty());

    let mut args = cli.args.iter();
    // TODO: do not parse if no filter is given
    let id = ".".to_string();
    let filter_str = args.next().unwrap_or(&id);
    let files: Vec<_> = args.collect();

    let (main, mut errs) = jaq_core::parse::parse(filter_str, jaq_core::parse::main());

    let filter = main.map(|main| defs.finish(main, &mut errs));
    let filter = if errs.is_empty() {
        filter.unwrap()
    } else {
        for err in errs {
            report(err)
                .eprint(ariadne::Source::from(filter_str))
                .unwrap();
        }
        std::process::exit(3);
    };
    //println!("Filter: {:?}", filter);

    use std::iter::once;
    if files.is_empty() {
        let stdin = std::io::stdin();
        let inputs = if cli.null {
            Box::new(once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>
        } else {
            Box::new(read_json(stdin.lock()))
        };
        let inputs = slurp(cli.slurp, inputs);
        run_and_print(&cli, &filter, inputs)?;
    } else {
        for file in files {
            let file = std::fs::File::open(file)?;
            let file = std::io::BufReader::new(file);
            let inputs = read_json(file);
            let inputs = slurp(cli.slurp, inputs);
            run_and_print(&cli, &filter, inputs)?;
        }
    }
    Ok(())
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

fn run_and_print(
    cli: &Cli,
    filter: &Filter,
    iter: impl Iterator<Item = Result<Val, serde_json::Error>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    let mut last = None;
    for item in iter {
        let input = item.unwrap_or_else(|e| {
            eprintln!("Failed to parse JSON: {}", e);
            std::process::exit(4);
        });
        //println!("Got {:?}", input);
        for output in filter.run(input) {
            let output = output.unwrap_or_else(|e| {
                eprintln!("Error: {}", e);
                std::process::exit(5);
            });
            last = Some(output.as_bool());
            match output {
                Val::Str(s) if cli.raw_output => print!("{}", s),
                _ => colored_json::write_colored_json(&output.into(), &mut stdout)?,
            };
            if !cli.join_output {
                println!()
            }
        }
    }
    stdout.flush()?;

    if cli.exit {
        std::process::exit(last.map(|b| (!b).into()).unwrap_or(4));
    }
    Ok(())
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
