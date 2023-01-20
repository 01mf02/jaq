use clap::{Parser, ValueEnum};
use jaq_core::{Ctx, Definitions, Filter, RcIter, Val};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::process::{ExitCode, Termination};

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

    /// Set variable `$<a>` to string containing the contents of file `f`
    ///
    /// Unlike jq, this provides the variable *only* in the main filter.
    #[arg(long, value_names = &["a", "f"])]
    rawfile: Vec<String>,

    /// Set variable `$<a>` to array containing the JSON values in file `f`
    ///
    /// Unlike jq, this provides the variable *only* in the main filter.
    #[arg(long, value_names = &["a", "f"])]
    slurpfile: Vec<String>,

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

    let (vars, ctx) = binds(&cli)?.into_iter().unzip();

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
        let inputs = read_buffered(&cli, io::stdin().lock());
        with_stdout(|out| run(&cli, &filter, ctx, inputs, |v| print(&cli, v, out)))?
    } else {
        let mut last = None;
        for file in files {
            let path = std::path::Path::new(file);
            let file = mmap_file(path).map_err(|e| Error::Io(Some(file.to_string()), e))?;
            let inputs = read_slice(&cli, &file);
            if cli.in_place {
                // create a temporary file where output is written to
                let location = path.parent().unwrap();
                let mut tmp = tempfile::Builder::new()
                    .prefix("jaq")
                    .tempfile_in(location)?;

                last = run(&cli, &filter, ctx.clone(), inputs, |output| {
                    print(&cli, output, tmp.as_file_mut())
                })?;

                // replace the input file with the temporary file
                let perms = std::fs::metadata(path)?.permissions();
                tmp.persist(path).map_err(Error::Persist)?;
                std::fs::set_permissions(path, perms)?;
            } else {
                last = with_stdout(|out| {
                    run(&cli, &filter, ctx.clone(), inputs, |v| print(&cli, v, out))
                })?;
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

fn bind<F>(var_val: &mut Vec<(String, Val)>, args: &[String], f: F) -> Result<(), Error>
where
    F: Fn(&str) -> Result<Val, Error>,
{
    for arg_val in args.chunks(2) {
        if let [arg, val] = arg_val {
            var_val.push((arg.clone(), f(val)?));
        }
    }
    Ok(())
}

fn binds(cli: &Cli) -> Result<Vec<(String, Val)>, Error> {
    let mut var_val = Vec::new();

    bind(&mut var_val, &cli.arg, |v| {
        Ok(Val::Str(v.to_string().into()))
    })?;
    bind(&mut var_val, &cli.rawfile, |f| {
        let s = std::fs::read_to_string(f).map_err(|e| Error::Io(Some(f.to_string()), e));
        Ok(Val::Str(s?.into()))
    })?;
    bind(&mut var_val, &cli.slurpfile, |f| {
        let path = std::path::Path::new(f);
        let file = mmap_file(path).map_err(|e| Error::Io(Some(f.to_string()), e))?;
        Ok(Val::arr(json_slice(&file).collect::<Result<Vec<_>, _>>()?))
    })?;

    var_val.push(("ARGS".to_string(), args_named(&var_val)));

    Ok(var_val)
}

fn args_named(var_val: &[(String, Val)]) -> Val {
    use std::rc::Rc;
    let named = var_val
        .iter()
        .map(|(var, val)| (var.to_owned().into(), val.clone()));
    let named = Val::Obj(Rc::new(named.collect()));

    let args = std::iter::once(("named".to_string().into(), named));

    Val::Obj(Rc::new(args.collect()))
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

fn mmap_file(path: &std::path::Path) -> io::Result<memmap2::Mmap> {
    let file = std::fs::File::open(path)?;
    unsafe { memmap2::Mmap::map(&file) }
}

fn invalid_data(e: impl std::error::Error + Send + Sync + 'static) -> std::io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

fn json_slice(slice: &[u8]) -> impl Iterator<Item = io::Result<Val>> + '_ {
    let mut lexer = hifijson::SliceLexer::new(slice);
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        let token = lexer.ws_token()?;
        Some(Val::from_token(&mut lexer, token).map_err(invalid_data))
    })
}

fn json_read<'a>(read: impl BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        let token = lexer.ws_token()?;
        let v = Val::from_token(&mut lexer, token);
        Some(v.map_err(|e| core::mem::take(&mut lexer.error).unwrap_or_else(|| invalid_data(e))))
    })
}

fn read_buffered<'a, R>(cli: &Cli, read: R) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a>
where
    R: BufRead + 'a,
{
    if cli.raw_input {
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::str)))
    } else {
        let vals = json_read(read);
        Box::new(collect_if(cli.slurp, vals, Val::arr))
    }
}

fn read_slice<'a>(cli: &Cli, slice: &'a [u8]) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a> {
    if cli.raw_input {
        let read = io::BufReader::new(slice);
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::str)))
    } else {
        let vals = json_slice(slice);
        Box::new(collect_if(cli.slurp, vals, Val::arr))
    }
}

fn raw_input<'a, R>(slurp: bool, mut read: R) -> impl Iterator<Item = io::Result<String>> + 'a
where
    R: BufRead + 'a,
{
    if slurp {
        let mut buf = String::new();
        let s = read.read_to_string(&mut buf).map(|_| buf);
        Box::new(std::iter::once(s))
    } else {
        Box::new(read.lines()) as Box<dyn Iterator<Item = _>>
    }
}

fn collect_if<'a, T: 'a, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
    f: impl FnOnce(Vec<T>) -> T,
) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> {
    if slurp {
        let slurped: Result<Vec<_>, _> = iter.collect();
        Box::new(core::iter::once(slurped.map(f)))
    } else {
        Box::new(iter)
    }
}

#[derive(Debug)]
struct ParseError {
    error: chumsky::error::Simple<String>,
    filter: String,
}

#[derive(Debug)]
enum Error {
    Io(Option<String>, io::Error),
    Chumsky(Vec<ParseError>),
    Parse(String),
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
            Self::Parse(e) => {
                eprintln!("Error: failed to parse: {}", e);
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

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
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
    iter: impl Iterator<Item = io::Result<Val>>,
    mut f: impl FnMut(Val) -> io::Result<()>,
) -> Result<Option<bool>, Error> {
    let mut last = None;
    let iter = iter.map(|r| r.map_err(|e| e.to_string()));

    let iter = Box::new(iter) as Box<dyn Iterator<Item = _>>;
    let null = Box::new(core::iter::once(Ok(Val::Null))) as Box<dyn Iterator<Item = _>>;

    let iter = RcIter::new(iter);
    let null = RcIter::new(null);

    let ctx = Ctx::new(vars, &iter);

    for item in if cli.null_input { &null } else { &iter } {
        let input = item.map_err(Error::Parse)?;
        //println!("Got {:?}", input);
        for output in filter.run(ctx.clone(), input) {
            let output = output.map_err(Error::Jaq)?;
            last = Some(output.as_bool());
            f(output)?;
        }
    }
    Ok(last)
}

fn print(cli: &Cli, val: Val, writer: &mut impl Write) -> io::Result<()> {
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
                    cli.color_mode(),
                )
            } else {
                ColoredFormatter::new(PrettyFormatter::new()).write_colored_json(
                    &val.into(),
                    writer,
                    cli.color_mode(),
                )
            }?
        }
    };
    if !cli.join_output {
        writeln!(writer)?
    }
    Ok(())
}

fn with_stdout<T>(f: impl FnOnce(&mut io::StdoutLock) -> Result<T, Error>) -> Result<T, Error> {
    let mut stdout = io::stdout().lock();
    let y = f(&mut stdout)?;
    stdout.flush()?;
    Ok(y)
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
