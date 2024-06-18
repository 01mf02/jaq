use clap::{Parser, ValueEnum};
use core::fmt::{self, Display, Formatter};
use jaq_interpret::{Ctx, Filter, FilterT, ParseCtx, RcIter, Val};
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
    compact_output: bool,

    /// Use n spaces for indentation
    #[arg(long, value_name = "n", default_value_t = 2)]
    indent: usize,

    /// Use tabs for indentation rather than spaces
    #[arg(long)]
    tab: bool,

    /// Do not print a newline after each value
    ///
    /// Unlike jq, this does not enable `--raw-output`.
    #[arg(short, long)]
    join_output: bool,

    /// Color output
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    color: ColorWhen,

    /// Read filter from a file
    ///
    /// In this case, all arguments are interpreted as input files.
    #[arg(short, long, value_name = "FILE")]
    from_file: Option<PathBuf>,

    /// Set variable `$<a>` to string `<v>`
    #[arg(long, value_names = &["a", "v"])]
    arg: Vec<String>,

    /// Set variable `$<a>` to string containing the contents of file `f`
    #[arg(long, value_names = &["a", "f"])]
    rawfile: Vec<String>,

    /// Set variable `$<a>` to array containing the JSON values in file `f`
    #[arg(long, value_names = &["a", "f"])]
    slurpfile: Vec<String>,

    /// Run tests from a file
    #[arg(long, value_name = "FILE")]
    run_tests: Option<PathBuf>,

    /// Filter to execute, followed by list of input files
    args: Vec<String>,
}

#[derive(Clone, ValueEnum)]
enum ColorWhen {
    Always,
    Auto,
    Never,
}

impl ColorWhen {
    fn use_if(&self, f: impl Fn() -> bool) -> bool {
        match self {
            Self::Always => true,
            Self::Auto => f(),
            Self::Never => false,
        }
    }
}

fn main() -> ExitCode {
    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().filter_or("LOG", "debug"))
        // omit name of module that emitted log message
        .format_target(false)
        .init();

    let cli = Cli::parse();

    if !cli.in_place && cli.color.use_if(|| atty::is(atty::Stream::Stdout)) {
        yansi::enable();
    } else {
        yansi::disable();
    }

    match real_main(&cli) {
        Ok(exit) => exit,
        Err(e) => {
            if cli.color.use_if(|| atty::is(atty::Stream::Stderr)) {
                yansi::enable();
            } else {
                yansi::disable();
            }
            e.report()
        }
    }
}

fn real_main(cli: &Cli) -> Result<ExitCode, Error> {
    if let Some(test_file) = &cli.run_tests {
        return Ok(run_tests(std::fs::File::open(test_file)?));
    }

    let (vars, ctx) = binds(cli)?.into_iter().unzip();

    let mut args = cli.args.iter();
    let filter = match &cli.from_file {
        Some(file) => parse(&std::fs::read_to_string(file)?, vars)?,
        None => {
            if let Some(filter) = args.next() {
                parse(filter, vars)?
            } else {
                Filter::default()
            }
        }
    };
    //println!("Filter: {:?}", filter);
    let files: Vec<_> = args.collect();

    let last = if files.is_empty() {
        let inputs = read_buffered(cli, io::stdin().lock());
        with_stdout(|out| run(cli, &filter, ctx, inputs, |v| print(out, cli, &v)))?
    } else {
        let mut last = None;
        for file in files {
            let path = std::path::Path::new(file);
            let file = load_file(path).map_err(|e| Error::Io(Some(file.to_string()), e))?;
            let inputs = read_slice(cli, &file);
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
        let file = load_file(path).map_err(|e| Error::Io(Some(f.to_string()), e))?;
        Ok(Val::arr(json_slice(&file).collect::<Result<Vec<_>, _>>()?))
    })?;

    var_val.push(("ARGS".to_string(), args_named(&var_val)));
    let env = std::env::vars().map(|(k, v)| (k.into(), Val::str(v)));
    var_val.push(("ENV".to_string(), Val::obj(env.collect())));

    Ok(var_val)
}

fn args_named(var_val: &[(String, Val)]) -> Val {
    let named = var_val
        .iter()
        .map(|(var, val)| (var.clone().into(), val.clone()));
    let args = std::iter::once(("named".to_string().into(), Val::obj(named.collect())));
    Val::obj(args.collect())
}

fn parse(filter_str: &str, vars: Vec<String>) -> Result<Filter, Vec<ParseError>> {
    let mut defs = ParseCtx::new(vars);
    defs.insert_natives(jaq_core::core());
    defs.insert_defs(jaq_std::std());
    assert!(defs.errs.is_empty());
    let (filter, errs) = jaq_parse::parse(filter_str, jaq_parse::main());
    if !errs.is_empty() {
        return Err(errs
            .into_iter()
            .map(|error| ParseError {
                error,
                filter: filter_str.to_owned(),
            })
            .collect());
    }
    let filter = defs.compile(filter.unwrap());
    if defs.errs.is_empty() {
        Ok(filter)
    } else {
        Err(defs
            .errs
            .into_iter()
            .map(|error| ParseError {
                error: chumsky::error::Simple::custom(error.1, error.0.to_string()),
                filter: filter_str.to_owned(),
            })
            .collect())
    }
}

/// Try to load file by memory mapping and fall back to regular loading if it fails.
fn load_file(path: &std::path::Path) -> io::Result<Box<dyn core::ops::Deref<Target = [u8]>>> {
    let file = std::fs::File::open(path)?;
    match unsafe { memmap2::Mmap::map(&file) } {
        Ok(mmap) => Ok(Box::new(mmap)),
        Err(_) => Ok(Box::new(std::fs::read(path)?)),
    }
}

fn invalid_data(e: impl std::error::Error + Send + Sync + 'static) -> std::io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

fn json_slice(slice: &[u8]) -> impl Iterator<Item = io::Result<Val>> + '_ {
    let mut lexer = hifijson::SliceLexer::new(slice);
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        Some(Val::parse(lexer.ws_token()?, &mut lexer).map_err(invalid_data))
    })
}

fn json_read<'a>(read: impl BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        let v = Val::parse(lexer.ws_token()?, &mut lexer);
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
    Jaq(jaq_interpret::Error),
    Persist(tempfile::PersistError),
    FalseOrNull,
    NoOutput,
}

impl Termination for Error {
    fn report(self) -> ExitCode {
        let exit = match self {
            Self::FalseOrNull => 1,
            Self::Io(prefix, e) => {
                eprint!("Error: ");
                if let Some(p) = prefix {
                    eprint!("{p}: ");
                }
                eprintln!("{e}");
                2
            }
            Self::Persist(e) => {
                eprintln!("Error: {e}");
                2
            }
            Self::Chumsky(errs) => {
                for e in errs {
                    eprintln!("Error: {}", report(&e.filter, &e.error));
                }
                3
            }
            Self::NoOutput => 4,
            Self::Parse(e) => {
                eprintln!("Error: failed to parse: {e}");
                5
            }
            Self::Jaq(e) => {
                eprintln!("Error: {e}");
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

/// Run a filter with given input values and run `f` for every value output.
///
/// This function cannot return an `Iterator` because it creates an `RcIter`.
/// This is most unfortunate. We should think about how to simplify this ...
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
        for output in filter.run((ctx.clone(), input)) {
            let output = output.map_err(Error::Jaq)?;
            last = Some(output.as_bool());
            f(output)?;
        }
    }
    Ok(last)
}

struct FormatterFn<F>(F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatterFn<F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0(f)
    }
}

struct PpOpts {
    compact: bool,
    indent: String,
}

impl PpOpts {
    fn indent(&self, f: &mut Formatter, level: usize) -> fmt::Result {
        if !self.compact {
            write!(f, "{}", self.indent.repeat(level))?
        }
        Ok(())
    }

    fn newline(&self, f: &mut Formatter) -> fmt::Result {
        if !self.compact {
            writeln!(f)?
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
        Val::Null => "null".fmt(f),
        Val::Bool(b) => b.fmt(f),
        Val::Int(i) => i.fmt(f),
        Val::Float(x) if x.is_finite() => write!(f, "{x:?}"),
        Val::Float(_) => "null".fmt(f),
        Val::Num(n) => n.fmt(f),
        Val::Str(s) => write!(f, "{:?}", s.green()),
        Val::Arr(a) => {
            '['.bold().fmt(f)?;
            if !a.is_empty() {
                fmt_seq(f, opts, level, &**a, |f, x| fmt_val(f, opts, level + 1, x))?;
            }
            ']'.bold().fmt(f)
        }
        Val::Obj(o) => {
            '{'.bold().fmt(f)?;
            if !o.is_empty() {
                fmt_seq(f, opts, level, &**o, |f, (k, val)| {
                    write!(f, "{:?}:", k.bold())?;
                    if !opts.compact {
                        write!(f, " ")?;
                    }
                    fmt_val(f, opts, level + 1, val)
                })?;
            }
            '}'.bold().fmt(f)
        }
    }
}

fn print(writer: &mut impl Write, cli: &Cli, val: &Val) -> io::Result<()> {
    let f = |f: &mut Formatter| fmt_val_root(f, cli, val);
    write!(writer, "{}", FormatterFn(f))
}

fn fmt_val_root(f: &mut Formatter, cli: &Cli, val: &Val) -> fmt::Result {
    match val {
        Val::Str(s) if cli.raw_output => write!(f, "{s}")?,
        _ => {
            let opts = PpOpts {
                compact: cli.compact_output,
                indent: if cli.tab {
                    String::from("\t")
                } else {
                    " ".repeat(cli.indent)
                },
            };
            fmt_val(f, &opts, 0, val)?;
        }
    };
    if !cli.join_output {
        writeln!(f)?;
    }
    Ok(())
}

fn with_stdout<T>(f: impl FnOnce(&mut io::StdoutLock) -> Result<T, Error>) -> Result<T, Error> {
    let mut stdout = io::stdout().lock();
    let y = f(&mut stdout)?;
    stdout.flush()?;
    Ok(y)
}

#[derive(Debug)]
struct Report<'a> {
    code: &'a str,
    message: String,
    labels: Vec<(core::ops::Range<usize>, String, Color)>,
}

#[derive(Clone, Debug)]
enum Color {
    Yellow,
    Red,
}

impl Color {
    fn apply(&self, d: impl Display) -> String {
        use yansi::{Color, Paint};
        let color = match self {
            Self::Yellow => Color::Yellow,
            Self::Red => Color::Red,
        };
        d.fg(color).to_string()
    }
}

fn report<'a>(code: &'a str, e: &chumsky::error::Simple<String>) -> Report<'a> {
    use chumsky::error::SimpleReason;

    let eof = || "end of input".to_string();

    let message = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        let found = if e.found().is_some() {
            "Unexpected token"
        } else {
            "Unexpected end of input"
        };
        let when = if let Some(label) = e.label() {
            format!(" while parsing {label}")
        } else {
            String::new()
        };
        let expected = if e.expected().len() == 0 {
            "something else".to_string()
        } else {
            let f = |e: &Option<String>| e.as_ref().map_or_else(eof, |e| e.to_string());
            e.expected().map(f).collect::<Vec<_>>().join(", ")
        };
        format!("{found}{when}, expected {expected}",)
    };

    let label = if let SimpleReason::Custom(msg) = e.reason() {
        msg.clone()
    } else {
        let token = |c: &String| format!("token {}", Color::Red.apply(c));
        format!("Unexpected {}", e.found().map_or_else(eof, token))
    };
    // convert character indices to byte offsets
    let char_to_byte = |i| {
        code.char_indices()
            .map(|(i, _c)| i)
            .chain([code.len(), code.len()])
            .nth(i)
            .unwrap()
    };
    let conv = |span: &core::ops::Range<_>| char_to_byte(span.start)..char_to_byte(span.end);
    let mut labels = Vec::from([(conv(&e.span()), label, Color::Red)]);

    if let SimpleReason::Unclosed { span, delimiter } = e.reason() {
        let text = format!("Unclosed delimiter {}", Color::Yellow.apply(delimiter));
        labels.insert(0, (conv(span), text, Color::Yellow));
    }
    Report {
        code,
        message,
        labels,
    }
}

impl Display for Report<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use codesnake::{Block, CodeWidth, Label, LineIndex};
        let idx = LineIndex::new(self.code);
        let labels = self.labels.clone().into_iter().map(|(range, text, color)| {
            Label::new(range, text).with_style(move |s| color.apply(s).to_string())
        });
        let block = Block::new(&idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        });
        writeln!(f, "{}", self.message)?;
        write!(f, "{}\n{}{}", block.prologue(), block, block.epilogue())
    }
}

fn run_test(test: jaq_syn::test::Test<String>) -> Result<(Val, Val), Error> {
    let inputs = RcIter::new(Box::new(core::iter::empty()));
    let ctx = Ctx::new(Vec::new(), &inputs);

    let filter = parse(&test.filter, Vec::new())?;

    let json = |s: String| {
        use hifijson::token::Lex;
        hifijson::SliceLexer::new(s.as_bytes())
            .exactly_one(Val::parse)
            .map_err(invalid_data)
    };
    let input = json(test.input)?;
    let expect: Result<Vec<_>, _> = test.output.into_iter().map(json).collect();
    let obtain: Result<Vec<_>, _> = filter.run((ctx, input)).collect();
    Ok((Val::arr(expect?), Val::arr(obtain.map_err(Error::Jaq)?)))
}

fn run_tests(file: std::fs::File) -> ExitCode {
    let lines = io::BufReader::new(file).lines().map(Result::unwrap);
    let tests = jaq_syn::test::Parser::new(lines);

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
