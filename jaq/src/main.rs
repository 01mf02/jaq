use clap::{Parser, ValueEnum};
use core::fmt::{self, Display, Formatter};
use jaq_core::{compile, load, Ctx, Native, RcIter};
use jaq_json::Val;
use std::ffi::{OsStr, OsString};
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::{ExitCode, Termination};

type Filter = jaq_core::Filter<Native<Val>>;

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
    ///
    /// When this is set to `auto`, colors are enabled if
    /// output is written to a terminal and
    /// the `NO_COLOR` environment variable is not set.
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    color: ColorWhen,

    /// Read filter from a file given by filter argument
    #[arg(short, long)]
    from_file: bool,

    /// Search for modules and data in given directory
    ///
    /// If this option is given multiple times, all given directories are searched.
    #[arg(short = 'L', long, value_name = "DIR")]
    library_path: Vec<PathBuf>,

    /// Set variable `$<a>` to string `<v>`
    #[arg(long, value_names = &["a", "v"])]
    arg: Vec<OsString>,

    /// Set variable `$<a>` to string containing the contents of file `f`
    #[arg(long, value_names = &["a", "f"])]
    rawfile: Vec<OsString>,

    /// Set variable `$<a>` to array containing the JSON values in file `f`
    #[arg(long, value_names = &["a", "f"])]
    slurpfile: Vec<OsString>,

    /// Run tests from a file
    #[arg(long, value_name = "FILE")]
    run_tests: Option<PathBuf>,

    /// Collect positional arguments into `$ARGS.positional`
    ///
    /// When using this flag, positional arguments are not used as input files.
    #[arg(long)]
    args: bool,

    /// Filter to execute
    ///
    /// If this argument is not given, it is assumed to be `.`, the identity filter.
    filter: Option<OsString>,

    /// Positional arguments, by default used as input files
    #[arg(name = "ARG")]
    posargs: Vec<OsString>,
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
        .format(|buf, record| match record.level() {
            // format error messages (yielded by `stderr`) without newline
            log::Level::Error => write!(buf, "{}", record.args()),
            // format debug messages such as `["DEBUG:", [1, 2, 3]]`
            level => writeln!(buf, "[\"{}:\", {}]", level, record.args()),
        })
        .init();

    let cli = Cli::parse();

    let no_color = std::env::var("NO_COLOR").map_or(false, |v| !v.is_empty());
    let detect_color = |stream| atty::is(stream) && !no_color;
    let set_color = |on| {
        if on {
            yansi::enable();
        } else {
            yansi::disable();
        }
    };

    set_color(!cli.in_place && cli.color.use_if(|| detect_color(atty::Stream::Stdout)));

    match real_main(&cli) {
        Ok(exit) => exit,
        Err(e) => {
            set_color(cli.color.use_if(|| detect_color(atty::Stream::Stderr)));
            e.report()
        }
    }
}

fn real_main(cli: &Cli) -> Result<ExitCode, Error> {
    if let Some(test_file) = &cli.run_tests {
        return Ok(run_tests(std::fs::File::open(test_file)?));
    }

    let (vars, mut ctx): (Vec<String>, Vec<Val>) = binds(cli)?.into_iter().unzip();

    let (vals, filter) = match &cli.filter {
        None => (Vec::new(), Filter::default()),
        Some(filter) => {
            let (path, code) = if cli.from_file {
                (filter.into(), std::fs::read_to_string(filter)?)
            } else {
                ("<inline>".into(), filter.clone().into_string()?)
            };

            parse(&path, &code, &vars, &cli.library_path).map_err(Error::Report)?
        }
    };
    ctx.extend(vals);
    //println!("Filter: {:?}", filter);

    let files = if cli.args { &[] } else { &*cli.posargs };
    let last = if files.is_empty() {
        let inputs = read_buffered(cli, io::stdin().lock());
        with_stdout(|out| run(cli, &filter, ctx, inputs, |v| print(out, cli, &v)))?
    } else {
        let mut last = None;
        for file in files {
            let path = Path::new(file);
            let file =
                load_file(path).map_err(|e| Error::Io(Some(path.display().to_string()), e))?;
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

fn bind<F>(var_val: &mut Vec<(String, Val)>, args: &[OsString], f: F) -> Result<(), Error>
where
    F: Fn(&OsStr) -> Result<Val, Error>,
{
    for arg_val in args.chunks(2) {
        if let [arg, val] = arg_val {
            var_val.push((arg.to_os_string().into_string()?, f(val)?));
        }
    }
    Ok(())
}

fn binds(cli: &Cli) -> Result<Vec<(String, Val)>, Error> {
    let mut var_val = Vec::new();

    bind(&mut var_val, &cli.arg, |v| {
        Ok(Val::Str(v.to_os_string().into_string()?.into()))
    })?;
    bind(&mut var_val, &cli.rawfile, |path| {
        let s = std::fs::read_to_string(path).map_err(|e| Error::Io(Some(format!("{path:?}")), e));
        Ok(Val::Str(s?.into()))
    })?;
    bind(&mut var_val, &cli.slurpfile, |path| {
        json_array(path).map_err(|e| Error::Io(Some(format!("{path:?}")), e))
    })?;

    let positional = if cli.args { &*cli.posargs } else { &[] };
    let positional = positional.iter().cloned();
    let positional = positional.map(|s| Ok(Val::from(s.into_string()?)));
    let positional = positional.collect::<Result<Vec<_>, Error>>()?;

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

fn parse(
    path: &PathBuf,
    code: &str,
    vars: &[String],
    paths: &[PathBuf],
) -> Result<(Vec<Val>, Filter), Vec<FileReports>> {
    use compile::Compiler;
    use load::{import, Arena, File, Loader};

    let paths = if paths.is_empty() {
        &["~/.jq", "$ORIGIN/../lib/jq", "$ORIGIN/../lib"].map(|x| x.into())
    } else {
        paths
    };

    let vars: Vec<_> = vars.iter().map(|v| format!("${v}")).collect();
    let arena = Arena::default();
    let loader = Loader::new(jaq_std::defs().chain(jaq_json::defs())).with_std_read(paths);
    //let loader = Loader::new([]).with_std_read(paths);
    let path = path.into();
    let modules = loader
        .load(&arena, File { path, code })
        .map_err(load_errors)?;

    let mut vals = Vec::new();
    import(&modules, |p| {
        let path = p.find(paths, "json")?;
        vals.push(json_array(path).map_err(|e| e.to_string())?);
        Ok(())
    })
    .map_err(load_errors)?;

    let compiler = Compiler::default()
        .with_funs(jaq_std::funs().chain(jaq_json::funs()))
        .with_global_vars(vars.iter().map(|v| &**v));
    let filter = compiler.compile(modules).map_err(compile_errors)?;
    Ok((vals, filter))
}

fn load_errors(errs: load::Errors<&str, PathBuf>) -> Vec<FileReports> {
    use load::Error;

    let errs = errs.into_iter().map(|(file, err)| {
        let code = file.code;
        let err = match err {
            Error::Io(errs) => errs.into_iter().map(|e| report_io(code, e)).collect(),
            Error::Lex(errs) => errs.into_iter().map(|e| report_lex(code, e)).collect(),
            Error::Parse(errs) => errs.into_iter().map(|e| report_parse(code, e)).collect(),
        };
        (file.map_code(|s| s.into()), err)
    });
    errs.collect()
}

fn compile_errors(errs: compile::Errors<&str, PathBuf>) -> Vec<FileReports> {
    let errs = errs.into_iter().map(|(file, errs)| {
        let code = file.code;
        let errs = errs.into_iter().map(|e| report_compile(code, e)).collect();
        (file.map_code(|s| s.into()), errs)
    });
    errs.collect()
}

/// Try to load file by memory mapping and fall back to regular loading if it fails.
fn load_file(path: impl AsRef<Path>) -> io::Result<Box<dyn core::ops::Deref<Target = [u8]>>> {
    let file = std::fs::File::open(path.as_ref())?;
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

fn json_array(path: impl AsRef<Path>) -> io::Result<Val> {
    json_slice(&load_file(path.as_ref())?).collect()
}

fn read_buffered<'a, R>(cli: &Cli, read: R) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a>
where
    R: BufRead + 'a,
{
    if cli.raw_input {
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::from)))
    } else {
        Box::new(collect_if(cli.slurp, json_read(read)))
    }
}

fn read_slice<'a>(cli: &Cli, slice: &'a [u8]) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a> {
    if cli.raw_input {
        let read = io::BufReader::new(slice);
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::from)))
    } else {
        Box::new(collect_if(cli.slurp, json_slice(slice)))
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

fn collect_if<'a, T: FromIterator<T> + 'a, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> {
    if slurp {
        Box::new(core::iter::once(iter.collect()))
    } else {
        Box::new(iter)
    }
}

type FileReports = (load::File<String, PathBuf>, Vec<Report>);

#[derive(Debug)]
enum Error {
    Io(Option<String>, io::Error),
    Report(Vec<FileReports>),
    Utf8(OsString),
    Parse(String),
    Jaq(jaq_core::Error<Val>),
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
            Self::Report(file_reports) => {
                for (file, reports) in file_reports {
                    let idx = codesnake::LineIndex::new(&file.code);
                    for e in reports {
                        eprintln!("Error: {}", e.message);
                        let block = e.into_block(&idx);
                        eprintln!("{}[{}]", block.prologue(), file.path.display());
                        eprintln!("{}{}", block, block.epilogue())
                    }
                }
                3
            }
            Self::NoOutput => 4,
            Self::Utf8(s) => {
                eprintln!("Error: failed to interpret as UTF-8: {s:?}");
                5
            }
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

/// Conversion of errors from [`OsString::into_string`].
impl From<OsString> for Error {
    fn from(e: OsString) -> Self {
        Self::Utf8(e)
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
            use jaq_core::ValT;
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

type StringColors = Vec<(String, Option<Color>)>;

#[derive(Debug)]
struct Report {
    message: String,
    labels: Vec<(core::ops::Range<usize>, StringColors, Color)>,
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

fn report_io(code: &str, (path, error): (&str, String)) -> Report {
    let path_range = load::span(code, path);
    Report {
        message: format!("could not load file {}: {}", path, error),
        labels: [(path_range, [(error, None)].into(), Color::Red)].into(),
    }
}

fn report_lex(code: &str, (expected, found): load::lex::Error<&str>) -> Report {
    // truncate found string to its first character
    let found = &found[..found.char_indices().nth(1).map_or(found.len(), |(i, _)| i)];

    let found_range = load::span(code, found);
    let found = match found {
        "" => [("unexpected end of input".to_string(), None)].into(),
        c => [("unexpected character ", None), (c, Some(Color::Red))]
            .map(|(s, c)| (s.into(), c))
            .into(),
    };
    let label = (found_range, found, Color::Red);

    let labels = match expected {
        load::lex::Expect::Delim(open) => {
            let text = [("unclosed delimiter ", None), (open, Some(Color::Yellow))]
                .map(|(s, c)| (s.into(), c));
            Vec::from([(load::span(code, open), text.into(), Color::Yellow), label])
        }
        _ => Vec::from([label]),
    };

    Report {
        message: format!("expected {}", expected.as_str()),
        labels,
    }
}

fn report_parse(code: &str, (expected, found): load::parse::Error<&str>) -> Report {
    let found_range = load::span(code, found);

    let found = if found.is_empty() {
        "unexpected end of input"
    } else {
        "unexpected token"
    };
    let found = [(found.to_string(), None)].into();

    Report {
        message: format!("expected {}", expected.as_str()),
        labels: Vec::from([(found_range, found, Color::Red)]),
    }
}

fn report_compile(code: &str, (found, undefined): compile::Error<&str>) -> Report {
    use compile::Undefined::Filter;
    let found_range = load::span(code, found);
    let wnoa = |exp, got| format!("wrong number of arguments (expected {exp}, found {got})");
    let message = match (found, undefined) {
        ("reduce", Filter(arity)) => wnoa("2", arity),
        ("foreach", Filter(arity)) => wnoa("2 or 3", arity),
        (_, undefined) => format!("undefined {}", undefined.as_str()),
    };
    let found = [(message.clone(), None)].into();

    Report {
        message,
        labels: Vec::from([(found_range, found, Color::Red)]),
    }
}

type CodeBlock = codesnake::Block<codesnake::CodeWidth<String>, String>;

impl Report {
    fn into_block(self, idx: &codesnake::LineIndex) -> CodeBlock {
        use codesnake::{Block, CodeWidth, Label};
        let color_maybe = |(text, color): (_, Option<Color>)| match color {
            None => text,
            Some(color) => color.apply(text).to_string(),
        };
        let labels = self.labels.into_iter().map(|(range, text, color)| {
            let text = text.into_iter().map(color_maybe).collect::<Vec<_>>();
            Label::new(range)
                .with_text(text.join(""))
                .with_style(move |s| color.apply(s).to_string())
        });
        Block::new(idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        })
    }
}

fn run_test(test: load::test::Test<String>) -> Result<(Val, Val), Error> {
    let (ctx, filter) = parse(&PathBuf::new(), &test.filter, &[], &[]).map_err(Error::Report)?;

    let inputs = RcIter::new(Box::new(core::iter::empty()));
    let ctx = Ctx::new(ctx, &inputs);

    let json = |s: String| {
        use hifijson::token::Lex;
        hifijson::SliceLexer::new(s.as_bytes())
            .exactly_one(Val::parse)
            .map_err(invalid_data)
    };
    let input = json(test.input)?;
    let expect: Result<Val, _> = test.output.into_iter().map(json).collect();
    let obtain: Result<Val, _> = filter.run((ctx, input)).collect();
    Ok((expect?, obtain.map_err(Error::Jaq)?))
}

fn run_tests(file: std::fs::File) -> ExitCode {
    let lines = io::BufReader::new(file).lines().map(Result::unwrap);
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
