//! Command-line argument parsing
use core::fmt;
use std::env::ArgsOs;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

#[derive(Copy, Clone, Debug)]
pub enum Format {
    /// When the option `--slurp` is used additionally,
    /// then the whole input is read into a single string.
    Raw,
    Json,
    Cbor,
    Toml,
    Xml,
    Yaml,
}

const FMTS: &str = "raw, json, cbor, toml, xml, yaml";

impl Format {
    /// Determine a file format from a path.
    pub fn determine(path: &Path) -> Option<Self> {
        match path.extension()?.to_str()? {
            "cbor" => Some(Format::Cbor),
            "toml" => Some(Format::Toml),
            "xml" | "xhtml" => Some(Format::Xml),
            "yml" | "yaml" => Some(Format::Yaml),
            "json" => Some(Format::Json),
            _ => None,
        }
    }

    /// Parse a format name.
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "cbor" => Some(Format::Cbor),
            "raw" => Some(Format::Raw),
            "json" => Some(Format::Json),
            "toml" => Some(Format::Toml),
            "xml" => Some(Format::Xml),
            "yaml" => Some(Format::Yaml),
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct Cli {
    // Input options
    pub from: Option<Format>,
    pub null_input: bool,
    /// When input is read from files,
    /// jaq yields an array for each file, whereas
    /// jq produces only a single array.
    pub slurp: bool,

    // Output options
    pub to: Option<Format>,
    pub compact_output: bool,
    /// This flag enables `--raw-output`.
    pub join_output: bool,
    pub in_place: bool,
    pub sort_keys: bool,
    pub color_output: bool,
    pub monochrome_output: bool,
    pub tab: bool,
    pub indent: Option<usize>,

    // Compilation options
    pub from_file: bool,
    /// If this option is given multiple times, all given directories are searched.
    pub library_path: Vec<PathBuf>,

    // Key-value options
    pub arg: Vec<(String, String)>,
    pub argjson: Vec<(String, String)>,
    pub slurpfile: Vec<(String, OsString)>,
    pub rawfile: Vec<(String, OsString)>,

    // Positional arguments
    /// If this argument is not given, it is assumed to be `.`, the identity filter.
    pub filter: Option<Filter>,
    pub files: Vec<PathBuf>,
    pub args: Vec<String>,
    //pub jsonargs: Vec<String>,
    pub run_tests: Option<Vec<PathBuf>>,
    /// If there is some last output value `v`,
    /// then the exit status code is
    /// 1 if `v < true` (that is, if `v` is `false` or `null`) and
    /// 0 otherwise.
    /// If there is no output value, then the exit status code is 4.
    ///
    /// If any error occurs, then this option has no effect.
    pub exit_status: bool,
    pub version: bool,
    pub help: bool,

    color_stdout: std::cell::OnceCell<bool>,
}

#[derive(Debug)]
pub enum Filter {
    Inline(String),
    FromFile(PathBuf),
}

impl Cli {
    fn positional(&mut self, mode: &Mode, arg: OsString) -> Result<(), Error> {
        if self.filter.is_none() {
            self.filter = Some(if self.from_file {
                Filter::FromFile(arg.into())
            } else {
                Filter::Inline(arg.into_string()?)
            })
        } else {
            match mode {
                Mode::Files => self.files.push(arg.into()),
                Mode::Args => self.args.push(arg.into_string()?),
                //Mode::JsonArgs => self.jsonargs.push(arg.into_string()?),
            }
        }
        Ok(())
    }

    fn long(&mut self, mode: &mut Mode, arg: &str, args: &mut ArgsOs) -> Result<(), Error> {
        let int = |s: OsString| s.into_string().ok()?.parse().ok();
        match arg {
            // handle all arguments after "--"
            "" => args.try_for_each(|arg| self.positional(mode, arg))?,

            "from" => self.from = Some(parse_format("--from", args)?),
            "null-input" => self.short('n', args)?,
            "raw-input" => self.short('R', args)?,
            "slurp" => self.short('s', args)?,

            "to" => self.to = Some(parse_format("--to", args)?),
            "compact-output" => self.short('c', args)?,
            "raw-output" => self.short('r', args)?,
            "join-output" => self.short('j', args)?,
            "in-place" => self.short('i', args)?,
            "sort-keys" => self.short('S', args)?,
            "color-output" => self.short('C', args)?,
            "monochrome-output" => self.short('M', args)?,
            "tab" => self.tab = true,
            "indent" => {
                self.indent = Some(args.next().and_then(int).ok_or(Error::Int("--indent"))?)
            }
            "from-file" => self.short('f', args)?,
            "library-path" => self.short('L', args)?,
            "arg" => {
                let (name, value) = parse_key_val("--arg", args)?;
                self.arg.push((name, value.into_string()?));
            }
            "argjson" => {
                let (name, value) = parse_key_val("--argjson", args)?;
                self.argjson.push((name, value.into_string()?));
            }
            "slurpfile" => self.slurpfile.push(parse_key_val("--slurpfile", args)?),
            "rawfile" => self.rawfile.push(parse_key_val("--rawfile", args)?),

            "args" => *mode = Mode::Args,
            //"jsonargs" => *mode = Mode::JsonArgs,
            "run-tests" => self.run_tests = Some(args.map(PathBuf::from).collect()),
            "exit-status" => self.short('e', args)?,
            "version" => self.short('V', args)?,
            "help" => self.short('h', args)?,

            arg => Err(Error::Flag(format!("--{arg}")))?,
        }
        Ok(())
    }

    fn short(&mut self, arg: char, args: &mut ArgsOs) -> Result<(), Error> {
        match arg {
            'R' => self.from = Some(Format::Raw),
            'n' => self.null_input = true,
            's' => self.slurp = true,

            'r' => self.to = Some(Format::Raw),
            'c' => self.compact_output = true,
            'j' => {
                self.join_output = true;
                self.short('r', args)?
            }
            'i' => self.in_place = true,
            'S' => self.sort_keys = true,
            'C' => self.color_output = true,
            'M' => self.monochrome_output = true,

            'f' => self.from_file = true,
            'L' => self
                .library_path
                .push(args.next().ok_or(Error::Path("-L"))?.into()),
            'e' => self.exit_status = true,
            'V' => self.version = true,
            'h' => self.help = true,
            arg => Err(Error::Flag(format!("-{arg}")))?,
        }
        Ok(())
    }

    pub fn parse() -> Result<Self, Error> {
        let mut cli = Self::default();
        let mut mode = Mode::Files;
        let mut args = std::env::args_os();
        args.next();
        while let Some(arg) = args.next() {
            match arg.to_str() {
                // we've got a valid UTF-8 argument here
                Some(s) => match s.strip_prefix("--") {
                    Some(rest) => cli.long(&mut mode, rest, &mut args)?,
                    None => match s.strip_prefix("-") {
                        Some(rest) => rest.chars().try_for_each(|c| cli.short(c, &mut args))?,
                        None => cli.positional(&mode, arg)?,
                    },
                },
                // we've got invalid UTF-8, so it is no valid flag
                // note that we do not check here whether arg starts with `-`,
                // because this seems to be quite difficult to do in a portable way
                None => cli.positional(&mode, arg)?,
            }
        }
        Ok(cli)
    }

    pub fn color_if(&self, f: impl Fn() -> bool) -> bool {
        if self.monochrome_output {
            false
        } else if self.color_output {
            true
        } else {
            f()
        }
    }

    pub fn color_stdio(&self, io: impl std::io::IsTerminal) -> bool {
        let no_color = || std::env::var("NO_COLOR").is_ok_and(|v| !v.is_empty());
        self.color_if(|| io.is_terminal() && !no_color())
    }

    pub fn color_stdout(&self) -> bool {
        let init = || self.color_stdio(std::io::stdout());
        *self.color_stdout.get_or_init(init)
    }

    pub fn indent(&self) -> usize {
        self.indent.unwrap_or(2)
    }
}

#[derive(Debug)]
pub enum Error {
    Flag(String),
    Utf8(OsString),
    KeyValue(&'static str),
    Int(&'static str),
    Path(&'static str),
    Format(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Flag(s) => write!(f, "unknown flag: {s}"),
            Self::Utf8(s) => write!(f, "invalid UTF-8: {s:?}"),
            Self::KeyValue(o) => write!(f, "{o} expects a key and a value"),
            Self::Int(o) => write!(f, "{o} expects an integer"),
            Self::Path(o) => write!(f, "{o} expects a path"),
            Self::Format(o) => write!(f, "{o} expects a data format (possible values: {FMTS})"),
        }
    }
}

/// Conversion of errors from [`OsString::into_string`].
impl From<OsString> for Error {
    fn from(e: OsString) -> Self {
        Self::Utf8(e)
    }
}

fn parse_format(arg: &'static str, args: &mut ArgsOs) -> Result<Format, Error> {
    let err = || Error::Format(arg);
    let fmt = args.next().and_then(|a| a.into_string().ok());
    Format::from_str(&fmt.ok_or_else(err)?).ok_or_else(err)
}

fn parse_key_val(arg: &'static str, args: &mut ArgsOs) -> Result<(String, OsString), Error> {
    let err = || Error::KeyValue(arg);
    let key = args.next().ok_or_else(err)?.into_string()?;
    let val = args.next().ok_or_else(err)?;
    Ok((key, val))
}

/// Interpretation of positional arguments.
enum Mode {
    Args,
    //JsonArgs,
    Files,
}
