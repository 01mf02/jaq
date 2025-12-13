//! Pretty-printing compilation errors.
use core::fmt::{self, Display, Formatter};
use jaq_core::{compile, load};

/// File and corresponding error reports.
pub type FileReports<P = ()> = (load::File<String, P>, Vec<Report>);

/// Report errors that may occur when loading a module.
pub fn load_errors<P>(errs: load::Errors<&str, P>) -> Vec<FileReports<P>> {
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

/// Report errors that may occur when compiling a module.
pub fn compile_errors<P>(errs: compile::Errors<&str, P>) -> Vec<FileReports<P>> {
    let errs = errs.into_iter().map(|(file, errs)| {
        let code = file.code;
        let errs = errs.into_iter().map(|e| report_compile(code, e)).collect();
        (file.map_code(|s| s.into()), errs)
    });
    errs.collect()
}

type StringColors = Vec<(String, Option<Color>)>;

/// Error report.
#[derive(Debug)]
pub struct Report {
    /// error summary
    pub message: String,
    labels: Vec<(core::ops::Range<usize>, StringColors, Color)>,
}

/// Error color.
#[derive(Copy, Clone, Debug)]
pub enum Color {
    /// used for most errors
    Red = 31,
    /// used for unclosed delimiters
    Yellow = 33,
}

impl Color {
    /// Format a string with ANSI colors.
    pub fn ansi(self, text: impl core::fmt::Display) -> String {
        let ansi = |i| format!("\x1b[{i}m");
        format!("{}{text}{}", ansi(self as usize), ansi(0))
    }
}

fn report_io(code: &str, (path, error): (&str, String)) -> Report {
    let path_range = load::span(code, path);
    Report {
        message: format!("could not load file {path}: {error}"),
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
    /// Convert report to a code block.
    ///
    /// This constructs a line index for its `code` input, which
    /// redoes work if there are multiple error messages for the same file.
    /// However, normally, this should not be a performance bottleneck.
    pub fn to_block(&self, code: &str, paint: fn(Color, String) -> String) -> CodeBlock {
        use codesnake::{Block, CodeWidth, Label, LineIndex};
        let color_maybe = |(text, color): (String, Option<Color>)| match color {
            None => text,
            Some(color) => paint(color, text),
        };
        let labels = self.labels.iter().cloned().map(|(range, text, color)| {
            let text = text.into_iter().map(color_maybe).collect::<Vec<_>>();
            Label::new(range)
                .with_text(text.join(""))
                .with_style(move |s| paint(color, s))
        });
        let idx = LineIndex::new(code);
        Block::new(&idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        })
    }
}

/// Path and corresponding code block for pretty-printing.
pub struct PathBlock<P>(P, CodeBlock);

impl<P> PathBlock<P> {
    /// Construct a new [`PathBlock`].
    pub fn new(path: P, block: CodeBlock) -> Self {
        Self(path, block)
    }
}

impl<P: Display> Display for PathBlock<P> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let PathBlock(path, block) = self;
        writeln!(f, "{}{}", block.prologue(), path)?;
        writeln!(f, "{}{}", block, block.epilogue())
    }
}
