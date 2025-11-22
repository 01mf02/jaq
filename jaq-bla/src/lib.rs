use jaq_core::{compile, load};

pub type FileReports<P> = (load::File<String, P>, Vec<Report>);

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

pub fn compile_errors<P>(errs: compile::Errors<&str, P>) -> Vec<FileReports<P>> {
    let errs = errs.into_iter().map(|(file, errs)| {
        let code = file.code;
        let errs = errs.into_iter().map(|e| report_compile(code, e)).collect();
        (file.map_code(|s| s.into()), errs)
    });
    errs.collect()
}

type StringColors = Vec<(String, Option<Color>)>;

#[derive(Debug)]
pub struct Report {
    pub message: String,
    labels: Vec<(core::ops::Range<usize>, StringColors, Color)>,
}

#[derive(Copy, Clone, Debug)]
pub enum Color {
    Yellow,
    Red,
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
    pub fn to_block(
        &self,
        idx: &codesnake::LineIndex,
        paint: fn(Color, String) -> String,
    ) -> CodeBlock {
        use codesnake::{Block, CodeWidth, Label};
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
        Block::new(idx, labels).unwrap().map_code(|c| {
            let c = c.replace('\t', "    ");
            let w = unicode_width::UnicodeWidthStr::width(&*c);
            CodeWidth::new(c, core::cmp::max(w, 1))
        })
    }
}
