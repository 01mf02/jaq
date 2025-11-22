use core::fmt::{self, Display, Formatter};

#[derive(Default)]
pub struct Style<S = &'static str> {
    pub red: S,
    pub yellow: S,
    pub bold: S,
    pub reset: S,
}

pub const ANSI: Style = Style {
    red: "\x1b[31m",
    yellow: "\x1b[33m",
    bold: "\x1b[1m",
    reset: "\x1b[0m",
};

impl Style {
    pub fn if_color(self, color: bool) -> Self {
        if color {
            self
        } else {
            Self::default()
        }
    }

    pub fn display<'a, T: Display + 'a>(&'a self, open: &'a str, x: T) -> impl Display + 'a {
        FormatterFn(move |f: &mut Formatter| write!(f, "{open}{x}{}", self.reset))
    }
}

pub struct FormatterFn<F>(pub F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatterFn<F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0(f)
    }
}
