//! Helpers to interface with the `regex` crate.

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use regex_lite::{self as regex, Error, Regex, RegexBuilder};

#[derive(Copy, Clone, Default)]
pub struct Flags {
    // global search
    g: bool,
    // ignore empty matches
    n: bool,
    // case-insensitive
    i: bool,
    // multi-line mode: ^ and $ match begin/end of line
    m: bool,
    // single-line mode: allow . to match \n
    s: bool,
    // greedy
    l: bool,
    // extended mode: ignore whitespace and allow line comments (starting with `#`)
    x: bool,
}

impl Flags {
    pub fn new(flags: &str) -> Result<Self, char> {
        let mut out = Self::default();
        for flag in flags.chars() {
            match flag {
                'g' => out.g = true,
                'n' => out.n = true,
                'i' => out.i = true,
                'm' => out.m = true,
                's' => out.s = true,
                'l' => out.l = true,
                'x' => out.x = true,
                'p' => {
                    out.m = true;
                    out.s = true;
                }
                c => return Err(c),
            }
        }
        Ok(out)
    }

    pub fn ignore_empty(self) -> bool {
        self.n
    }

    pub fn global(self) -> bool {
        self.g
    }

    fn impact(self, builder: &mut RegexBuilder) -> &mut RegexBuilder {
        builder
            .case_insensitive(self.i)
            .multi_line(self.m)
            .dot_matches_new_line(self.s)
            .swap_greed(self.l)
            .ignore_whitespace(self.x)
    }

    pub fn regex(self, re: &str) -> Result<Regex, Error> {
        let mut builder = RegexBuilder::new(re);
        self.impact(&mut builder).build()
    }
}

type CharIndices<'a> =
    core::iter::Chain<core::str::CharIndices<'a>, core::iter::Once<(usize, char)>>;

/// Mapping between byte and character indices.
pub struct ByteChar<'a>(core::iter::Peekable<core::iter::Enumerate<CharIndices<'a>>>);

impl<'a> ByteChar<'a> {
    pub fn new(s: &'a str) -> Self {
        let last = core::iter::once((s.len(), '\0'));
        Self(s.char_indices().chain(last).enumerate().peekable())
    }

    /// Convert byte offset to UTF-8 character offset.
    ///
    /// This needs to be called with monotonically increasing values of `byte_offset`.
    fn char_of_byte(&mut self, byte_offset: usize) -> Option<usize> {
        loop {
            let (char_i, (byte_i, _char)) = self.0.peek()?;
            if byte_offset == *byte_i {
                return Some(*char_i);
            } else {
                self.0.next();
            }
        }
    }
}

pub struct Match<S> {
    pub offset: usize,
    pub length: usize,
    pub string: S,
    pub name: Option<S>,
}

impl<'a> Match<&'a str> {
    pub fn new(bc: &mut ByteChar, m: regex::Match<'a>, name: Option<&'a str>) -> Self {
        Self {
            offset: bc.char_of_byte(m.start()).unwrap(),
            length: m.as_str().chars().count(),
            string: m.as_str(),
            name,
        }
    }

    pub fn fields<T: From<isize> + From<String> + 'a>(&self) -> impl Iterator<Item = (T, T)> + '_ {
        [
            ("offset", (self.offset as isize).into()),
            ("length", (self.length as isize).into()),
            ("string", self.string.to_string().into()),
        ]
        .into_iter()
        .chain(self.name.iter().map(|n| ("name", (*n).to_string().into())))
        .map(|(k, v)| (k.to_string().into(), v))
    }
}

pub enum Part<S> {
    Matches(Vec<Match<S>>),
    Mismatch(S),
}

/// Apply a regular expression to the given input value.
///
/// `sm` indicates whether to
/// 1. output strings that do *not* match the regex, and
/// 2. output the matches.
pub fn regex<'a>(s: &'a str, re: &'a Regex, flags: Flags, sm: (bool, bool)) -> Vec<Part<&'a str>> {
    // mismatches & matches
    let (mi, ma) = sm;

    let mut last_byte = 0;
    let mut bc = ByteChar::new(s);
    let mut out = Vec::new();

    for c in re.captures_iter(s) {
        let whole = c.get(0).unwrap();
        if flags.ignore_empty() && whole.as_str().is_empty() {
            continue;
        }
        let match_names = c.iter().zip(re.capture_names());
        let matches = match_names.filter_map(|(m, n)| Some(Match::new(&mut bc, m?, n)));
        if mi {
            out.push(Part::Mismatch(&s[last_byte..whole.start()]));
            last_byte = whole.end();
        }
        if ma {
            out.push(Part::Matches(matches.collect()));
        }
        if !flags.global() {
            break;
        }
    }
    if mi {
        out.push(Part::Mismatch(&s[last_byte..]));
    }
    out
}
