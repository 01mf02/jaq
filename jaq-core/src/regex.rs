//! Helpers to interface with the `regex` crate.

use alloc::string::{String, ToString};
use alloc::{format, rc::Rc, vec::Vec};
use jaq_interpret::{Error, Val};

#[derive(Default)]
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

    pub fn ignore_empty(&self) -> bool {
        self.n
    }

    pub fn global(&self) -> bool {
        self.g
    }

    fn impact<'a>(&'a self, builder: &'a mut regex::RegexBuilder) -> &mut regex::RegexBuilder {
        builder
            .case_insensitive(self.i)
            .multi_line(self.m)
            .dot_matches_new_line(self.s)
            .swap_greed(self.l)
            .ignore_whitespace(self.x)
    }

    pub fn regex(&self, re: &str) -> Result<regex::Regex, regex::Error> {
        let mut builder = regex::RegexBuilder::new(re);
        self.impact(&mut builder).build()
    }
}

/// Mapping between byte and character indices.
pub struct ByteChar<'a> {
    prev_byte: usize,
    prev_char: usize,
    rest: core::str::CharIndices<'a>,
}

impl<'a> ByteChar<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut ci = s.char_indices();
        // skip the first one, because it is already taken into account
        ci.next();
        Self {
            prev_byte: 0,
            prev_char: 0,
            rest: ci,
        }
    }

    /// Convert byte offset to UTF-8 character offset.
    ///
    /// This needs to be called with monotonically increasing values of `byte_offset`.
    fn char_of_byte(&mut self, byte_offset: usize) -> usize {
        assert!(self.prev_byte <= byte_offset);
        if self.prev_byte != byte_offset {
            self.prev_byte = byte_offset;
            self.prev_char += 1 + self.rest.position(|(p, _)| p == byte_offset).unwrap();
        }
        self.prev_char
    }
}

pub struct Match {
    pub offset: usize,
    pub length: usize,
    pub string: String,
    pub name: Option<String>,
}

impl Match {
    pub fn new<'a>(bc: &mut ByteChar, m: regex::Match<'a>, name: Option<&'a str>) -> Self {
        Self {
            offset: bc.char_of_byte(m.start()),
            length: m.as_str().chars().count(),
            string: m.as_str().to_string(),
            name: name.map(|s| s.to_string()),
        }
    }
}

impl From<Match> for Val {
    fn from(m: crate::regex::Match) -> Self {
        let obj = [
            ("offset", Self::Int(m.offset as isize)),
            ("length", Self::Int(m.length as isize)),
            ("string", Self::str(m.string)),
            ("name", m.name.map(Self::str).unwrap_or(Self::Null)),
        ];
        let obj = obj.into_iter().filter(|(_, v)| *v != Self::Null);
        Self::obj(obj.map(|(k, v)| (Rc::new(k.to_string()), v)).collect())
    }
}

/// Apply a regular expression to the given input value.
///
/// `sm` indicates whether to
/// 1. output strings that do *not* match the regex, and
/// 2. output the matches.
pub fn regex(s: &str, re: &str, flags: &str, sm: (bool, bool)) -> Result<Vec<Val>, Error> {
    let fail_flag = |e| Error::str(format!("invalid regex flag: {e}"));
    let fail_re = |e| Error::str(format!("invalid regex: {e}"));
    let flags = Flags::new(flags).map_err(fail_flag)?;
    let re = flags.regex(re).map_err(fail_re)?;
    let (split, matches) = sm;

    let mut last_byte = 0;
    let mut bc = ByteChar::new(s);
    let mut out = Vec::new();

    for c in re.captures_iter(s) {
        let whole = c.get(0).unwrap();
        if whole.start() >= s.len() || (flags.ignore_empty() && whole.as_str().is_empty()) {
            continue;
        }
        let vs = c
            .iter()
            .zip(re.capture_names())
            .filter_map(|(match_, name)| Some(Match::new(&mut bc, match_?, name)))
            .map(Val::from);
        if split {
            out.push(Val::str(s[last_byte..whole.start()].to_string()));
            last_byte = whole.end();
        }
        if matches {
            out.push(Val::arr(vs.collect()));
        }
        if !flags.global() {
            break;
        }
    }
    if split {
        out.push(Val::str(s[last_byte..].to_string()));
    }
    Ok(out)
}
