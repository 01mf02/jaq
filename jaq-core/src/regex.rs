//! Helpers to interface with the `regex` crate.

use alloc::string::{String, ToString};

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
        Match {
            offset: bc.char_of_byte(m.start()),
            length: m.as_str().chars().count(),
            string: m.as_str().to_string(),
            name: name.map(|s| s.to_string()),
        }
    }
}
