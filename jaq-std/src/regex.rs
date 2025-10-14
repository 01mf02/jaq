//! Helpers to interface with the `regex` crate.

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use bstr::ByteSlice;
use regex_automata::{nfa::thompson, util::syntax};
use thompson::pikevm::{PikeVM as Regex, Builder as RegexBuilder};
use thompson::BuildError as Error;

//use regex_automata::bytes::{self as regex, Error, Regex, RegexBuilder};

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
        use syntax::Config;
        builder.syntax(Config::new()
            .case_insensitive(self.i)
            .multi_line(self.m)
            .dot_matches_new_line(self.s)
            .swap_greed(self.l)
            .ignore_whitespace(self.x)
            .utf8(false)
        )
    }

    pub fn regex(self, re: &str) -> Result<Regex, Error> {
        let mut builder = RegexBuilder::new();
        self.impact(&mut builder)
            .thompson(thompson::Config::new().utf8(false))
.build(re)
    }
}

type CharIndices<'a> =
    core::iter::Chain<bstr::CharIndices<'a>, core::iter::Once<(usize, usize, char)>>;

/// Mapping between byte and character indices.
pub struct ByteChar<'a>(core::iter::Peekable<core::iter::Enumerate<CharIndices<'a>>>);

impl<'a> ByteChar<'a> {
    pub fn new(s: &'a [u8]) -> Self {
        let last = core::iter::once((s.len(), 0, '\0'));
        Self(s.char_indices().chain(last).enumerate().peekable())
    }

    /// Convert byte offset to UTF-8 character offset.
    ///
    /// This needs to be called with monotonically increasing values of `byte_offset`.
    fn char_of_byte(&mut self, byte_offset: usize) -> Option<usize> {
        loop {
            let (char_i, (byte_i, _, _char)) = self.0.peek()?;
            if byte_offset == *byte_i {
                return Some(*char_i);
            } else {
                self.0.next();
            }
        }
    }
}

pub struct Match<B, S> {
    pub offset: usize,
    pub length: usize,
    pub string: B,
    pub name: Option<S>,
}

impl<'a> Match<&'a [u8], String> {
    pub fn new(s: &'a [u8], bc: &mut ByteChar, m: regex_automata::Span, name: Option<String>) -> Self {
        let string = &s[m.start..m.end];
        Self {
            offset: bc.char_of_byte(m.start).unwrap(),
            length: string.chars().count(),
            string,
            name,
        }
    }

    pub fn fields<T: From<isize> + From<String> + 'a>(
        &self,
        f: impl Fn(&'a [u8]) -> T,
    ) -> impl Iterator<Item = (T, T)> + '_ {
        [
            ("offset", (self.offset as isize).into()),
            ("length", (self.length as isize).into()),
            ("string", f(self.string)),
        ]
        .into_iter()
        .chain(self.name.iter().map(|n| ("name", (*n).to_string().into())))
        .map(|(k, v)| (k.to_string().into(), v))
    }
}

pub enum Part<B, S> {
    Matches(Vec<Match<B, S>>),
    Mismatch(B),
}

/// Apply a regular expression to the given input value.
///
/// `sm` indicates whether to
/// 1. output strings that do *not* match the regex, and
/// 2. output the matches.
pub fn regex<'a>(
    s: &'a [u8],
    re: &'a Regex,
    flags: Flags,
    sm: (bool, bool),
) -> Vec<Part<&'a [u8], String>> {
    // mismatches & matches
    let (mi, ma) = sm;

    let mut last_byte = 0;
    let mut bc = ByteChar::new(s);
    let mut out = Vec::new();
    let mut cache = re.create_cache();

    for c in re.captures_iter(&mut cache, s) {
        let whole = c.get_match().unwrap();
        if flags.ignore_empty() && whole.is_empty() {
            continue;
        }
        //let match_names = c.iter().zip(re.capture_names());
        use regex_automata::PatternID;
        let names = c.group_info().pattern_names(PatternID::ZERO).map(|o| o.map(|s| s.to_string()));
        let match_names = c.iter().zip(names);
        let matches = match_names.filter_map(|(m, n)| Some(Match::new(s, &mut bc, m?, n)));
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
