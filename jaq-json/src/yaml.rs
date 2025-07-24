//! YAML parsing.
use crate::{Num, Val};
use alloc::{borrow::Cow, format, string::String, vec::Vec};
use core::fmt::{self, Formatter};
use saphyr_parser::{Event, Input, Parser, ScalarStyle, ScanError, Span, Tag};

/// Lex error.
#[derive(Debug)]
pub struct Lerror(ScanError);

/// Parse error.
#[derive(Debug)]
pub enum Error {
    /// Lex error
    Lex(Lerror),
    /// Scalar value has been encountered with an invalid type, e.g. `!!null 1`
    Scalar(&'static str, String, Span),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Lex(Lerror(e)) => e.fmt(f),
            Self::Scalar(typ, s, span) => {
                let (line, col) = (span.start.line(), span.start.col());
                write!(f, "scalar \"{s}\" is no {typ} ({line}:{col})")
            }
        }
    }
}

impl From<ScanError> for Error {
    fn from(e: ScanError) -> Self {
        Self::Lex(Lerror(e))
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

type EventSpan<'input> = (Event<'input>, Span);

struct State<'input, T: Input> {
    parser: Parser<'input, T>,
    aliases: Vec<Val>,
}

impl<'input, T: Input> State<'input, T> {
    fn new(parser: Parser<'input, T>) -> Self {
        let aliases = Vec::new();
        Self { parser, aliases }
    }

    fn next(&mut self) -> Result<EventSpan<'input>, Error> {
        self.parser.next().unwrap().map_err(Error::from)
    }

    fn push_alias(&mut self, val: Val, anchor_id: usize) {
        self.aliases.push(val);
        assert!(self.aliases.len() == anchor_id);
    }

    fn get_alias(&self, anchor_id: usize) -> Val {
        self.aliases[anchor_id - 1].clone()
    }

    fn parse_doc(&mut self, ev: EventSpan) -> Result<Val, Error> {
        assert!(matches!(ev, (Event::DocumentStart(..), _)));
        let next = self.next()?;
        let v = self.parse_val(next)?;
        assert!(matches!(self.next()?, (Event::DocumentEnd, _)));
        Ok(v)
    }

    fn parse_stream_entry(&mut self) -> Option<Result<Val, Error>> {
        match self.next() {
            Ok((Event::StreamEnd, _)) => None,
            Ok(next) => Some(self.parse_doc(next)),
            Err(e) => Some(Err(e)),
        }
    }

    fn parse_seq_entry(&mut self) -> Option<Result<Val, Error>> {
        match self.next() {
            Ok((Event::SequenceEnd, _)) => None,
            Ok(next) => Some(self.parse_val(next)),
            Err(e) => Some(Err(e)),
        }
    }

    fn parse_map_entry(&mut self) -> Option<Result<(Val, Val), Error>> {
        match self.next() {
            Ok((Event::MappingEnd, _)) => None,
            Ok((next, span)) => Some(self.parse_val((next, span)).and_then(|k| {
                let next = self.next()?;
                Ok((k, self.parse_val(next)?))
            })),
            Err(e) => Some(Err(e)),
        }
    }

    fn parse_val(&mut self, ev: EventSpan) -> Result<Val, Error> {
        let (val, anchor_id) = match ev {
            (Event::Scalar(s, ScalarStyle::Plain, anchor_id, tag), span) => {
                (parse_plain_scalar(s, tag.as_ref(), span)?, anchor_id)
            }
            (Event::Scalar(s, _style, anchor_id, tag), span) => {
                (parse_string_scalar(s, tag.as_ref(), span)?, anchor_id)
            }
            (Event::SequenceStart(anchor_id, _tag), _) => {
                let iter = core::iter::from_fn(|| self.parse_seq_entry());
                (iter.collect::<Result<Val, _>>()?, anchor_id)
            }
            (Event::MappingStart(anchor_id, _tag), _) => {
                let iter = core::iter::from_fn(|| self.parse_map_entry());
                (Val::obj(iter.collect::<Result<_, _>>()?), anchor_id)
            }
            (Event::Alias(anchor_id), _) => (self.get_alias(anchor_id), 0),
            // SAFETY: these should never be returned by saphyr at this point
            (Event::Nothing | Event::SequenceEnd | Event::MappingEnd, _) => panic!(),
            (Event::DocumentStart(..) | Event::DocumentEnd, _) => panic!(),
            (Event::StreamStart | Event::StreamEnd, _) => panic!(),
        };
        if anchor_id > 0 {
            self.push_alias(val.clone(), anchor_id);
        }
        Ok(val)
    }
}

fn parse_sign(s: &str) -> (Option<char>, &str) {
    let mut cs = s.chars();
    match cs.next() {
        Some(sign @ ('+' | '-')) => (Some(sign), cs.as_str()),
        _ => (None, s),
    }
}

/// Return radix and integer part.
fn parse_radix(s: &str) -> Option<(u32, &str)> {
    let mut cs = s.chars();
    Some(match cs.next() {
        Some('0') => match cs.next() {
            Some('x') => (16, cs.as_str()),
            Some('b') => (2, cs.as_str()),
            Some('o') => (8, cs.as_str()),
            Some(_) => return None,
            None => (2, s),
        },
        Some('1'..='9') => (10, s),
        _ => return None,
    })
}

fn parse_int(s: &str) -> Option<Val> {
    let (pos, s) = parse_sign(s);
    let (radix, s) = parse_radix(s)?;
    let n = Num::try_from_int_str(s, radix)?;
    Some(Val::Num(if pos == Some('-') { -n } else { n }))
}

fn strip(s: &str, f: impl FnMut(char) -> bool) -> (&str, &str) {
    let rest = s.trim_start_matches(f);
    (&s[..s.len() - rest.len()], rest)
}

// Watch out, something like "1." is valid YAML, but not valid JSON
fn normalise_float(sign: Option<char>, s: &str) -> Option<String> {
    let mk_sign = |sign| if sign == Some('-') { "-" } else { "" };
    let digits = |s| strip(s, |c| c.is_ascii_digit());
    let sign = mk_sign(sign);

    let (i, s) = digits(s);
    (!i.starts_with('0') || i == "0").then_some(())?;

    let (dot, (f, s)) = s
        .strip_prefix('.')
        .map_or(("", ("", s)), |s| (".", digits(s)));

    // either an integral or fractional part must be there
    (!i.is_empty() || !f.is_empty()).then_some(())?;

    let (exp, e_sign, (e, s)) = s.strip_prefix(['e', 'E']).map_or(("", "", ("", s)), |s| {
        let (e_sign, s) = parse_sign(s);
        ("e", mk_sign(e_sign), digits(s))
    });

    // if an 'e'/'E' is present, an exponent must be present
    (exp.is_empty() || !e.is_empty()).then_some(())?;
    // the string must be fully consumed
    (s.is_empty()).then_some(())?;

    let i = if i.is_empty() { "0" } else { i };
    let f = if f.is_empty() && dot == "." { "0" } else { f };
    Some(format!("{sign}{i}{dot}{f}{exp}{e_sign}{e}"))
}

fn parse_float(s: &str) -> Option<Val> {
    let (sign, rest) = parse_sign(s);
    if matches!(rest, ".inf" | ".Inf" | ".INF") {
        let f = f64::INFINITY;
        Some(Val::from(if sign == Some('-') { -f } else { f }))
    } else {
        normalise_float(sign, rest).map(|f| Val::Num(Num::Dec(f.into())))
    }
}

fn parse_string_scalar(s: Cow<str>, tag: Option<&Cow<Tag>>, span: Span) -> Result<Val, Error> {
    match tag.and_then(|t| t.is_yaml_core_schema().then_some(&*t.suffix)) {
        None | Some("str") => Ok(Val::Str(s.into_owned().into())),
        Some("binary") => parse_plain_scalar(s, tag, span),
        Some(tag) => todo!("invalid tag {tag}"),
    }
}

fn parse_plain_scalar(s: Cow<str>, tag: Option<&Cow<Tag>>, span: Span) -> Result<Val, Error> {
    // if the tag starts with "!!"
    let tag = tag.and_then(|t| t.is_yaml_core_schema().then_some(&*t.suffix));
    let err = |s: Cow<str>, typ| Error::Scalar(typ, s.into_owned(), span);
    Ok(match (&*s, tag) {
        ("null" | "Null" | "NULL" | "~", None | Some("null")) => Val::Null,
        /*
        ("y" | "Y" | "yes" | "Yes" | "YES", None | Some("bool")) => b(true),

        ("n" | "N" | "no" | "No" | "NO", None | Some("bool")) => b(false),
        */
        ("true" | "True" | "TRUE", None | Some("bool")) => Val::Bool(true),
        ("false" | "False" | "FALSE", None | Some("bool")) => Val::Bool(false),
        /*
        ("on" | "On" | "ON", None | Some("bool")) => b(true),
        ("off" | "Off" | "OFF", None | Some("bool")) => b(false),
        */
        (_, Some("null")) => Err(err(s, "null"))?,
        (_, Some("bool")) => Err(err(s, "bool"))?,

        (".nan" | ".NaN" | ".NAN", None | Some("float")) => Val::from(f64::NAN),
        (_, Some("int")) => parse_int(&s).ok_or_else(|| err(s, "int"))?,
        (_, Some("float")) => parse_float(&s).ok_or_else(|| err(s, "float"))?,
        (_, Some("str")) => Val::Str(s.into_owned().into()),
        (_, Some("binary")) => {
            use base64::{engine::general_purpose::STANDARD, Engine};
            let no_ws: String = s.chars().filter(|c| !c.is_whitespace()).collect();
            Val::Bin(STANDARD.decode(no_ws).map_err(|_| err(s, "binary"))?.into())
        }
        // Is it an int? Is it a float? No, it's ... a string!
        (_, None) => parse_int(&s)
            .or_else(|| parse_float(&s))
            .unwrap_or_else(|| Val::Str(s.into_owned().into())),
        (_, Some(tag)) => todo!("invalid tag {tag}"),
    })
}

/// Parse a stream of YAML documents.
pub fn parse_str(s: &str) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let mut st = State::new(Parser::new_from_str(s));
    assert!(matches!(st.next(), Ok((Event::StreamStart, _))));
    core::iter::from_fn(move || st.parse_stream_entry())
}
