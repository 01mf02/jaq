//! XML support.
use alloc::string::{String, ToString};
use alloc::{borrow::ToOwned, format, vec::Vec};
use core::fmt::{self, Formatter};
use jaq_json::Val;
use xmlparser::{ElementEnd, ExternalId, StrSpan, TextPos, Token, Tokenizer};

/// Parse a stream of root XML values.
pub fn parse_many(s: &str) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let mut tokens = Tokenizer::from(s);
    core::iter::from_fn(move || tokens.next().map(|tk| parse(tk?, &mut tokens)))
}

/// Prefix and local name of a tag.
#[derive(Debug)]
struct Tag<'a>(StrSpan<'a>, StrSpan<'a>);

impl PartialEq for Tag<'_> {
    fn eq(&self, rhs: &Self) -> bool {
        (self.0.as_str(), self.1.as_str()) == (rhs.0.as_str(), rhs.1.as_str())
    }
}

impl fmt::Display for Tag<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if !self.0.is_empty() {
            write!(f, "{}:", self.0)?;
        }
        write!(f, "{}", self.1)
    }
}

impl Tag<'_> {
    fn tag_pos(&self, tokens: &Tokenizer) -> TagPos {
        let pos = tokens.stream().gen_text_pos_from(self.0.start());
        TagPos(self.to_string(), pos)
    }
}

/// Tag and its human-readable position for error reporting.
#[derive(Debug)]
pub struct TagPos(String, TextPos);

impl fmt::Display for TagPos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} (at {})", self.0, self.1)
    }
}

/// Parse error.
#[derive(Debug)]
pub enum Error {
    /// Lex error
    Lex(xmlparser::Error),
    /// Unmatched closing tag, e.g. `<a></b>`
    Unmatched(TagPos, TagPos),
    /// Unclosed tag, e.g. `<a>` or `<a`
    Unclosed(TagPos),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Lex(e) => e.fmt(f),
            Self::Unmatched(open, close) => {
                write!(f, "expected closing tag for {open}, found {close}")
            }
            Self::Unclosed(open) => {
                write!(f, "expected closing tag for {open}, found end of file")
            }
        }
    }
}

impl From<xmlparser::Error> for Error {
    fn from(e: xmlparser::Error) -> Self {
        Self::Lex(e)
    }
}

impl std::error::Error for Error {}

fn parse_children(tag: &Tag, tokens: &mut Tokenizer) -> Result<Vec<Val>, Error> {
    let mut children = Vec::new();
    loop {
        let next = tokens.next();
        match next.ok_or_else(|| Error::Unclosed(tag.tag_pos(tokens)))?? {
            Token::ElementEnd {
                end: ElementEnd::Close(prefix, local),
                ..
            } => {
                let tag_ = Tag(prefix, local);
                if *tag == tag_ {
                    return Ok(children);
                } else {
                    Err(Error::Unmatched(tag.tag_pos(tokens), tag_.tag_pos(tokens)))?
                }
            }
            tk => children.push(parse(tk, tokens)?),
        }
    }
}

fn tac(tag: &Tag, tokens: &mut Tokenizer) -> Result<Val, Error> {
    let mut attrs = Vec::new();
    let children = loop {
        let next = tokens.next();
        match next.ok_or_else(|| Error::Unclosed(tag.tag_pos(tokens)))?? {
            Token::Attribute {
                prefix,
                local,
                value,
                ..
            } => attrs.push((
                Tag(prefix, local).to_string().into(),
                value.as_str().to_owned().into(),
            )),
            Token::ElementEnd { end, .. } => match end {
                ElementEnd::Open => break Some(parse_children(tag, tokens)?),
                ElementEnd::Empty => break None,
                // SAFETY: xmlparser returns an error instead of yielding this
                ElementEnd::Close(..) => panic!(),
            },
            // SAFETY: xmlparser returns an error instead of yielding this
            _ => panic!(),
        }
    };
    let attrs = if attrs.is_empty() { None } else { Some(attrs) };

    Ok(make_obj([
        ("t", Some(tag.to_string().into())),
        ("a", attrs.map(|v| Val::obj(v.into_iter().collect()))),
        ("c", children.map(|v| v.into_iter().collect())),
    ]))
}

fn doctype(name: &str, external: Option<ExternalId>, internal: Option<&str>) -> Val {
    let external = external.map(|ext| match ext {
        ExternalId::System(system) => format!("SYSTEM {system}"),
        ExternalId::Public(pub_id, system) => format!("PUBLIC {pub_id} {system}"),
    });
    make_obj([
        ("name", Some(name.to_owned())),
        ("external", external),
        ("internal", internal.map(|s| s.to_owned())),
    ])
}

fn make_obj<T: Into<Val>, const N: usize>(arr: [(&str, Option<T>); N]) -> Val {
    let iter = arr
        .into_iter()
        .flat_map(|(k, v)| v.map(|v| (k.to_owned().into(), v.into())));
    Val::obj(iter.collect())
}

fn parse(tk: Token, tokens: &mut Tokenizer) -> Result<Val, Error> {
    let ss_val = |ss: StrSpan| ss.as_str().to_owned().into();
    let singleton = |k: &str, v| Val::obj(core::iter::once((k.to_string().into(), v)).collect());

    Ok(match tk {
        Token::Declaration {
            version,
            encoding,
            standalone,
            ..
        } => singleton(
            "xmldecl",
            make_obj([
                ("version", Some(ss_val(version))),
                ("encoding", encoding.map(ss_val)),
                ("standalone", standalone.map(|b| b.into())),
            ]),
        ),
        Token::ProcessingInstruction {
            target, content, ..
        } => singleton(
            "pi",
            make_obj([
                ("target", Some(ss_val(target))),
                ("content", content.map(ss_val)),
            ]),
        ),
        Token::Cdata { text, .. } => singleton("cdata", ss_val(text)),
        Token::Comment { text, .. } => singleton("comment", ss_val(text)),
        Token::ElementStart { prefix, local, .. } => tac(&Tag(prefix, local), tokens)?,
        Token::Text { text } => ss_val(text),
        // SAFETY: xmlparser returns an error instead of yielding this
        Token::Attribute { .. }
        | Token::DtdEnd { .. }
        | Token::ElementEnd { .. }
        | Token::EntityDeclaration { .. } => panic!(),
        Token::DtdStart {
            name,
            external_id,
            span,
        } => {
            let internal = loop {
                let Some(tk) = tokens.next() else {
                    let pos = tokens.stream().gen_text_pos_from(span.start());
                    Err(Error::Unclosed(TagPos("DOCTYPE".into(), pos)))?
                };
                if let Token::DtdEnd { span: span_ } = tk? {
                    break &tokens.stream().span().as_str()[span.end()..span_.start()];
                }
            };
            singleton("doctype", doctype(&name, external_id, Some(internal)))
        }
        Token::EmptyDtd {
            name, external_id, ..
        } => singleton("doctype", doctype(&name, external_id, None)),
    })
}
