//! XML parsing.
use crate::{Map, Val};
use alloc::string::{String, ToString};
use alloc::{borrow::ToOwned, boxed::Box, format, vec::Vec};
use bstr::BStr;
use bytes::Bytes;
use core::fmt::{self, Formatter};
use xmlparser::{ElementEnd, ExternalId, StrSpan, TextPos, Token, Tokenizer};

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

/// Lex error.
#[derive(Debug)]
pub struct Lerror(xmlparser::Error);

/// Parse error.
#[derive(Debug)]
pub enum Error {
    /// Lex error
    Lex(Lerror),
    /// Unmatched closing tag, e.g. `<a></b>`
    Unmatched(TagPos, TagPos),
    /// Unclosed tag, e.g. `<a>`
    Unclosed(TagPos),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Lex(Lerror(e)) => e.fmt(f),
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
        Self::Lex(Lerror(e))
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

fn parse_children(tag: &Tag, tokens: &mut Tokenizer) -> Result<Vec<Val>, Error> {
    let mut children = Vec::new();
    loop {
        let Some(tk) = tokens.next() else {
            return Err(Error::Unclosed(tag.tag_pos(tokens)));
        };
        match tk? {
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
        // SAFETY: xmlparser returns an error instead of None
        let tk = tokens.next().unwrap();
        match tk? {
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

/// Parse a stream of root XML values.
pub fn parse_str(s: &str) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let mut tokens = Tokenizer::from(s);
    core::iter::from_fn(move || tokens.next().map(|tk| parse(tk?, &mut tokens)))
}

/// Serialisation error.
#[derive(Debug)]
pub enum Serror {
    /// Unknown key with value was found in an object, e.g. `{t: "a", x: 1}`
    InvalidEntry(&'static str, Val, Val),
    /// Object with zero or more than one keys found, e.g. `{}`, `{a: 1, b: 2}`
    SingletonObj(Val),
}

impl fmt::Display for Serror {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::InvalidEntry(o, k, v) => {
                write!(f, "invalid entry in {o} object: {{\"{k}\": {v}}}")
            }
            Self::SingletonObj(v) => write!(f, "expected singleton object, found: {v}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Serror {}

/// XML value.
pub enum XmlVal<S = Bytes> {
    /// XML declaration, e.g. `<?xml version='1.0' encoding='UTF-8' standalone='yes'?>`
    XmlDecl(Vec<(S, S)>),
    /// DOCTYPE directive, e.g. `<!DOCTYPE greeting SYSTEM "hello.dtd" [...]>`
    DocType {
        /// name of the document type, e.g. "greeting"
        name: S,
        /// reference to an external file, e.g. `SYSTEM "hello.dtd"`
        external: Option<S>,
        /// internal definition of the DTD, e.g. `...`
        internal: Option<S>,
    },
    /// Processing instruction, e.g. <?xml-stylesheet type="text/css" href="style.css"?>`
    Pi {
        /// target, e.g. `xml-stylesheet`
        target: S,
        /// content, e.g. `type="text/css" href="style.css"`
        content: Option<S>,
    },
    /// An element consisting of a Tag, an Attribute, and Content
    ///
    /// For example, `<a href="bla">Link</a>`.
    Tac(S, Vec<(S, S)>, Option<Box<Self>>),
    /// A sequence of XML values, e.g. `Hello<br />World`
    Seq(Vec<Self>),
    /// A string, e.g. `Hello world`
    Str(S),
    /// CDATA, e.g. `<![CDATA[text]]>`
    Cdata(S),
    /// Comment, e.g. `<!-- text -->`
    Comment(S),
}

impl TryFrom<&Val> for XmlVal {
    type Error = Serror;
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        use jaq_std::ValT;
        let from_kv = |(k, v): (&_, &_)| match (k, v) {
            (Val::Str2(k, _), Val::Str2(v, _)) => Ok((k.clone(), v.clone())),
            _ => Err(Serror::InvalidEntry("attribute", k.clone(), v.clone())),
        };
        let from_kvs = |a: &Map| a.iter().map(from_kv).collect::<Result<_, _>>();

        let from_tac = |o: &Map| {
            let mut t = Bytes::default();
            let mut a = Vec::new();
            let mut c = None;
            for (k, v) in o.iter() {
                let fail = || Serror::InvalidEntry("tac", k.clone(), v.clone());
                let k = k.as_utf8_str().ok_or_else(fail)?;
                match (k, v) {
                    (b"t", Val::Str2(s, _)) => t = s.clone(),
                    (b"a", Val::Obj(attrs)) => a = from_kvs(attrs)?,
                    (b"c", v) => c = Some(Box::new(v.try_into()?)),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Tac(t.clone(), a, c))
        };
        let from_dt = |o: &Map| {
            let mut name = Bytes::default();
            let mut external = None;
            let mut internal = None;
            for (k, v) in o.iter() {
                let fail = || Serror::InvalidEntry("doctype", k.clone(), v.clone());
                let k = k.as_utf8_str().ok_or_else(fail)?;
                match (k, v) {
                    (b"name", Val::Str2(s, _)) => name = s.clone(),
                    (b"external", Val::Str2(s, _)) => external = Some(s.clone()),
                    (b"internal", Val::Str2(s, _)) => internal = Some(s.clone()),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::DocType {
                name,
                external,
                internal,
            })
        };
        let from_pi = |o: &Map| {
            let mut target = Bytes::default();
            let mut content = None;
            for (k, v) in o.iter() {
                let fail = || Serror::InvalidEntry("pi", k.clone(), v.clone());
                let k = k.as_utf8_str().ok_or_else(fail)?;
                match (k, v) {
                    (b"target", Val::Str2(s, _)) => target = s.clone(),
                    (b"content", Val::Str2(s, _)) => content = Some(s.clone()),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Pi { target, content })
        };
        let contains_key = |o: &Map, k: &str| o.contains_key(&Val::from(k.to_string()));
        match v {
            Val::Str2(s, _) => Ok(Self::Str(s.clone())),
            Val::Arr(a) => a
                .iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()
                .map(Self::Seq),
            Val::Obj(o) if contains_key(o, "t") => from_tac(o),
            Val::Obj(o) => {
                let mut o = o.iter();
                let (k, v) = match (o.next(), o.next()) {
                    (Some(kv), None) => kv,
                    _ => Err(Serror::SingletonObj(v.clone()))?,
                };
                let fail = || Serror::InvalidEntry("unknown", k.clone(), v.clone());
                let k = k.as_utf8_str().ok_or_else(fail)?;
                match (k, v) {
                    (b"xmldecl", Val::Obj(kvs)) => from_kvs(kvs).map(Self::XmlDecl),
                    (b"doctype", Val::Obj(o)) if contains_key(o, "name") => from_dt(o),
                    (b"cdata", Val::Str2(s, _)) => Ok(Self::Cdata(s.clone())),
                    (b"comment", Val::Str2(s, _)) => Ok(Self::Comment(s.clone())),
                    (b"pi", Val::Obj(o)) if contains_key(o, "target") => from_pi(o),
                    _ => Err(fail())?,
                }
            }
            Val::Null | Val::Bool(_) | Val::Num(_) => Ok(Self::Str(v.to_string().into())),
        }
    }
}

impl fmt::Display for XmlVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let write_kvs = |f: &mut Formatter, a: &Vec<_>| {
            a.iter()
                .try_for_each(|(k, v)| write!(f, " {}=\"{}\"", BStr::new(k), BStr::new(v)))
        };
        match self {
            Self::Str(s) => write!(f, "{}", BStr::new(s)),
            Self::Seq(a) => a.iter().try_for_each(|v| v.fmt(f)),
            Self::Tac(t, a, c) => {
                write!(f, "<{}", BStr::new(t))?;
                write_kvs(f, a)?;
                if let Some(c) = c {
                    write!(f, ">{c}</{}>", BStr::new(t))
                } else {
                    write!(f, "/>")
                }
            }
            Self::XmlDecl(a) => {
                write!(f, "<?xml")?;
                write_kvs(f, a)?;
                write!(f, ">")
            }
            Self::DocType {
                name,
                external,
                internal,
            } => {
                write!(f, "<!DOCTYPE {}", BStr::new(name))?;
                if let Some(s) = external {
                    write!(f, " {}", BStr::new(s))?;
                }
                if let Some(s) = internal {
                    write!(f, " [{}]", BStr::new(s))?;
                }
                write!(f, ">")
            }
            Self::Cdata(s) => write!(f, "<![CDATA[{}]]>", BStr::new(s)),
            Self::Comment(s) => write!(f, "<!--{}-->", BStr::new(s)),
            Self::Pi { target, content } => {
                write!(f, "<?{}", BStr::new(target))?;
                if let Some(s) = content {
                    write!(f, " {}", BStr::new(s))?;
                }
                write!(f, ">")
            }
        }
    }
}
