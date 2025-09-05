//! XML support.
use crate::{bstr, Map, Val};
use alloc::string::{String, ToString};
use alloc::{borrow::ToOwned, boxed::Box, format, vec::Vec};
use core::fmt::{self, Display, Formatter};
use xmlparser::{ElementEnd, ExternalId, StrSpan, TextPos, Token, Tokenizer};

/// Parse a stream of root XML values.
pub fn parse_many(s: &str) -> impl Iterator<Item = Result<Val, PError>> + '_ {
    let mut tokens = Tokenizer::from(s);
    core::iter::from_fn(move || tokens.next().map(|tk| parse(tk?, &mut tokens)))
}

/// Parse a stream of root XML values and collect it into an array.
pub(crate) fn parse_collect(s: &str) -> Result<Val, PError> {
    parse_many(s).collect()
}

/// Serialise a value to an XML value.
pub fn serialise<'a>(v: &'a Val) -> Result<impl Display + 'a, impl Display> {
    XmlVal::try_from(v)
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

/// Lex error.
#[derive(Debug)]
pub struct LError(xmlparser::Error);

/// Parse error.
#[derive(Debug)]
pub enum PError {
    /// Lex error
    Lex(LError),
    /// Unmatched closing tag, e.g. `<a></b>`
    Unmatched(TagPos, TagPos),
    /// Unclosed tag, e.g. `<a>`
    Unclosed(TagPos),
}

impl fmt::Display for PError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Lex(LError(e)) => e.fmt(f),
            Self::Unmatched(open, close) => {
                write!(f, "expected closing tag for {open}, found {close}")
            }
            Self::Unclosed(open) => {
                write!(f, "expected closing tag for {open}, found end of file")
            }
        }
    }
}

impl From<xmlparser::Error> for PError {
    fn from(e: xmlparser::Error) -> Self {
        Self::Lex(LError(e))
    }
}

#[cfg(feature = "std")]
impl std::error::Error for PError {}

fn parse_children(tag: &Tag, tokens: &mut Tokenizer) -> Result<Vec<Val>, PError> {
    let mut children = Vec::new();
    loop {
        let Some(tk) = tokens.next() else {
            return Err(PError::Unclosed(tag.tag_pos(tokens)));
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
                    Err(PError::Unmatched(tag.tag_pos(tokens), tag_.tag_pos(tokens)))?
                }
            }
            tk => children.push(parse(tk, tokens)?),
        }
    }
}

fn tac(tag: &Tag, tokens: &mut Tokenizer) -> Result<Val, PError> {
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

fn parse(tk: Token, tokens: &mut Tokenizer) -> Result<Val, PError> {
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
                    Err(PError::Unclosed(TagPos("DOCTYPE".into(), pos)))?
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

/// Serialisation error.
#[derive(Debug)]
enum SError {
    /// Unknown key with value was found in an object, e.g. `{t: "a", x: 1}`
    InvalidEntry(&'static str, Val, Val),
    /// Object with zero or more than one keys found, e.g. `{}`, `{a: 1, b: 2}`
    SingletonObj(Val),
}

impl fmt::Display for SError {
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
impl std::error::Error for SError {}

/// XML value.
enum XmlVal<S> {
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
    Scalar(Val),
    /// CDATA, e.g. `<![CDATA[text]]>`
    Cdata(S),
    /// Comment, e.g. `<!-- text -->`
    Comment(S),
}

impl<'a> TryFrom<&'a Val> for XmlVal<&'a [u8]> {
    type Error = SError;
    fn try_from(v: &'a Val) -> Result<Self, Self::Error> {
        use jaq_std::ValT;
        let from_kv = |(k, v): (&'a _, &'a _)| match (k, v) {
            (Val::Str(k, _), Val::Str(v, _)) => Ok((&**k, &**v)),
            _ => Err(SError::InvalidEntry("attribute", k.clone(), v.clone())),
        };
        let from_kvs = |a: &'a Map| a.iter().map(from_kv).collect::<Result<_, _>>();

        let from_tac = |o: &'a Map| {
            let mut t = &b""[..];
            let mut a = Vec::new();
            let mut c = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("tac", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"t", Val::Str(s, _)) => t = s,
                    (b"a", Val::Obj(attrs)) => a = from_kvs(attrs)?,
                    (b"c", v) => c = Some(Box::new(v.try_into()?)),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Tac(t, a, c))
        };
        let from_dt = |o: &'a Map| {
            let mut name = &b""[..];
            let mut external = None;
            let mut internal = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("doctype", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"name", Val::Str(s, _)) => name = s,
                    (b"external", Val::Str(s, _)) => external = Some(&**s),
                    (b"internal", Val::Str(s, _)) => internal = Some(&**s),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::DocType {
                name,
                external,
                internal,
            })
        };
        let from_pi = |o: &'a Map| {
            let mut target = &b""[..];
            let mut content = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("pi", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"target", Val::Str(s, _)) => target = s,
                    (b"content", Val::Str(s, _)) => content = Some(&**s),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Pi { target, content })
        };
        let contains_key = |o: &Map, k: &str| o.contains_key(&Val::from(k.to_string()));
        match v {
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
                    _ => Err(SError::SingletonObj(v.clone()))?,
                };
                let fail = || SError::InvalidEntry("unknown", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"xmldecl", Val::Obj(kvs)) => from_kvs(kvs).map(Self::XmlDecl),
                    (b"doctype", Val::Obj(o)) if contains_key(o, "name") => from_dt(o),
                    (b"cdata", Val::Str(s, _)) => Ok(Self::Cdata(s)),
                    (b"comment", Val::Str(s, _)) => Ok(Self::Comment(s)),
                    (b"pi", Val::Obj(o)) if contains_key(o, "target") => from_pi(o),
                    _ => Err(fail())?,
                }
            }
            Val::Null | Val::Bool(_) | Val::Num(_) | Val::Str(..) => Ok(Self::Scalar(v.clone())),
        }
    }
}

impl fmt::Display for XmlVal<&[u8]> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let write_kvs = |f: &mut Formatter, a: &Vec<_>| {
            a.iter()
                .try_for_each(|(k, v)| write!(f, " {}=\"{}\"", bstr(k), bstr(v)))
        };
        match self {
            Self::Scalar(Val::Str(s, _)) => write!(f, "{}", bstr(s)),
            Self::Scalar(v) => write!(f, "{v}"),
            Self::Seq(a) => a.iter().try_for_each(|v| v.fmt(f)),
            Self::Tac(t, a, c) => {
                write!(f, "<{}", bstr(t))?;
                write_kvs(f, a)?;
                if let Some(c) = c {
                    write!(f, ">{c}</{}>", bstr(t))
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
                write!(f, "<!DOCTYPE {}", bstr(name))?;
                if let Some(s) = external {
                    write!(f, " {}", bstr(s))?;
                }
                if let Some(s) = internal {
                    write!(f, " [{}]", bstr(s))?;
                }
                write!(f, ">")
            }
            Self::Cdata(s) => write!(f, "<![CDATA[{}]]>", bstr(s)),
            Self::Comment(s) => write!(f, "<!--{}-->", bstr(s)),
            Self::Pi { target, content } => {
                write!(f, "<?{}", bstr(target))?;
                if let Some(s) = content {
                    write!(f, " {}", bstr(s))?;
                }
                write!(f, ">")
            }
        }
    }
}
