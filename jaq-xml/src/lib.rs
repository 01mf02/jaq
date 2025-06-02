extern crate alloc;

use core::fmt::{self, Formatter};
use jaq_json::{Map, Val};
use xmlparser::{ElementEnd, ExternalId, StrSpan, TextPos, Token, Tokenizer};

// prefix and local name of a tag
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

#[derive(Debug)]
pub struct TagPos(String, TextPos);

impl fmt::Display for TagPos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} (at {})", self.0, self.1)
    }
}

#[derive(Debug)]
pub enum Error {
    Xmlparser(xmlparser::Error),
    Unmatched(TagPos, TagPos),
    Unclosed(TagPos),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Xmlparser(e) => e.fmt(f),
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
        Self::Xmlparser(e)
    }
}

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

pub fn doctype(name: &str, external: Option<ExternalId>, internal: Option<&str>) -> Val {
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

pub fn parse(tk: Token, tokens: &mut Tokenizer) -> Result<Val, Error> {
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

pub fn parse_str(s: &str) -> impl Iterator<Item = Result<Val, Error>> + '_ {
    let mut tokens = Tokenizer::from(s);
    core::iter::from_fn(move || tokens.next().map(|tk| parse(tk?, &mut tokens)))
}

type RcStr = alloc::rc::Rc<String>;

#[derive(Debug)]
pub enum Serror {
    InvalidEntry(&'static str, RcStr, Val),
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

impl std::error::Error for Serror {}

pub enum XmlVal {
    XmlDecl(Vec<(RcStr, RcStr)>),
    DocType {
        name: RcStr,
        external: Option<RcStr>,
        internal: Option<RcStr>,
    },
    Pi {
        target: RcStr,
        content: Option<RcStr>,
    },
    Tac(RcStr, Vec<(RcStr, RcStr)>, Option<Box<Self>>),
    Seq(Vec<Self>),
    Str(RcStr),
    Cdata(RcStr),
    Comment(RcStr),
}

impl TryFrom<&Val> for XmlVal {
    type Error = Serror;
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        let from_kv = |(k, v): (&RcStr, &_)| match v {
            Val::Str(v) => Ok((k.clone(), v.clone())),
            _ => Err(Serror::InvalidEntry("attribute", k.clone(), v.clone())),
        };
        let from_kvs = |a: &Map<_, _>| a.iter().map(from_kv).collect::<Result<_, _>>();
        match v {
            Val::Str(s) => Ok(Self::Str(s.clone())),
            Val::Arr(a) => a
                .iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()
                .map(Self::Seq),
            Val::Obj(o) if o.contains_key(&"t".to_string()) => {
                let mut t = RcStr::default();
                let mut a = Vec::new();
                let mut c = None;
                for (k, v) in o.iter() {
                    match (&***k, v) {
                        ("t", Val::Str(s)) => t = s.clone(),
                        ("a", Val::Obj(attrs)) => a = from_kvs(attrs)?,
                        ("c", v) => c = Some(Box::new(v.try_into()?)),
                        _ => Err(Serror::InvalidEntry("tac", k.clone(), v.clone()))?,
                    }
                }
                Ok(Self::Tac(t.clone(), a, c))
            }
            Val::Obj(o) => {
                let mut o = o.iter();
                let (k, v) = match (o.next(), o.next()) {
                    (Some(kv), None) => kv,
                    _ => Err(Serror::SingletonObj(v.clone()))?,
                };
                match (&***k, v) {
                    ("xmldecl", Val::Obj(kvs)) => from_kvs(kvs).map(Self::XmlDecl),
                    ("doctype", Val::Obj(o)) if o.contains_key(&"name".to_string()) => {
                        let mut name = RcStr::default();
                        let mut external = None;
                        let mut internal = None;
                        for (k, v) in o.iter() {
                            match (&***k, v) {
                                ("name", Val::Str(s)) => name = s.clone(),
                                ("external", Val::Str(s)) => external = Some(s.clone()),
                                ("internal", Val::Str(s)) => internal = Some(s.clone()),
                                _ => Err(Serror::InvalidEntry("doctype", k.clone(), v.clone()))?,
                            }
                        }
                        Ok(Self::DocType {
                            name,
                            external,
                            internal,
                        })
                    }
                    ("cdata", Val::Str(s)) => Ok(Self::Cdata(s.clone())),

                    ("comment", Val::Str(s)) => Ok(Self::Comment(s.clone())),
                    ("pi", Val::Obj(o)) if o.contains_key(&"target".to_string()) => {
                        let mut target = RcStr::default();
                        let mut content = None;
                        for (k, v) in o.iter() {
                            match (&***k, v) {
                                ("target", Val::Str(s)) => target = s.clone(),
                                ("content", Val::Str(s)) => content = Some(s.clone()),
                                _ => Err(Serror::InvalidEntry("pi", k.clone(), v.clone()))?,
                            }
                        }
                        Ok(Self::Pi { target, content })
                    }
                    _ => Err(Serror::InvalidEntry("unknown", k.clone(), v.clone()))?,
                }
            }
            Val::Null | Val::Bool(_) | Val::Int(_) | Val::Float(_) | Val::Num(_) => {
                Ok(Self::Str(v.to_string().into()))
            }
        }
    }
}

impl XmlVal {
    pub fn write(&self, f: &mut Formatter) -> fmt::Result {
        let write_kvs = |f: &mut Formatter, a: &Vec<_>| {
            a.iter().try_for_each(|(k, v)| write!(f, " {k}=\"{v}\""))
        };
        match self {
            Self::Str(s) => write!(f, "{s}"),
            Self::Seq(a) => a.iter().try_for_each(|v| v.write(f)),
            Self::Tac(t, a, c) => {
                write!(f, "<{t}")?;
                write_kvs(f, a)?;
                if let Some(c) = c {
                    write!(f, ">")?;
                    c.write(f)?;
                    write!(f, "</{t}>")
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
                write!(f, "<!DOCTYPE {name}")?;
                if let Some(s) = external {
                    write!(f, " {s}")?;
                }
                if let Some(s) = internal {
                    write!(f, " [{s}]")?;
                }
                write!(f, ">")
            }
            Self::Cdata(s) => write!(f, "<![CDATA[{s}]]>"),
            Self::Comment(s) => write!(f, "<!--{s}-->"),
            Self::Pi { target, content } => {
                write!(f, "<?{target}")?;
                if let Some(s) = content {
                    write!(f, " {s}")?;
                }
                write!(f, ">")
            }
        }
    }
}
