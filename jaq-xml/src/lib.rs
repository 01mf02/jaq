extern crate alloc;

use core::fmt::{self, Formatter};
use jaq_json::{Map, Val};
use xmlparser::{ElementEnd, Error, ExternalId, StrSpan, Token, Tokenizer};

// prefix and local name of a tag
type Tag<'a> = (StrSpan<'a>, StrSpan<'a>);

fn tag_as_str((prefix, local): Tag) -> (&str, &str) {
    (prefix.as_str(), local.as_str())
}

fn parse_children(tag: &Tag, tokens: &mut Tokenizer) -> Result<Vec<Val>, Error> {
    let mut children = Vec::new();
    loop {
        let Some(tk) = tokens.next() else { todo!() };
        match tk? {
            Token::ElementEnd {
                end: ElementEnd::Close(prefix, local),
                ..
            } => {
                if tag_as_str((prefix, local)) == tag_as_str(*tag) {
                    return Ok(children);
                } else {
                    todo!()
                }
            }
            tk => children.push(parse(tk, tokens)?),
        }
    }
}

// TODO: unescaping HTML sequences?
fn tac(tag: &Tag, tokens: &mut Tokenizer) -> Result<Val, Error> {
    let tag_str = |tag: &Tag| {
        if tag.0.is_empty() {
            tag.1.as_str().to_owned()
        } else {
            format!("{}:{}", tag.0, tag.1)
        }
    };

    let mut attrs = Vec::new();
    let children = loop {
        let Some(tk) = tokens.next() else { todo!() };
        match tk? {
            Token::Attribute {
                prefix,
                local,
                value,
                ..
            } => attrs.push((
                tag_str(&(prefix, local)).into(),
                value.as_str().to_owned().into(),
            )),
            Token::ElementEnd { end, .. } => match end {
                ElementEnd::Open => break Some(parse_children(tag, tokens)?),
                ElementEnd::Empty => break None,
                ElementEnd::Close(..) => panic!(),
            },
            _ => panic!(),
        }
    };
    let attrs = if attrs.is_empty() { None } else { Some(attrs) };

    Ok(make_obj([
        ("t", Some(tag_str(tag).into())),
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
        Token::ElementStart { prefix, local, .. } => tac(&(prefix, local), tokens)?,
        Token::Text { text } => ss_val(text),
        // xmlparser should never yield these tokens at this point
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
                let Some(tk) = tokens.next() else { todo!() };
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

fn invalid_entry(o: &'static str, k: &RcStr, v: &Val) -> ! {
    panic!("invalid entry in {o} object: {{\"{k}\": {v}}}")
}

impl TryFrom<&Val> for XmlVal {
    type Error = ();
    fn try_from(v: &Val) -> Result<Self, Self::Error> {
        let from_kv = |(k, v): (&RcStr, &_)| match v {
            Val::Str(v) => Ok((k.clone(), v.clone())),
            _ => invalid_entry("attribute", k, v),
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
                        _ => invalid_entry("tac", k, v),
                    }
                }
                Ok(Self::Tac(t.clone(), a, c))
            }
            Val::Obj(o) => {
                let mut o = o.iter();
                let (k, v) = match (o.next(), o.next()) {
                    (Some(kv), None) => kv,
                    _ => panic!("expected singleton object, found: {v}"),
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
                                _ => invalid_entry("doctype", k, v),
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
                                _ => invalid_entry("pi", k, v),
                            }
                        }
                        Ok(Self::Pi { target, content })
                    }
                    _ => invalid_entry("unknown", k, v),
                }
            }
            _ => panic!("expected XML value, found {v}"),
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
