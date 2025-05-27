use jaq_json::Val;
use xmlparser::{ElementEnd, Error, ExternalId, StrSpan, Token, Tokenizer};
use core::fmt::{self, Display, Formatter};

// prefix and local name of a tag
type Tag<'a> = (StrSpan<'a>, StrSpan<'a>);

fn tag_as_str<'a>((prefix, local): Tag<'a>) -> (&'a str, &'a str) {
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

pub fn write(f: &mut Formatter, v: &Val) -> fmt::Result {
    use jaq_core::ValT;
    let write_kv = |f: &mut Formatter, kv: (_, &Val)| match kv {
        (k, Val::Str(v)) => write!(f, " {k}=\"v\""),
        _ => panic!(),
    };
    match v {
        Val::Str(s) => write!(f, "{s}"),
        Val::Arr(a) => a.iter().try_for_each(|v| write(f, v)),
        Val::Obj(o) if o.contains_key(&"t".to_string()) => {
            let mut t = "";
            let mut a = None;
            let mut c = None;

            for (k, v) in o.iter() {
                match (&***k, v) {
                    ("t", Val::Str(s)) => t = &**s,
                    ("a", Val::Obj(attrs)) => a = Some(attrs),
                    ("c", v) => c = Some(v),
                    (_, _) => panic!(),
                }
            }
            write!(f, "<{t}")?;
            if let Some(a) = a {
                a.iter().try_for_each(|kv| write_kv(f, kv))?
            }
            match c {
                Some(c) => {
                    write!(f, ">")?;
                    write(f, c)?;
                    write!(f, "</{t}>")
                }
                None => write!(f, "/>"),
            }
        }
        Val::Obj(o) => {
            let mut o = o.iter();
            let (k, v) = o.next().unwrap();
            assert!(o.next().is_none());
            match &***k {
                "xmldecl" => {
                    write!(f, "<?xml")?;
                    match v {
                        Val::Obj(kvs) => kvs.iter().try_for_each(|kv| write_kv(f, kv))?,
                        _ => todo!(),
                    };
                    write!(f, ">")
                }
                "doctype" => {
                    let mut name = "";
                    let mut external = None;
                    let mut internal = None;
                    match v {
                        Val::Obj(o) if o.contains_key(&"name".to_string()) => {
                            for (k, v) in o.iter() {
                                match (&***k, v) {
                                    ("name", Val::Str(s)) => name = &**s,
                                    ("external", Val::Str(s)) => external = Some(s),
                                    ("internal", Val::Str(s)) => internal = Some(s),
                                    (_, _) => panic!(),
                                }
                            }
                        }
                        _ => todo!(),
                    };
                    write!(f, "<!DOCTYPE {name}")?;
                    if let Some(s) = external {
                        write!(f, " {s}")?;
                    }
                    if let Some(s) = internal {
                        write!(f, " [{s}]")?;
                    }
                    write!(f, ">")
                }
                "cdata" => write!(f, "<![CDATA[{}]]>", v.as_str().unwrap()),
                "comment" => write!(f, "<!--{}-->", v.as_str().unwrap()),
                "pi" => {
                    let mut target = "";
                    let mut content = None;
                    match v {
                        Val::Obj(o) if o.contains_key(&"target".to_string()) => {
                            for (k, v) in o.iter() {
                                match (&***k, v) {
                                    ("target", Val::Str(s)) => target = &**s,
                                    ("content", Val::Str(s)) => content = Some(s),
                                    (_, _) => panic!(),
                                }
                            }
                        }
                        _ => todo!(),
                    };
                    write!(f, "<?{target}")?;
                    if let Some(s) = content {
                        write!(f, " {s}")?;
                    }
                    write!(f, ">")
                }
                _ => todo!(),
            }
        }
        _ => {
            dbg!(v);
            todo!()
        }
    }
}
