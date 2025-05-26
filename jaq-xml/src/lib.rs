use jaq_core::val::ValT;
use jaq_json::Val;
use quick_xml::errors::{Error, IllFormedError, Result};
use quick_xml::events::{BytesCData, BytesPI, BytesStart, BytesText, Event};
use quick_xml::name::QName;
use quick_xml::{Reader, Writer};
use std::io::Write;

pub mod new;

pub fn u8_str(u: &[u8]) -> Result<String> {
    std::str::from_utf8(u)
        .map_err(|e| Error::Encoding(e.into()))
        .map(|s| s.to_owned())
}

fn tac(start: &BytesStart, c: Option<Val>) -> Result<Val> {
    let tag = Val::from(u8_str(start.name().as_ref())?);

    let attrs = start.attributes().map(|a| {
        let a = a?;
        Ok((
            u8_str(a.key.as_ref())?.into(),
            Val::from(a.unescape_value()?.into_owned()),
        ))
    });
    let attrs = if start.attributes().next().is_none() {
        None
    } else {
        Some(Val::obj(attrs.collect::<Result<_>>()?))
    };

    let tac = [("t", Some(tag)), ("a", attrs), ("c", c)]
        .into_iter()
        .flat_map(|(k, v)| v.map(|v| (k.to_string().into(), v)));
    Ok(Val::obj(tac.collect()))
}

fn parse(read: &mut Reader<&[u8]>, parent: Option<QName>) -> Result<Option<Val>> {
    let text_str = |t: BytesText| t.unescape().map(|s| s.into_owned());
    let singleton = |k: &str, v| Val::obj(core::iter::once((k.to_string().into(), v)).collect());

    Ok(Some(match read.read_event()? {
        Event::End(_) => return Ok(None),
        Event::Eof => match parent {
            None => return Ok(None),
            Some(parent) => Err(IllFormedError::MissingEndTag(u8_str(parent.as_ref())?))?,
        },
        Event::Start(start) => {
            let children = parse_many(read, Some(start.name()))?.into_iter().collect();
            tac(&start, Some(children))?
        }
        Event::Empty(start) => tac(&start, None)?,
        Event::Text(e) => Val::from(text_str(e)?),
        Event::Comment(e) => singleton("comment", u8_str(&e)?.into()),
        Event::PI(pi) => singleton("pi", u8_str(&pi)?.into()),
        Event::CData(cd) => singleton("cdata", u8_str(&cd.into_inner())?.into()),
        Event::DocType(dt) => singleton("doctype", text_str(dt)?.into()),
        Event::Decl(d) => singleton("pi", u8_str(&d)?.into()),
    }))
}

pub fn parse_many(read: &mut Reader<&[u8]>, parent: Option<QName>) -> Result<Vec<Val>> {
    core::iter::from_fn(|| parse(read, parent).transpose()).collect()
}

pub fn parse_slice(slice: &[u8]) -> impl Iterator<Item = Result<Val>> + '_ {
    let mut read = Reader::from_reader(slice);
    core::iter::from_fn(move || parse(&mut read, None).transpose())
}

pub fn write<W: Write>(writer: &mut Writer<W>, v: &Val) -> std::io::Result<()> {
    match v {
        Val::Str(s) => writer.write_event(Event::Text(BytesText::new(s))),
        Val::Arr(a) => a.iter().try_for_each(|v| write(writer, v)),
        Val::Obj(o) if o.contains_key(&"t".to_string()) => {
            let mut t = "";
            let mut a: Box<dyn Iterator<Item = _>> = Box::new(core::iter::empty());
            let mut c = None;

            for (k, v) in o.iter() {
                match (&***k, v) {
                    ("t", Val::Str(s)) => t = &**s,
                    ("a", Val::Obj(attrs)) => {
                        a = Box::new(attrs.iter().map(|kv| match kv {
                            (k, Val::Str(v)) => (&***k, &***v),
                            _ => panic!(),
                        }))
                    }
                    ("c", v) => c = Some(v),
                    (_, _) => panic!(),
                }
            }
            let elem = writer.create_element(t).with_attributes(a);
            match c {
                Some(c) => elem.write_inner_content(|writer| write(writer, c))?,
                None => elem.write_empty()?,
            };
            Ok(())
        }
        Val::Obj(o) => {
            let mut o = o.iter();
            let (k, v) = o.next().unwrap();
            assert!(o.next().is_none());
            let v = v.as_str().unwrap();
            match &***k {
                "cdata" => {
                    BytesCData::escaped(v).try_for_each(|cd| writer.write_event(Event::CData(cd)))
                }
                "pi" => writer.write_event(Event::PI(BytesPI::new(v))),
                "doctype" => writer.write_event(Event::DocType(BytesText::new(v))),
                "comment" => Ok(()),
                _ => panic!(),
            }
        }
        _ => todo!(),
    }
}
