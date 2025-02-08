use quick_xml::errors::{Error, Result};
use quick_xml::events::attributes::Attribute;
use quick_xml::events::{BytesStart, BytesText, Event};
use quick_xml::reader::Reader;

#[derive(Debug)]
enum Node {
    Tac(String, Vec<(String, String)>, Vec<Node>),
    Text(String),
    Comment(String),
    Doctype(String),
    Pi(String),
}

fn parse_many(read: &mut Reader<&[u8]>, root: bool) -> Result<Vec<Node>> {
    core::iter::from_fn(|| parse2(read, root).transpose()).collect()
}

fn parse2(read: &mut Reader<&[u8]>, root: bool) -> Result<Option<Node>> {
    let u8_str = |u: &[u8]| {
        std::str::from_utf8(u)
            .map_err(|e| Error::Encoding(e.into()))
            .map(|s| s.to_owned())
    };
    let text_str = |t: BytesText| t.unescape().map(|s| s.into_owned());
    let attr =
        |a: Attribute| Ok::<_, Error>((u8_str(a.key.as_ref())?, a.unescape_value()?.into_owned()));
    let attrs = |s: BytesStart| s.attributes().map(|a| Ok(attr(a?)?)).collect::<Result<_>>();

    Ok(Some(match read.read_event()? {
        Event::End(_) => return Ok(None),
        Event::Eof if root => return Ok(None),
        Event::Eof => panic!(),
        Event::Start(start) => {
            let tag = u8_str(start.name().as_ref())?;
            Node::Tac(tag, attrs(start)?, parse_many(read, false)?)
        }
        Event::Empty(start) => {
            let tag = u8_str(start.name().as_ref())?;
            Node::Tac(tag, attrs(start)?, Vec::new())
        }
        Event::Text(e) => Node::Text(text_str(e)?),
        Event::Comment(e) => Node::Comment(text_str(e)?),
        Event::PI(pi) => Node::Pi(u8_str(&pi)?),
        Event::CData(cd) => Node::Text(u8_str(&cd.into_inner())?),
        Event::DocType(dt) => Node::Doctype(text_str(dt)?),
        Event::Decl(d) => Node::Pi(u8_str(&d)?),
    }))
}

fn main() {
    let xml = r#"<?xml version = '1.0' ?><tag1 att1 = "test">
                <bla/>
                <?bla blu?>
                <tag2><!--Test comment-->Test</tag2>
                <tag2>Test 2</tag2>
             </tag1>"#;
    let mut read = Reader::from_str(xml);
    read.config_mut().trim_text(true);

    let nodes = parse_many(&mut read, true);
    dbg!(nodes);
}
