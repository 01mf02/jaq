use jaq_xml::{parse_many, write, u8_str};
use quick_xml::{Reader, Writer};

fn main() {
    let xml = r#"<?xml version = '1.0' ?><tag1 att1 = "test">
                <bla/>
                <?bla blu?>
                <tag2><!--Test comment-->Test</tag2>
                <tag2>Test 2</tag2>
             </tag1>"#;
    let mut read = Reader::from_str(xml);
    read.config_mut().trim_text(true);

    let mut writer = Writer::new(std::io::Cursor::new(Vec::new()));

    let nodes = parse_many(&mut read, None).unwrap();
    dbg!(&nodes);

    for node in &nodes {
        write(&mut writer, node).unwrap();
    }
    let output = u8_str(&writer.into_inner().into_inner()).unwrap();
    println!("{output}");
}
