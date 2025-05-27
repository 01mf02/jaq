use jaq_xml::new::parse_str;

fn main() {
    let xml = r#"<?xml version = '1.0' ?><tag1 att1 = "test">
                <foo:bar/>
                <?bla blu?>
                Multiple
                lines
                <![CDATA[text]]>
                <tag2><!--Test comment-->Test</tag2>
                <tag2>Test 2</tag2>
             </tag1>"#;
    /*
    let mut tokens = xmlparser::Tokenizer::from(xml);
    for tk in &mut tokens {
        println!("{tk:?}")
    }
    */
    let vals = parse_str(xml);
    for v in vals {
        println!("{v:?}")
    }
}
