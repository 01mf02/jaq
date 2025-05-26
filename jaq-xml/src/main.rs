use jaq_xml::new::parse;
use xmlparser::Tokenizer;

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
    let mut tokens = Tokenizer::from(xml);
    /*
    for tk in &mut tokens {
        println!("{tk:?}")
    }
    */
    let vals = core::iter::from_fn(|| tokens.next().map(|tk| parse(tk?, &mut tokens)));
    for v in vals {
        println!("{v:?}")
    }
}
