use jaq_core::load::lex::{Lexer, StrPart, Tok, Token};
use jaq_json::Val;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let stdin = io::stdin();

    for filter in stdin.lock().split(b'\0') {
        let filter = filter?;
        let filter = std::str::from_utf8(&filter)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let tokens = match Lexer::new(filter).lex() {
            Ok(tokens) => tokens,
            Err(_) => panic!("{filter}"),
        };

        let trail = &filter[filter.trim_end_matches(char::is_whitespace).len()..];
        let trail = (!trail.is_empty()).then(|| space(trail));
        let ast = Val::from_iter(highlight_tokens(filter, tokens.into_iter()).chain(trail));

        println!("{}", ast);
    }

    Ok(())
}

fn str(s: impl Into<String>) -> Val {
    Val::from(s.into())
}

fn obj(fields: impl IntoIterator<Item = (&'static str, Val)>) -> Val {
    Val::obj(fields.into_iter().map(|(k, v)| (str(k), v)).collect())
}

fn span(class: &str, value: impl Into<String>) -> Val {
    span_many(class, [Val::from(value.into())])
}

fn span_many(class: &str, children: impl IntoIterator<Item = Val>) -> Val {
    obj([("t", str(class)), ("c", Val::from_iter(children))])
}

fn space(space: &str) -> Val {
    if space.chars().all(|c| c.is_whitespace()) {
        str(space)
    } else {
        span("comment", space)
    }
}

fn highlight_tokens<'a, I: Iterator<Item = Token<&'a str>>>(
    mut src: &'a str,
    tokens: I,
) -> impl Iterator<Item = Val> + use<'a, I> {
    tokens.flat_map(move |token| {
        let off = offset_of(src, token.as_str());

        let space = (off > 0).then(|| space(&src[..off]));
        src = &src[off + token.as_str().len()..];

        space
            .into_iter()
            .chain(std::iter::once(highlight_token(token)))
    })
}

const KEYWORDS: &[&str] = &[
    "as", "and", "or", "if", "then", "else", "elif", "end", "try", "catch", "label", "break",
    "reduce", "foreach", "def",
];
const CMP: &[&str] = &["<", ">", "<=", ">=", "==", "!="];
const UPD: &[&str] = &["=", "|=", "+=", "-=", "*=", "/=", "%=", "//="];
const MATH: &[&str] = &["+", "-", "*", "/", "%"];

fn highlight_token(token: Token<&str>) -> Val {
    let Token(src, tok) = token;

    match tok {
        Tok::Word if KEYWORDS.contains(&src) => span("keyword", src),
        Tok::Word if src == "null" => span("null", src),
        Tok::Word if ["true", "false"].contains(&src) => span("boolean", src),
        Tok::Word => span("word", src),
        Tok::Var => span("variable", src),
        Tok::Fmt => span("format", src),
        Tok::Num => span("number", src),
        Tok::Sym if CMP.contains(&src) => span("compare", src),
        Tok::Sym if UPD.contains(&src) => span("update", src),
        Tok::Sym if MATH.contains(&src) => span("math", src),
        Tok::Sym => span("symbol", src),
        Tok::Block(tokens) => {
            let open = core::iter::once(span("symbol", &src[..1]));
            let rest = highlight_tokens(&src[1..], tokens.into_iter());
            span_many("block", open.chain(rest))
        }
        Tok::Str(parts) => {
            let mut out = Vec::new();

            let quote = || span("string", "\"");
            out.push(quote());

            for part in parts {
                match part {
                    StrPart::Str(s) => out.push(span("string", s)),
                    StrPart::Char(c) => out.push(span("escape", escape_char(c))),
                    StrPart::Term(token) => {
                        out.push(span("escape", "\\"));
                        out.push(highlight_token(token));
                    }
                }
            }

            out.push(quote());

            span_many("block", out)
        }
    }
}

// TODO: This does not preserve the precise string representation.
// For example, '\n' may have originally been '\u00??`.
// Unfortunately, this information is lost by jaq's lexer ...
fn escape_char(c: char) -> String {
    match c {
        '\x08' => "\\b".into(),
        '\x0C' => "\\f".into(),
        '\n' => "\\n".into(),
        '\r' => "\\r".into(),
        '\t' => "\\t".into(),
        '"' => "\\\"".into(),
        '\\' => "\\\\".into(),
        '/' => "\\/".to_owned(),
        c => format!("\\u{:04x}", c as u32),
    }
}

fn offset_of(src: &str, slice: &str) -> usize {
    let base = src.as_ptr() as usize;
    let ptr = slice.as_ptr() as usize;
    ptr - base
}
