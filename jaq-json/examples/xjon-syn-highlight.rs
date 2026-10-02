use hifijson::token::{Expect, Lex};
use hifijson::{LexWrite, SliceLexer};
use jaq_json::Val;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let stdin = io::stdin();

    for value in stdin.lock().split(b'\0') {
        let value = value?;
        //println!("{}", core::str::from_utf8(&value).unwrap());
        let mut tokens = Vec::new();
        let mut lexer = SliceLexer::new(&value);
        let mut slice = lexer.as_slice();

        while let Some(next) = ws_tk(&mut lexer) {
            let consumed = slice.len() - lexer.as_slice().len();
            if consumed > 0 {
                tokens.push(space(&slice[..consumed]));
            }
            slice = lexer.as_slice();

            let typ = parse(next, &mut lexer).unwrap();
            let consumed = slice.len() - lexer.as_slice().len();

            tokens.push(obj([
                ("t", str(typ)),
                ("c", Val::utf8_str(slice[..consumed].to_owned())),
            ]));
            slice = lexer.as_slice();
        }
        if !slice.is_empty() {
            tokens.push(space(slice));
        }
        println!("{}", Val::from_iter(tokens))
    }

    Ok(())
}

/// Eat whitespace/comments, then peek at next character.
fn ws_tk<L: Lex>(lexer: &mut L) -> Option<u8> {
    loop {
        lexer.eat_whitespace();
        match lexer.peek_next() {
            Some(b'#') => lexer.skip_until(|c| c == b'\n'),
            next => return next,
        }
    }
}

fn str(s: impl Into<String>) -> Val {
    Val::from(s.into())
}

fn obj(fields: impl IntoIterator<Item = (&'static str, Val)>) -> Val {
    Val::obj(fields.into_iter().map(|(k, v)| (str(k), v)).collect())
}

fn space(space: &[u8]) -> Val {
    let val = Val::utf8_str(space.to_owned());
    if space.iter().all(|c| c.is_ascii_whitespace()) {
        val
    } else {
        obj([("t", str("comment")), ("c", val)])
    }
}

fn parse<L: LexWrite>(next: u8, lexer: &mut L) -> Result<&'static str, hifijson::Error> {
    Ok(match next {
        b'n' if lexer.strip_prefix(b"null") => "null",
        b't' if lexer.strip_prefix(b"true") => "boolean",
        b'f' if lexer.strip_prefix(b"false") => "boolean",
        b'N' if lexer.strip_prefix(b"NaN") => "number",
        b'I' if lexer.strip_prefix(b"Infinity") => "number",
        b'0'..=b'9' | b'+' | b'-' => {
            parse_num(lexer)?;
            "number"
        }
        b'b' if lexer.strip_prefix(b"b\"") => {
            parse_string(lexer)?;
            "bytes"
        }
        b'"' => {
            parse_string(lexer.discarded())?;
            "string"
        }
        b'[' | b']' | b'{' | b'}' | b',' | b':' => {
            lexer.take_next();
            "symbol"
        }
        _ => Err(Expect::Value)?,
    })
}

fn parse_string<L: LexWrite>(lexer: &mut L) -> Result<Vec<u8>, hifijson::Error> {
    let on_escape = |lexer: &mut L, _out: &mut Vec<u8>| {
        use hifijson::escape::Error;
        lexer.take_next().ok_or(Error::Eof)?;
        Ok(())
    };
    lexer
        .str_fold(Vec::new(), |_bytes, _out| Ok(()), on_escape)
        .map_err(hifijson::Error::Str)
}

fn parse_num<L: LexWrite>(lexer: &mut L) -> Result<(), hifijson::Error> {
    let num = hifijson::num::Num::signed_digits();
    let (_num, _parts) = lexer.num_string_with(num).unvalidated();
    Ok(())
}
