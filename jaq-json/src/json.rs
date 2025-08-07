//! JSON parsing.
use crate::{bstr, Error, Map, Num, Val, ValR};
use alloc::{string::ToString, vec::Vec};
use hifijson::token::{Expect, Lex, Token};
use hifijson::{str, LexAlloc, SliceLexer};

fn parse_string<L: LexAlloc>(lexer: &mut L) -> Result<Vec<u8>, hifijson::Error> {
    let on_string = |bytes: &mut L::Bytes, out: &mut Vec<u8>| {
        out.extend(bytes.iter());
        Ok(())
    };
    lexer.str_fold(Vec::new(), on_string, |lexer, escape, out| {
        let c = lexer.escape_char(escape).map_err(str::Error::Escape)?;
        out.extend(c.encode_utf8(&mut [0; 4]).as_bytes());
        Ok(())
    })
}

/// Parse at least one JSON value, given an initial token and a lexer.
///
/// If the underlying lexer reads input fallibly (for example `IterLexer`),
/// the error returned by this function might be misleading.
/// In that case, always check whether the lexer contains an error.
pub fn parse(token: Token, lexer: &mut impl LexAlloc) -> Result<Val, hifijson::Error> {
    match token {
        Token::Null => Ok(Val::Null),
        Token::True => Ok(Val::Bool(true)),
        Token::False => Ok(Val::Bool(false)),
        Token::DigitOrMinus => Ok(Val::Num({
            let (num, parts) = lexer.num_string()?;
            // if we are dealing with an integer ...
            if parts.dot.is_none() && parts.exp.is_none() {
                Num::try_from_int_str(&num, 10).unwrap()
            } else {
                Num::Dec(num.to_string().into())
            }
        })),
        Token::Quote => Ok(Val::utf8_str(parse_string(lexer)?)),
        Token::LSquare => Ok(Val::Arr({
            let mut arr = Vec::new();
            lexer.seq(Token::RSquare, |token, lexer| {
                arr.push(parse(token, lexer)?);
                Ok::<_, hifijson::Error>(())
            })?;
            arr.into()
        })),
        Token::LCurly => Ok(Val::obj({
            let mut obj = Map::default();
            lexer.seq(Token::RCurly, |token, lexer| {
                let is_colon = |t: &Token| *t == Token::Colon;
                let key = parse(token, lexer)?;
                lexer.ws_token().filter(is_colon).ok_or(Expect::Colon)?;
                let value = parse(lexer.ws_token().ok_or(Expect::Value)?, lexer)?;
                obj.insert(key, value);
                Ok::<_, hifijson::Error>(())
            })?;
            obj
        })),
        _ => Err(Expect::Value)?,
    }
}

/// Convert bytes to a single JSON value.
pub(crate) fn from_bytes(s: &[u8]) -> ValR {
    let fail = |e| Error::str(format_args!("cannot parse {} as JSON: {e}", bstr(s)));
    let mut lexer = SliceLexer::new(s);
    lexer.exactly_one(parse).map_err(fail)
}
