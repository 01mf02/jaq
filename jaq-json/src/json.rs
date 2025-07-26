//! JSON parsing.
use crate::{Error, Map, Num, Val, ValR};
use alloc::{string::ToString, vec::Vec};
use hifijson::token::{Expect, Lex, Token};
use hifijson::{LexAlloc, SliceLexer};

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
        Token::Quote => Ok(Val::from(lexer.str_string()?.to_string())),
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

/// Convert string to a single JSON value.
pub(crate) fn from_str(s: &str) -> ValR {
    let fail = |e| Error::str(format_args!("cannot parse {s} as JSON: {e}"));
    let mut lexer = SliceLexer::new(s.as_bytes());
    lexer.exactly_one(parse).map_err(fail)
}
