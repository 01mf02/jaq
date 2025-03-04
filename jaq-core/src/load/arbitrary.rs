//! Generate arbitrary instances of central lexer types.

use super::lex::{StrPart, Tok, Token};
use arbitrary::{Arbitrary, Unstructured};

impl<'a> Arbitrary<'a> for StrPart<&'a str, Token<&'a str>> {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        match u.choose_index(3)? {
            0 => Ok(StrPart::Str(u.arbitrary()?)),
            1 => Ok(StrPart::Term(Token("(", Tok::Block(u.arbitrary()?)))),
            _ => Ok(StrPart::Char(u.arbitrary()?)),
        }
    }
}

impl<'a> Arbitrary<'a> for Token<&'a str> {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let s: &str = u.arbitrary::<&str>()?.trim();
        let mut chars = s.chars();
        let tok = match chars.next() {
            Some('a'..='z' | 'A'..='Z' | '_') => Tok::Word,
            Some('$') => Tok::Var,
            Some('0'..='9') => Tok::Num,
            Some('-') if matches!(chars.next(), Some('0'..='9')) => Tok::Num,
            Some('"') => Tok::Str(u.arbitrary()?),
            Some('(' | '[' | '{') => Tok::Block(u.arbitrary()?),
            Some(_) => Tok::Sym,
            None => u.choose_iter([Tok::Word, Tok::Num])?,
        };
        Ok(Self(s, tok))
    }
}
