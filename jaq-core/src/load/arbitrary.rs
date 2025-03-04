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
        // I tried to loop here to get a non-empty string;
        // however, sometimes u.arbitrary() kept giving me an empty string,
        // so the loop did not terminate.
        let s: &str = u.arbitrary::<&str>()?.trim();
        if s.is_empty() {
            return Err(arbitrary::Error::IncorrectFormat);
        };
        let mut chars = s.chars();
        let tok = match chars.next().unwrap() {
            'a'..='z' | 'A'..='Z' | '_' => Tok::Word,
            '$' => Tok::Var,
            '0'..='9' => Tok::Num,
            '-' if matches!(chars.next(), Some('0'..='9')) => Tok::Num,
            '"' => Tok::Str(u.arbitrary()?),
            '(' | '[' | '{' => Tok::Block(u.arbitrary()?),
            _ => Tok::Sym,
        };
        Ok(Self(s, tok))
    }
}
