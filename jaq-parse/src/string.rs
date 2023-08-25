use super::Token;
use chumsky::prelude::*;
use jaq_syn::{Call, Spanned, Str};

pub fn str_<T, P>(expr: P) -> impl Parser<Token, Str<Spanned<T>>, Error = P::Error> + Clone
where
    T: From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    //.filter(|fmt| fmt.name.starts_with('@'));
    let fmt = super::def::call(expr.clone()).map_with_span(|x, span| ((T::from(x), span)).into());

    let parenthesised = expr.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let chars = select! {
        Token::Str(s) => s,
    };
    let parts = chars
        .then(parenthesised.then(chars).repeated())
        .map(|(head, tail)| {
            use core::iter::once;
            use jaq_syn::string::Part;
            let tail = tail
                .into_iter()
                .flat_map(|(f, s)| [Part::Fun(f), Part::Str(s)]);
            once(Part::Str(head)).chain(tail).collect()
        });
    fmt.or_not()
        .then(parts.delimited_by(just(Token::Quote), just(Token::Quote)))
        .map(|(fmt, parts)| Str { fmt, parts })
        .labelled("string")
}

