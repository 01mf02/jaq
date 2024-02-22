use super::{Delim, Token};
use chumsky::prelude::*;
use jaq_syn::{Call, Spanned, Str};

pub fn str_<T, P>(
    #[allow(unused_variables)] unstable: bool,
    expr: P,
) -> impl Parser<Token, Str<Spanned<T>>, Error = P::Error> + Clone
where
    T: From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let call = |name| Call::new(name, Default::default());
    let ident = select! {
        Token::Ident(ident) if ident.starts_with('@') => ident,
    };
    let fmt = ident.map_with_span(move |x, span| (T::from(call(x)), span).into());

    let chars = select! {
        Token::Str(s) => s,
    };
    let parts = chars.then(Delim::Paren.around(expr).then(chars).repeated());
    let parts = parts.map(|(head, tail)| {
        use core::iter::once;
        use jaq_syn::string::Part::{Fun, Str};
        let tail = tail.into_iter().flat_map(|(f, s)| [Fun(f), Str(s)]);
        let parts = once(Str(head)).chain(tail);
        parts.filter(|p| !p.is_empty()).collect()
    });
    fmt.or_not()
        .then(parts.delimited_by(just(Token::Quote), just(Token::Quote)))
        .map(|(fmt, parts)| Str::new(fmt, parts))
        .labelled("string")
}
