use super::Token;
use chumsky::prelude::*;
use jaq_syn::path::{Opt, Part, Path, Str};
use jaq_syn::Spanned;

fn opt() -> impl Parser<Token, Opt, Error = Simple<Token>> + Clone {
    just(Token::Ctrl('?')).or_not().map(|q| match q {
        Some(_) => Opt::Optional,
        None => Opt::Essential,
    })
}

pub fn str_<T, P>(expr: P) -> impl Parser<Token, Str<Spanned<T>>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let parenthesised = expr.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let str_ = select! {
        Token::Str(s) => s,
    };
    str_.then(parenthesised.then(str_).repeated())
        .delimited_by(just(Token::Quote), just(Token::Quote))
        .map(|(head, tail)| Str { head, tail })
        .labelled("string")
}

pub fn key<T, P>(expr: P) -> impl Parser<Token, Str<Spanned<T>>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    select! {
        Token::Ident(s) => Str::from(s),
    }
    .or(str_(expr))
    .labelled("object key")
}

pub(crate) fn index<T: From<Str<Spanned<T>>>>(
    expr: impl Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, (Part<Spanned<T>>, Opt), Error = Simple<Token>> + Clone {
    key(expr)
        .map_with_span(|id, span| Part::Index((T::from(id), span)))
        .then(opt())
}

pub(crate) fn path<T, P>(expr: P) -> impl Parser<Token, Path<T>, Error = P::Error> + Clone
where
    T: From<Str<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let range = {
        let colon = just(Token::Ctrl(':'));
        let e2 = colon.clone().ignore_then(expr.clone().or_not());
        let starts_with_expr = expr.clone().then(e2.or_not()).map(|(e1, e2)| match e2 {
            None => Part::Index(e1),
            Some(e2) => Part::Range(Some(e1), e2),
        });
        let starts_with_colon = colon
            .ignore_then(expr.clone())
            .map(|e2| Part::Range(None, Some(e2)));

        starts_with_expr
            .or(starts_with_colon)
            .or_not()
            .map(|o| o.unwrap_or(Part::Range(None, None)))
    };

    let ranges = range
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .then(opt())
        .repeated();

    let dot_id = just(Token::Dot).ignore_then(index(expr));

    ranges
        .clone()
        .chain(dot_id.chain(ranges).repeated().flatten())
        .collect()
}
