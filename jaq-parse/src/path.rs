use super::{Delim, Token};
use chumsky::prelude::*;
use jaq_syn::path::{Opt, Part, Path};
use jaq_syn::{Call, Spanned, Str};

fn opt() -> impl Parser<Token, Opt, Error = Simple<Token>> + Clone {
    just(Token::Question).or_not().map(|q| match q {
        Some(_) => Opt::Optional,
        None => Opt::Essential,
    })
}

pub fn key<T, P>(
    unstable: bool,
    expr: P,
) -> impl Parser<Token, Str<Spanned<T>>, Error = P::Error> + Clone
where
    T: From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    select! {
        Token::Ident(s) => Str::from(s),
    }
    .or(super::string::str_(unstable, expr))
    .labelled("object key")
}

fn index<T, P>(
    unstable: bool,
    expr: P,
) -> impl Parser<Token, Part<Spanned<T>>, Error = P::Error> + Clone
where
    T: From<Str<Spanned<T>>> + From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    key(unstable, expr).map_with_span(|id, span| Part::Index((T::from(id), span)))
}

/// Match `[]`, `[e]`, `[e:]`, `[e:e]`, `[:e]` (all without brackets).
fn range<T, P>(expr: P) -> impl Parser<Token, Part<Spanned<T>>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let e2 = just(Token::Colon).ignore_then(expr.clone().or_not());
    let starts_with_expr = expr.clone().then(e2.or_not()).map(|(e1, e2)| match e2 {
        None => Part::Index(e1),
        Some(e2) => Part::Range(Some(e1), e2),
    });
    let starts_with_colon = just(Token::Colon)
        .ignore_then(expr.clone())
        .map(|e2| Part::Range(None, Some(e2)));

    starts_with_expr
        .or(starts_with_colon)
        .or_not()
        .map(|o| o.unwrap_or(Part::Range(None, None)))
}

/// A path after an atomic filter (that is not the identity filter).
pub fn path<T, P>(unstable: bool, expr: P) -> impl Parser<Token, Path<T>, Error = P::Error> + Clone
where
    T: From<Str<Spanned<T>>> + From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let range = Delim::Brack.around(range(expr.clone()));
    let dot_index = just(Token::Dot).ignore_then(index(unstable, expr));
    let dot_range = just(Token::Dot).or_not().ignore_then(range);
    dot_index.or(dot_range).then(opt()).repeated()
}

/// The first part of a path after an identity filter.
pub fn part<T, P>(
    unstable: bool,
    expr: P,
) -> impl Parser<Token, (Part<Spanned<T>>, Opt), Error = P::Error> + Clone
where
    T: From<Str<Spanned<T>>> + From<Call<Spanned<T>>>,
    P: Parser<Token, Spanned<T>, Error = Simple<Token>> + Clone,
{
    let range = Delim::Brack.around(range(expr.clone()));
    range.or(index(unstable, expr)).then(opt())
}
