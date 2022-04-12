use crate::{Spanned, Token};
use alloc::{string::String, vec::Vec};
use chumsky::prelude::*;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A path such as `.[].a?[1:]`.
pub type Path<T> = Vec<(Part<Spanned<T>>, Opt)>;

/// A part of a path, such as `[]`, `a`, and `[1:]` in `.[].a?[1:]`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Part<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

/// Optionality of a path part.
///
/// For example, `[] | .a` fails with an error, while `[] | .a?` returns nothing.
/// By default, path parts are *essential*, meaning that they fail.
/// Annotating them with `?` makes them *optional*.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug)]
pub enum Opt {
    /// Return nothing if the input cannot be accessed with the path
    Optional,
    /// Fail if the input cannot be accessed with the path
    Essential,
}

pub(crate) fn path<T, P>(expr: P) -> impl Parser<Token, Path<T>, Error = P::Error> + Clone
where
    T: From<String>,
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
            .ignore_then(expr)
            .map(|e2| Part::Range(None, Some(e2)));

        starts_with_expr
            .or(starts_with_colon)
            .or_not()
            .map(|o| o.unwrap_or(Part::Range(None, None)))
    };

    let opt = just(Token::Ctrl('?')).or_not().map(|q| match q {
        Some(_) => Opt::Optional,
        None => Opt::Essential,
    });

    let dot_id = filter_map(|span, tok| match tok {
        Token::Dot(Some(ident)) => Ok((T::from(ident), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    let dot = just(Token::Dot(None)).then(opt.clone());
    let dot_id = dot_id.map(Part::Index).then(opt.clone());

    let ranges = range
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .then(opt)
        .repeated();

    let head = dot.ignore_then(ranges.clone());
    let tail = dot_id.chain(ranges);

    head.or(tail.clone())
        .chain(tail.repeated().flatten())
        .collect()
}

impl Opt {
    /// If the value is optional, return `x`, else fail with `f(x)`.
    pub fn fail<T, E>(self, x: T, f: impl FnOnce(T) -> E) -> Result<T, E> {
        match self {
            Self::Optional => Ok(x),
            Self::Essential => Err(f(x)),
        }
    }

    pub fn collect<T, E>(self, iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
        match self {
            Self::Optional => Ok(iter.filter_map(|x| x.ok()).collect()),
            Self::Essential => iter.collect(),
        }
    }
}
