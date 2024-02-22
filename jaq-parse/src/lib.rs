//! JSON query language parser.
#![no_std]
#![deny(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod def;
pub(crate) mod either;
mod filter;
mod path;
mod prec_climb;
mod string;
mod token;

use jaq_syn as syn;

pub use def::{defs, main};
use token::{Delim, Token};

use alloc::{string::String, string::ToString, vec::Vec};
use chumsky::{prelude::*, primitive::custom};
use syn::Spanned;

fn silent() -> chumsky::debug::Silent {
    struct Silent {
        phantom: core::marker::PhantomData<()>,
    }
    let silent = Silent {
        phantom: Default::default(),
    };
    #[allow(unsafe_code)]
    unsafe {
        core::mem::transmute(silent)
    }
}

fn fail<I: Clone, O, E: chumsky::Error<I>>() -> impl chumsky::Parser<I, O, Error = E> + Clone {
    fn f<I: Clone, O, E: chumsky::Error<I>>(stream: &mut chumsky::Stream<I, <E as chumsky::Error<I>>::Span>) -> (
        Vec<chumsky::error::Located<I, E>>,
        Result<(O, Option<chumsky::error::Located<I, E>>), chumsky::error::Located<I, E>>,
    ) {
        let (errors, res): (Vec<chumsky::error::Located<I, E>>, _) =
            empty().not().parse_inner_silent(&mut silent(), stream);
        (
            errors,
            match res {
                Err(err) => Err(err),
                Ok((_out, Some(alt_err))) => Err(alt_err),
                Ok((_out, None)) => unimplemented!(),
            },
        )
    }
    custom(f)
}

/// Lex/parse error.
pub type Error = Simple<String>;

fn lex(
    #[cfg_attr(not(feature = "unstable-flag"), allow(unused_variables))] unstable: bool,
) -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    recursive(|tree| {
        token::tree(
            #[cfg(feature = "unstable-flag")]
            unstable,
            tree,
        )
    })
    .map_with_span(|tree, span| tree.tokens(span))
    .repeated()
    .flatten()
    .collect()
}

/// Parse a string with a given parser.
///
/// May produce `Some` output even if there were errors.
pub fn parse<T, P>(
    #[cfg(feature = "unstable-flag")] unstable: bool,
    src: &str,
    parser: P,
) -> (Option<T>, Vec<Error>)
where
    P: Parser<Token, T, Error = Simple<Token>> + Clone,
{
    #[cfg(not(feature = "unstable-flag"))]
    let unstable = false;
    let (tokens, lex_errs) = lex(unstable)
        .then_ignore(end())
        .recover_with(skip_then_retry_until([]))
        .parse_recovery(src);

    let (parsed, parse_errs) = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let stream = chumsky::Stream::from_iter(len..len + 1, tokens.into_iter());
        parser.then_ignore(end()).parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    let lex_errs = lex_errs.into_iter().map(|e| e.map(|c| c.to_string()));
    let parse_errs = parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string()));
    let errs: Vec<_> = lex_errs.chain(parse_errs).collect();

    (parsed, errs)
}
