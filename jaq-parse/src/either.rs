pub(crate) use ::either::*;
use alloc::vec::Vec;
use chumsky::{
    debug::{Silent, Verbose},
    error::Located,
    Error, Parser, Span, Stream,
};

macro_rules! map_either {
    ($value:expr, $pattern:pat => $result:expr) => {
        match $value {
            $crate::either::Left($pattern) => $crate::either::Left($result),
            $crate::either::Right($pattern) => $crate::either::Right($result),
        }
    };
}

// ([], Ok((out, alt_err))) => parsing successful,
// alt_err = potential alternative error should a different number of optional patterns be parsed
// ([x, ...], Ok((out, alt_err)) => parsing failed, but recovery occurred so parsing may continue
// ([...], Err(err)) => parsing failed, recovery failed, and one or more errors were produced
type PResult<I, O, E> = (
    Vec<Located<I, E>>,
    Result<(O, Option<Located<I, E>>), Located<I, E>>,
);

// Shorthand for a stream with the given input and error type.
type StreamOf<'a, I, E> = Stream<'a, I, <E as Error<I>>::Span>;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct ChumskyEither<L, R>(Either<L, R>);

impl<L, R> From<Either<L, R>> for ChumskyEither<L, R> {
    fn from(value: Either<L, R>) -> Self {
        Self(value)
    }
}

impl<L, R> From<ChumskyEither<L, R>> for Either<L, R> {
    fn from(value: ChumskyEither<L, R>) -> Self {
        value.0
    }
}

impl<I: Clone, O, L, R> Parser<I, O> for ChumskyEither<L, R>
where
    L: Parser<I, O>,
    R: Parser<I, O, Error = L::Error>,
{
    type Error = L::Error;

    #[allow(deprecated)]
    fn parse_inner<D: chumsky::debug::Debugger>(
        &self,
        debugger: &mut D,
        stream: &mut StreamOf<I, Self::Error>,
    ) -> PResult<I, O, Self::Error>
    where
        Self: Sized,
    {
        either::for_both!(&self.0, parser => Parser::parse_inner(parser, debugger, stream))
    }

    #[allow(deprecated)]
    fn parse_inner_verbose(
        &self,
        debugger: &mut Verbose,
        stream: &mut StreamOf<I, Self::Error>,
    ) -> PResult<I, O, Self::Error> {
        either::for_both!(&self.0, parser => Parser::parse_inner_verbose(parser, debugger, stream))
    }

    #[allow(deprecated)]
    fn parse_inner_silent(
        &self,
        debugger: &mut Silent,
        stream: &mut StreamOf<I, Self::Error>,
    ) -> PResult<I, O, Self::Error> {
        either::for_both!(&self.0, parser => Parser::parse_inner_silent(parser, debugger, stream))
    }

    fn parse_recovery<'a, Iter, S>(&self, stream: S) -> (Option<O>, Vec<Self::Error>)
    where
        Self: Sized,
        Iter: Iterator<Item = (I, <Self::Error as Error<I>>::Span)> + 'a,
        S: Into<Stream<'a, I, <Self::Error as Error<I>>::Span, Iter>>,
    {
        either::for_both!(&self.0, parser => Parser::parse_recovery(parser, stream))
    }

    fn parse_recovery_verbose<'a, Iter, S>(&self, stream: S) -> (Option<O>, Vec<Self::Error>)
    where
        Self: Sized,
        Iter: Iterator<Item = (I, <Self::Error as Error<I>>::Span)> + 'a,
        S: Into<Stream<'a, I, <Self::Error as Error<I>>::Span, Iter>>,
    {
        either::for_both!(&self.0, parser => Parser::parse_recovery_verbose(parser, stream))
    }

    fn parse<'a, Iter, S>(&self, stream: S) -> Result<O, Vec<Self::Error>>
    where
        Self: Sized,
        Iter: Iterator<Item = (I, <Self::Error as Error<I>>::Span)> + 'a,
        S: Into<Stream<'a, I, <Self::Error as Error<I>>::Span, Iter>>,
    {
        either::for_both!(&self.0, parser => Parser::parse(parser, stream))
    }
}

impl<L, R> Span for ChumskyEither<L, R>
where
    L: Span,
    R: Span<Offset = L::Offset>,
{
    type Context = Either<L::Context, R::Context>;

    type Offset = L::Offset;

    fn new(context: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
        Self::from(map_either!(context, context => Span::new(context, range)))
    }

    fn context(&self) -> Self::Context {
        map_either!(&self.0, span => Span::context(span))
    }

    fn start(&self) -> Self::Offset {
        either::for_both!(&self.0, s => Span::start(s))
    }

    fn end(&self) -> Self::Offset {
        either::for_both!(&self.0, s => Span::end(s))
    }
}
