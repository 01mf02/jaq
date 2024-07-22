//! Value access and iteration.
use alloc::vec::Vec;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A path such as `.[].a?[1:]`.
pub type Path<T> = Vec<(Part<crate::Spanned<T>>, Opt)>;

/// A part of a path, such as `[]`, `a`, and `[1:]` in `.[].a?[1:]`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Part<I> {
    /// Access arrays with integer and objects with string indices
    Index(I),
    /// Iterate over arrays with optional range bounds and over objects without bounds
    Range(Option<I>, Option<I>),
}

impl<I> Default for Part<I> {
    fn default() -> Self {
        Self::Range(None, None)
    }
}

/// Optionality of a path part, i.e. whether `?` is present.
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

impl<I> Part<I> {
    /// Apply a function to the contained indices.
    pub fn map<J>(self, mut f: impl FnMut(I) -> J) -> Part<J> {
        match self {
            Self::Index(i) => Part::Index(f(i)),
            Self::Range(l, h) => Part::Range(l.map(&mut f), h.map(f)),
        }
    }
}

impl Opt {
    /// If `self` is optional, return `x`, else fail with `f(x)`.
    pub fn fail<T, E>(self, x: T, f: impl FnOnce(T) -> E) -> Result<T, E> {
        match self {
            Self::Optional => Ok(x),
            Self::Essential => Err(f(x)),
        }
    }

    /// If `self` is optional, return all items of the iterator that are `Ok` and succeed,
    /// else return all items of the iterator and fail if any is `Err`.
    pub fn collect<T, E>(self, iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
        match self {
            Self::Optional => Ok(iter.filter_map(|x| x.ok()).collect()),
            Self::Essential => iter.collect(),
        }
    }
}
