/*
use alloc::{string::String, string::ToString, vec::Vec};
use core::fmt::{self, Display};
*/

/// Exception.
///
/// This is either an error or control flow data internal to jaq.
/// Users should only be able to observe errors.
#[derive(Clone, Debug)]
pub struct Exn<'a, V>(pub(crate) Inner<'a, V>);

#[derive(Clone, Debug)]
pub enum Inner<'a, V> {
    Err(Error<V>),
    /// Tail-recursive call.
    ///
    /// This is used internally to execute tail-recursive filters.
    /// If this can be observed by users, then this is a bug.
    TailCall(&'a crate::compile::TermId, crate::Vars<'a, V>, V),
    Break(usize),
}

impl<V> Exn<'_, V> {
    /// If the exception is an error, yield it, else yield the exception.
    pub fn get_err(self) -> Result<Error<V>, Self> {
        match self.0 {
            Inner::Err(e) => Ok(e),
            _ => Err(self),
        }
    }
}

impl<V> From<Error<V>> for Exn<'_, V> {
    fn from(e: Error<V>) -> Self {
        Exn(Inner::Err(e))
    }
}

use crate::Error;

/*
pub enum Part<V, S = &'static str> {
    Val(V),
    Str(S),
}

pub struct Error<V>(Part<V, Vec<Part<V>>>);

impl<V> Error<V> {
    pub fn new(v: V) -> Self {
        Self(Part::Val(v))
    }
}

impl<V> FromIterator<Part<V>> for Error<V> {
    fn from_iter<T: IntoIterator<Item = Part<V>>>(iter: T) -> Self {
        Self(Part::Str(iter.into_iter().collect()))
    }
}

impl<V: From<String> + Display> Error<V> {
    /// Convert the error into a value to be used by `catch` filters.
    pub fn into_val(self) -> V {
        if let Part::Val(v) = self.0 {
            v
        } else {
            V::from(self.to_string())
        }
    }
}

impl<V: Display> Display for Error<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Part::Val(v) => v.fmt(f),
            Part::Str(parts) => parts.iter().try_for_each(|part| match part {
                Part::Val(v) => v.fmt(f),
                Part::Str(s) => s.fmt(f),
            }),
        }
    }
}
*/
