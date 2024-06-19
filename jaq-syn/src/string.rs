// SPDX-FileCopyrightText: 2021 Michael Färber
//
// SPDX-License-Identifier: MIT

//! Interpolated strings.
use alloc::{boxed::Box, string::String, vec::Vec};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A part of an interpolated string.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Part<T> {
    /// constant string
    Str(String),
    /// interpolated filter
    Fun(T),
}

impl<T> Part<T> {
    /// Apply a function to the contained filters.
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Part<U> {
        match self {
            Self::Str(s) => Part::Str(s),
            Self::Fun(x) => Part::Fun(f(x)),
        }
    }

    /// Returns true if the part is an empty constant string.
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Str(s) => s.is_empty(),
            Self::Fun(_) => false,
        }
    }
}

/// A possibly interpolated string.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Str<T> {
    /// optional filter that is applied to the output of interpolated filters
    /// (`tostring` if not given)
    pub fmt: Option<Box<T>>,
    /// sequence of strings and interpolated filters
    pub parts: Vec<Part<T>>,
}

impl<T> Str<T> {
    /// Apply a function to the contained filters.
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Str<U> {
        Str {
            fmt: self.fmt.map(|fmt| Box::new(f(*fmt))),
            parts: self.parts.into_iter().map(|p| p.map(&mut f)).collect(),
        }
    }
}

impl<T> From<String> for Str<T> {
    fn from(s: String) -> Self {
        Self {
            fmt: None,
            parts: Vec::from([Part::Str(s)]),
        }
    }
}
