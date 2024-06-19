// SPDX-FileCopyrightText: 2021 Michael Färber
//
// SPDX-License-Identifier: MIT

//! Unit tests.

use alloc::vec::Vec;

/// A single jq unit test.
pub struct Test<S> {
    /// jq filter
    pub filter: S,
    /// input value in JSON format
    pub input: S,
    /// output values in JSON format
    pub output: Vec<S>,
}

/// Parser for a jq unit test.
pub struct Parser<I>(I);

impl<I> Parser<I> {
    /// Create a parser from an iterator over lines.
    pub fn new(lines: I) -> Self {
        Self(lines)
    }
}

impl<S: core::ops::Deref<Target = str>, I: Iterator<Item = S>> Iterator for Parser<I> {
    type Item = Test<S>;

    fn next(&mut self) -> Option<Self::Item> {
        let lines = &mut self.0;
        Some(Test {
            filter: lines.find(|l| !(l.is_empty() || l.starts_with('#')))?,
            input: lines.next()?,
            output: lines.take_while(|l| !l.is_empty()).collect(),
        })
    }
}
