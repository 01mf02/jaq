//! Unit tests.

use alloc::vec::Vec;

/// A single jq unit test.
#[cfg_attr(feature = "unstable-flag", non_exhaustive)]
pub struct Test<S> {
    /// Unstable flag
    #[cfg(feature = "unstable-flag")]
    pub unstable: bool,
    /// jq filter
    pub filter: S,
    /// input value in JSON format
    pub input: S,
    /// output values in JSON format
    pub output: Vec<S>,
}

/// Parser for a jq unit test.
#[cfg_attr(feature = "unstable-flag", non_exhaustive)]
pub struct Parser<I> {
    #[cfg_attr(not(feature = "unstable-flag"), allow(unused))]
    unstable: bool,
    lines: I,
}

impl<I> Parser<I> {
    /// Create a parser from an iterator over lines.
    pub fn new(#[cfg(feature = "unstable-flag")] unstable: bool, lines: I) -> Self {
        #[cfg(not(feature = "unstable-flag"))]
        let unstable = false;
        Self { unstable, lines }
    }
}

impl<S: core::ops::Deref<Target = str>, I: Iterator<Item = S>> Iterator for Parser<I> {
    type Item = Test<S>;

    fn next(&mut self) -> Option<Self::Item> {
        let lines = &mut self.lines;
        Some(Test {
            #[cfg(feature = "unstable-flag")]
            unstable: self.unstable,
            filter: lines.find(|l| !(l.is_empty() || l.starts_with('#')))?,
            input: lines.next()?,
            output: lines.take_while(|l| !l.is_empty()).collect(),
        })
    }
}
