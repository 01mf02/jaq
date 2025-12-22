//! Unit tests.

use crate::Error;
use alloc::vec::Vec;
use jaq_all::json::Val;
use std::{io::BufRead, process::ExitCode};

/// A single jq unit test.
pub struct Test<S> {
    /// jq filter
    pub filter: S,
    /// input value in JSON format
    pub input: S,
    /// output values in JSON format
    pub output: Vec<S>,
}

/// Parser for jq unit tests.
pub struct Tests<I>(I);

impl<I> Tests<I> {
    /// Create a parser from an iterator over lines.
    pub fn new(lines: I) -> Self {
        Self(lines)
    }
}

impl<S: core::ops::Deref<Target = str>, I: Iterator<Item = S>> Iterator for Tests<I> {
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

fn run_test(test: Test<String>) -> Result<(Val, Val), Error> {
    use jaq_all::data::{compile, run};
    use jaq_all::json::read::{parse_many, parse_single};

    let mut obtain = Vec::new();
    let runner = &Default::default();
    let path = |(f, b): jaq_all::load::FileReports| (f.map_path(|()| Default::default()), b);
    let filter =
        compile(&test.filter).map_err(|e| Error::Report(e.into_iter().map(path).collect()))?;
    let vars = Default::default();
    let input = core::iter::once(parse_single(test.input.as_bytes()).map_err(|e| e.to_string()));
    run(runner, &filter, vars, input, Error::Parse, |v| {
        obtain.push(v.map_err(Error::Jaq)?);
        Ok(())
    })?;

    let jsonn = |s: String| parse_many(s.as_bytes()).collect::<Result<Val, _>>();
    let expect = jsonn(test.output.join("\n")).map_err(|e| Error::Parse(e.to_string()));
    Ok((expect?, obtain.into_iter().collect()))
}

pub fn run(read: impl BufRead) -> ExitCode {
    let lines = read.lines().map(Result::unwrap);
    let tests = Tests::new(lines);

    let (mut passed, mut total) = (0, 0);
    for test in tests {
        println!("Testing {}", test.filter);
        match run_test(test) {
            Err(e) => eprintln!("{e:?}"),
            Ok((expect, obtain)) if expect != obtain => {
                eprintln!("expected {expect}, obtained {obtain}");
            }
            Ok(_) => passed += 1,
        }
        total += 1;
    }

    println!("{passed} out of {total} tests passed");

    if total > passed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
