//! Unit tests.

use crate::Error;
use alloc::vec::Vec;
use jaq_bla::data::{Ctx, Data};
use jaq_core::{unwrap_valr, Vars};
use jaq_json::{invalid_data, json, Val};
use std::io::BufRead;
use std::process::ExitCode;

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
    let filter = jaq_bla::compile(&test.filter, &[]).map_err(Error::Report)?;

    let data = Data {
        runner: &Default::default(),
        lut: &filter.lut,
        inputs: &jaq_std::input::RcIter::new(Box::new(core::iter::empty())),
    };
    let ctx = Ctx::new(&data, Vars::new([]));

    let json = |s: String| json::parse_single(s.as_bytes()).map_err(invalid_data);
    let jsonn = |s: String| json::parse_many(s.as_bytes()).collect::<Result<Val, _>>();
    let input = json(test.input)?;
    let expect: Result<Val, _> = jsonn(test.output.join("\n")).map_err(invalid_data);
    let obtain: Result<Val, _> = filter.id.run((ctx, input)).collect();
    Ok((expect?, unwrap_valr(obtain).map_err(Error::Jaq)?))
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
