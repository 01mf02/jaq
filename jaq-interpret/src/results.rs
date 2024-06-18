// SPDX-FileCopyrightText: 2021 Michael Färber
//
// SPDX-License-Identifier: MIT

//! Functions on iterators over results.

// TODO for v2.0: remove this from `results`
pub use crate::box_iter::box_once;
use crate::box_iter::BoxIter;
use alloc::vec::Vec;

/// A boxed iterator over `Result`s.
pub type Results<'a, T, E> = BoxIter<'a, Result<T, E>>;

/// If `x` is an `Err`, return it as iterator, else apply `f` to `x` and return its output.
pub fn then<'a, T, U: 'a, E: 'a>(
    x: Result<T, E>,
    f: impl FnOnce(T) -> Results<'a, U, E>,
) -> Results<'a, U, E> {
    x.map(f).unwrap_or_else(|e| box_once(Err(e)))
}

/// Apply the function to the value if there is no error,
/// set error if the function application yielded an error.
///
/// This is useful if we have to run a function in a context
/// where we cannot fail immediately.
pub fn run_if_ok<'a, T, E>(x: T, e: &mut Option<E>, f: &impl Fn(T) -> Results<'a, T, E>) -> Vec<T> {
    if e.is_some() {
        return Vec::new();
    };
    match f(x).collect() {
        Ok(y) => y,
        Err(err) => {
            *e = Some(err);
            Vec::new()
        }
    }
}

// TODO for v2.0: remove this
// if `inner` is true, output values that yield non-empty output;
// if `outer` is true, output values that yield     empty output
pub(crate) fn recurse<'a, T: Clone + 'a, E: Clone + 'a>(
    inner: bool,
    outer: bool,
    init: Results<'a, T, E>,
    f: impl Fn(T) -> Results<'a, T, E> + 'a,
) -> impl Iterator<Item = Result<T, E>> + 'a {
    let mut stack = Vec::from([init.peekable()]);
    core::iter::from_fn(move || loop {
        let v = loop {
            let mut iter = stack.pop()?;
            match iter.next() {
                None => continue,
                Some(Ok(v)) => {
                    if iter.peek().is_some() {
                        stack.push(iter);
                    }
                    break v;
                }
                e => return e,
            }
        };
        let mut iter = f(v.clone()).peekable();
        match (inner, outer) {
            (true, true) => {
                stack.push(iter);
                return Some(Ok(v));
            }
            (true, false) => {
                if iter.peek().is_some() {
                    stack.push(iter);
                    return Some(Ok(v));
                }
            }
            (false, true) => {
                if iter.peek().is_some() {
                    stack.push(iter);
                } else {
                    return Some(Ok(v));
                }
            }
            // pathological case, included only for completeness
            (false, false) => stack.push(iter),
        }
    })
}

pub(crate) enum Fold<'a, U, E> {
    /// things to be processed
    Input(U),
    /// things to be output, then to be input
    Output(Results<'a, U, E>),
}

// if `inner` is true, output intermediate results
pub(crate) fn fold<'a, T: Clone + 'a, U: Clone + 'a, E: Clone + 'a>(
    inner: bool,
    xs: impl Iterator<Item = Result<T, E>> + Clone + 'a,
    init: Fold<'a, U, E>,
    f: impl Fn(T, U) -> Results<'a, U, E> + 'a,
) -> impl Iterator<Item = Result<U, E>> + 'a {
    let mut stack = Vec::from([(xs, init)]);
    core::iter::from_fn(move || loop {
        let (mut xs, fold) = stack.pop()?;
        match fold {
            Fold::Output(mut ys) => match ys.next() {
                None => continue,
                Some(y) => {
                    // do not grow the stack if the output is empty
                    if ys.size_hint() != (0, Some(0)) {
                        stack.push((xs.clone(), Fold::Output(ys)));
                    }
                    match y {
                        Ok(y) if inner => {
                            stack.push((xs, Fold::Input(y.clone())));
                            return Some(Ok(y));
                        }
                        Ok(y) => stack.push((xs, Fold::Input(y))),
                        Err(e) => return Some(Err(e)),
                    }
                }
            },
            Fold::Input(y) => match xs.next() {
                None if inner => continue,
                None => return Some(Ok(y)),
                Some(Ok(x)) => stack.push((xs, Fold::Output(f(x, y)))),
                Some(Err(e)) => return Some(Err(e)),
            },
        }
    })
}
