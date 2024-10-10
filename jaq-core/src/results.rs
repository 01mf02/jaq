//! Functions on iterators over results.

use crate::box_iter::{box_once, BoxIter};
use alloc::vec::Vec;

/// A boxed iterator over `Result`s.
pub type Results<'a, T, E> = BoxIter<'a, Result<T, E>>;

/// If `x` is an `Err`, return it as iterator, else apply `f` to `x` and return its output.
pub fn then<'a, T, U: 'a, E: 'a>(
    x: Result<T, E>,
    f: impl FnOnce(T) -> Results<'a, U, E>,
) -> Results<'a, U, E> {
    x.map_or_else(|e| box_once(Err(e)), f)
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
