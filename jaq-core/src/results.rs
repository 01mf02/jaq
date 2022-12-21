//! Functions on iterators over results.

use crate::rc_lazy_list::List;
use alloc::{boxed::Box, vec::Vec};

type Results<'a, T, E> = Box<dyn Iterator<Item = Result<T, E>> + 'a>;

pub fn then<'a, T, U: 'a, E: 'a>(
    x: Result<T, E>,
    f: impl FnOnce(T) -> Results<'a, U, E>,
) -> Results<'a, U, E> {
    x.map(f)
        .unwrap_or_else(|e| Box::new(core::iter::once(Err(e))))
}

// if `inner` is true, output values that yield non-empty output;
// if `outer` is true, output values that yield     empty output
pub fn recurse<'a, T: Clone + 'a, E: Clone + 'a>(
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
                        stack.push(iter)
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

// if `inner` is true, output intermediate results
pub fn fold<'a, T: Clone + 'a, E: Clone + 'a>(
    inner: bool,
    xs: List<'a, Result<T, E>>,
    init: Results<'a, T, E>,
    f: impl Fn(T, T) -> Results<'a, T, E> + 'a,
) -> impl Iterator<Item = Result<T, E>> + 'a {
    let mut stack = Vec::from([(xs, init.peekable())]);
    core::iter::from_fn(move || loop {
        let (mut xs, mut vs) = stack.pop()?;
        let v = match vs.next() {
            Some(Ok(v)) => v,
            e @ Some(Err(_)) => return e,
            None => continue,
        };

        // this `if` avoids growing the stack unnecessarily
        if vs.peek().is_some() {
            stack.push((xs.clone(), vs));
        }

        let x = match xs.next() {
            Some(Ok(x)) => x,
            e @ Some(Err(_)) => return e,
            None => return Some(Ok(v)),
        };

        if inner {
            // `foreach`
            stack.push((xs, f(x, v.clone()).peekable()));
            return Some(Ok(v));
        } else {
            // `reduce`
            stack.push((xs, f(x, v).peekable()))
        }
    })
}
