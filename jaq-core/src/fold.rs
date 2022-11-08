use alloc::{boxed::Box, vec::Vec};

// if `inner` is true, output intermediate results
pub fn fold<'a, T: Clone + 'a, E: Clone + 'a>(
    inner: bool,
    xs: impl Iterator<Item = Result<T, E>> + 'a,
    init: Box<dyn Iterator<Item = Result<T, E>> + 'a>,
    f: impl Fn(T, T) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> + 'a,
) -> impl Iterator<Item = Result<T, E>> + 'a {
    use crate::rc_lazy_list::List;
    let xs = List::from_iter(xs);
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
