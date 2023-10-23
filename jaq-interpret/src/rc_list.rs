#[derive(Debug)]
pub struct List<T>(alloc::rc::Rc<Node<T>>);

#[derive(Debug)]
enum Node<T> {
    Nil,
    Cons(T, List<T>),
}

impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> List<T> {
    /// Return an empty list.
    pub fn new() -> Self {
        Self(Node::Nil.into())
    }

    /// Append a new element to the list.
    pub fn cons(self, x: T) -> Self {
        Self(Node::Cons(x, self).into())
    }

    /// Repeatedly add elements to the list.
    pub fn extend(self, iter: impl IntoIterator<Item = T>) -> Self {
        iter.into_iter().fold(self, |acc, x| acc.cons(x))
    }

    /// Return the element most recently added to the list.
    pub fn head(&self) -> Option<&T> {
        match &*self.0 {
            Node::Nil => None,
            Node::Cons(x, _) => Some(x),
        }
    }

    /// Get the `n`-th element from the list, starting from the most recently added.
    pub fn get(&self, n: usize) -> Option<&T> {
        self.skip(n).head()
    }

    /// Remove the `n` top values from the list.
    ///
    /// If `n` is greater than the number of list elements, return nil.
    pub fn skip(&self, n: usize) -> &Self {
        let mut cur = self;
        for _ in 0..n {
            match &*cur.0 {
                Node::Cons(_, xs) => cur = xs,
                Node::Nil => break,
            }
        }
        cur
    }

    #[cfg(test)]
    fn iter(&self) -> impl Iterator<Item = &T> {
        use alloc::boxed::Box;
        match &*self.0 {
            Node::Cons(x, xs) => Box::new(core::iter::once(x).chain(xs.iter())),
            Node::Nil => Box::new(core::iter::empty()) as Box<dyn Iterator<Item = _>>,
        }
    }
}

#[test]
fn test() {
    use alloc::{vec, vec::Vec};
    let eq = |l: &List<_>, a| assert_eq!(l.iter().cloned().collect::<Vec<_>>(), a);

    let l = List::new().cons(2).cons(1).cons(0);
    eq(&l, vec![0, 1, 2]);

    eq(l.skip(0), vec![0, 1, 2]);
    eq(l.skip(1), vec![1, 2]);
    eq(l.skip(2), vec![2]);
    eq(l.skip(3), vec![]);

    assert_eq!(l.get(0), Some(&0));
    assert_eq!(l.get(1), Some(&1));
    assert_eq!(l.get(2), Some(&2));
    assert_eq!(l.get(3), None);
}
