#[derive(Debug, PartialEq, Eq)]
pub struct List<T>(alloc::rc::Rc<Node<T>>);

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn iter(&self) -> Iter<T> {
        Iter(self)
    }
}

pub struct Iter<'a, T>(&'a List<T>);

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let (x, xs) = match &*self.0 .0 {
            Node::Cons(x, xs) => (x, xs),
            Node::Nil => return None,
        };
        self.0 = xs;
        Some(x)
    }
}

#[test]
fn test() {
    use alloc::{vec, vec::Vec};
    let eq = |l: &List<_>, a| assert_eq!(l.iter().cloned().collect::<Vec<_>>(), a);

    let l = List::new().cons(2).cons(1).cons(0);
    eq(&l, vec![0, 1, 2]);
}
