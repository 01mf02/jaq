#[derive(Clone, Debug)]
pub enum RcList<T> {
    Nil,
    Cons(T, alloc::rc::Rc<Self>),
}

impl<T> Default for RcList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> RcList<T> {
    pub fn new() -> Self {
        Self::Nil
    }

    pub fn cons(self, x: T) -> Self {
        Self::Cons(x, alloc::rc::Rc::new(self))
    }

    pub fn get(&self, mut n: usize) -> Option<&T> {
        let mut ctx = self;
        while let Self::Cons(x, xs) = ctx {
            if n == 0 {
                return Some(x);
            } else {
                n -= 1;
                ctx = xs;
            }
        }
        None
    }

    pub fn skip(&self, mut n: usize) -> &Self {
        let mut ctx = self;
        while n > 0 {
            match ctx {
                Self::Cons(_, xs) => ctx = xs,
                Self::Nil => return &Self::Nil,
            }
            n -= 1;
        }
        ctx
    }

    #[cfg(test)]
    fn iter(&self) -> impl Iterator<Item = &T> {
        use alloc::boxed::Box;
        match self {
            Self::Cons(x, xs) => Box::new(core::iter::once(x).chain(xs.iter())),
            Self::Nil => Box::new(core::iter::empty()) as Box<dyn Iterator<Item = _>>,
        }
    }
}

#[test]
fn test() {
    use alloc::{vec, vec::Vec};
    let eq = |l: &RcList<_>, a| assert_eq!(l.iter().cloned().collect::<Vec<_>>(), a);

    let l = RcList::new().cons(2).cons(1).cons(0);
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
