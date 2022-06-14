#[derive(Clone, Debug)]
pub enum RcList<T> {
    Nil,
    Cons(T, alloc::rc::Rc<Self>),
}

impl<T> RcList<T> {
    pub fn new() -> Self {
        Self::Nil
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
            match self {
                Self::Cons(_, xs) => ctx = xs,
                Self::Nil => return &Self::Nil,
            }
            n -= 1;
        }
        ctx
    }
}
