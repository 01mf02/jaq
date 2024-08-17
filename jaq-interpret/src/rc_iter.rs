/// A more flexible version of `&mut impl Iterator`.
pub struct RcIter<I: ?Sized>(core::cell::RefCell<I>);

impl<T, I: Iterator<Item = T> + ?Sized> Iterator for &RcIter<I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.0.borrow_mut().next()
    }
}

impl<I> RcIter<I> {
    /// Construct a new mutable iterator.
    pub const fn new(iter: I) -> Self {
        Self(core::cell::RefCell::new(iter))
    }
}
