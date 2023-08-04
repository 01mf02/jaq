use once_cell::unsync::Lazy;

pub struct LazyIter<I, F>(Lazy<I, F>);

impl<I, F> LazyIter<I, F> {
    pub fn new(f: F) -> Self {
        Self(Lazy::new(f))
    }
}

impl<I: Iterator, F: FnOnce() -> I> Iterator for LazyIter<I, F> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        self.0.next()
    }
}
