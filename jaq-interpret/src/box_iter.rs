use alloc::boxed::Box;

pub type BoxIter<'a, T> = Box<dyn Iterator<Item = T> + 'a>;

/// Return a boxed iterator that yields a single element.
pub fn box_once<'a, T: 'a>(x: T) -> BoxIter<'a, T> {
    Box::new(core::iter::once(x))
}

/// For every element `y` returned by `l`, return the output of `r(y, x)`.
///
/// In case that `l` returns only a single element, this does not clone `x`.
pub fn map_with<'a, T: Clone + 'a, U: 'a, V: 'a>(
    mut l: impl Iterator<Item = U> + 'a,
    x: T,
    r: impl Fn(U, T) -> V + 'a,
) -> BoxIter<'a, V> {
    // this special case is to avoid cloning `x`
    if l.size_hint().1 == Some(1) {
        if let Some(ly) = l.next() {
            // the Rust documentation states that
            // "a buggy iterator may yield [..] more than the upper bound of elements",
            // but so far, it seems that all iterators here are not buggy :)
            assert!(l.next().is_none());
            return box_once(r(ly, x));
        }
    }
    Box::new(l.map(move |ly| r(ly, x.clone())))
}

/// For every element `y` returned by `l`, return the outputs of `r(y, x)`.
///
/// In case that `l` returns only a single element, this does not clone `x`.
pub fn flat_map_with<'a, T: Clone + 'a, U: 'a, V: 'a>(
    mut l: impl Iterator<Item = U> + 'a,
    x: T,
    r: impl Fn(U, T) -> BoxIter<'a, V> + 'a,
) -> BoxIter<'a, V> {
    // this special case is to avoid cloning `x`
    if l.size_hint().1 == Some(1) {
        if let Some(ly) = l.next() {
            // the Rust documentation states that
            // "a buggy iterator may yield [..] more than the upper bound of elements",
            // but so far, it seems that all iterators here are not buggy :)
            assert!(l.next().is_none());
            return Box::new(r(ly, x));
        }
    }
    Box::new(l.flat_map(move |ly| r(ly, x.clone())))
}
