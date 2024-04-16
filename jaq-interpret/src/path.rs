use crate::box_iter::{box_once, flat_map_with, map_with, BoxIter};
use crate::results::then;
use crate::val::ValT;
use crate::Error;
use alloc::{boxed::Box, vec::Vec};
pub use jaq_syn::path::Opt;

#[derive(Clone, Debug)]
pub struct Path<F>(pub Vec<(Part<F>, Opt)>);

#[derive(Clone, Debug)]
pub enum Part<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

impl<'a, U: Clone + 'a, E: Clone + 'a, T: Clone + IntoIterator<Item = Result<U, E>> + 'a> Path<T> {
    pub fn explode(self) -> impl Iterator<Item = Result<Path<U>, E>> + 'a {
        Path(Vec::new())
            .combinations(self.0.into_iter())
            .map(|path| path.transpose())
    }
}

impl<'a, U: Clone + 'a> Path<U> {
    fn combinations<I, F>(self, mut iter: I) -> BoxIter<'a, Self>
    where
        I: Iterator<Item = (Part<F>, Opt)> + Clone + 'a,
        F: IntoIterator<Item = U> + Clone + 'a,
    {
        if let Some((part, opt)) = iter.next() {
            let parts = part.into_iter();
            flat_map_with(parts, (self, iter), move |part, (mut prev, iter)| {
                prev.0.push((part, opt));
                prev.combinations(iter)
            })
        } else {
            box_once(self)
        }
    }
}

impl<'a, V: ValT + 'a> Path<V> {
    pub fn run(self, v: V) -> Box<dyn Iterator<Item = Result<V, Error>> + 'a> {
        run(self.0.into_iter(), v)
    }

    pub fn update<F>(mut self, v: V, f: F) -> Result<V, Error>
    where
        F: Fn(V) -> Box<dyn Iterator<Item = Result<V, Error>> + 'a>,
    {
        if let Some(last) = self.0.pop() {
            update(self.0.into_iter(), last, v, &f)
        } else {
            // should be unreachable
            Ok(v)
        }
    }
}

fn run<'a, V: ValT + 'a, I>(mut iter: I, val: V) -> Box<dyn Iterator<Item = Result<V, Error>> + 'a>
where
    I: Iterator<Item = (Part<V>, Opt)> + Clone + 'a,
{
    if let Some((part, opt)) = iter.next() {
        let essential = matches!(opt, Opt::Essential);
        let ys = part.run(val).filter(move |v| essential || v.is_ok());
        flat_map_with(ys, iter, move |v, iter| then(v, |v| run(iter, v)))
    } else {
        box_once(Ok(val))
    }
}

fn update<'a, V: ValT, P, F>(mut iter: P, last: (Part<V>, Opt), v: V, f: &F) -> Result<V, Error>
where
    P: Iterator<Item = (Part<V>, Opt)> + Clone,
    F: Fn(V) -> Box<dyn Iterator<Item = Result<V, Error>> + 'a>,
{
    if let Some((part, opt)) = iter.next() {
        use core::iter::once;
        part.update(v, opt, |v| once(update(iter.clone(), last.clone(), v, f)))
    } else {
        last.0.update(v, last.1, f)
    }
}

impl<'a, V: ValT + 'a> Part<V> {
    fn run(&self, v: V) -> impl Iterator<Item = Result<V, Error>> + 'a {
        match self {
            Self::Index(idx) => box_once(v.index(idx)),
            Self::Range(None, None) => Box::new(v.values()),
            Self::Range(from, upto) => box_once(v.range(from.as_ref()..upto.as_ref())),
        }
    }

    fn update<F, I>(&self, v: V, opt: Opt, f: F) -> Result<V, Error>
    where
        F: Fn(V) -> I,
        I: Iterator<Item = Result<V, Error>>,
    {
        match self {
            Self::Index(idx) => v.map_index(idx, opt, f),
            Self::Range(None, None) => v.map_values(opt, f),
            Self::Range(from, upto) => v.map_range(from.as_ref()..upto.as_ref(), opt, f),
        }
    }
}

impl<'a, U: Clone + 'a, F: IntoIterator<Item = U> + Clone + 'a> Part<F> {
    fn into_iter(self) -> BoxIter<'a, Part<U>> {
        use Part::{Index, Range};
        match self {
            Index(i) => Box::new(i.into_iter().map(Index)),
            Range(None, None) => box_once(Range(None, None)),
            Range(Some(from), None) => {
                Box::new(from.into_iter().map(|from| Range(Some(from), None)))
            }
            Range(None, Some(upto)) => {
                Box::new(upto.into_iter().map(|upto| Range(None, Some(upto))))
            }
            Range(Some(from), Some(upto)) => {
                Box::new(flat_map_with(from.into_iter(), upto, move |from, upto| {
                    map_with(upto.into_iter(), from, move |upto, from| {
                        Range(Some(from), Some(upto))
                    })
                }))
            }
        }
    }
}

impl<T> Path<T> {
    pub fn map_ref<'a, U>(&'a self, mut f: impl FnMut(&'a T) -> U) -> Path<U> {
        let path = self.0.iter();
        let path = path.map(move |(part, opt)| (part.as_ref().map(&mut f), *opt));
        Path(path.collect())
    }
}

impl<T> Part<T> {
    fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Part<U> {
        use Part::{Index, Range};
        match self {
            Index(i) => Index(f(i)),
            Range(from, upto) => Range(from.map(&mut f), upto.map(&mut f)),
        }
    }
}

impl<T, E> Path<Result<T, E>> {
    fn transpose(self) -> Result<Path<T>, E> {
        self.0
            .into_iter()
            .map(|(part, opt)| Ok((part.transpose()?, opt)))
            .collect::<Result<_, _>>()
            .map(Path)
    }
}

impl<T, E> Part<Result<T, E>> {
    fn transpose(self) -> Result<Part<T>, E> {
        match self {
            Self::Index(i) => Ok(Part::Index(i?)),
            Self::Range(from, upto) => Ok(Part::Range(from.transpose()?, upto.transpose()?)),
        }
    }
}

impl<F> Part<F> {
    fn as_ref(&self) -> Part<&F> {
        match self {
            Self::Index(i) => Part::Index(i),
            Self::Range(from, upto) => Part::Range(from.as_ref(), upto.as_ref()),
        }
    }
}

impl<F> From<Part<F>> for Path<F> {
    fn from(p: Part<F>) -> Self {
        Self(Vec::from([(p, Opt::Essential)]))
    }
}
