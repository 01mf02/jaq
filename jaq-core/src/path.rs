use crate::{ClosedFilter, Error, Filter, RValR, RValRs, Val};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryInto;

#[derive(Clone, Debug)]
pub struct Path<F>(Vec<PathElem<F>>);

#[derive(Clone, Debug)]
pub enum PathElem<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

impl<F> Path<F> {
    pub fn new(path: Vec<PathElem<F>>) -> Self {
        Self(path)
    }
}

impl Path<ClosedFilter> {
    pub fn collect(&self, v: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        let mut path = self.0.iter().map(|p| p.run_indices(Rc::clone(&v)));
        path.try_fold(Vec::from([Rc::clone(&v)]), |acc, p| {
            let p = p?;
            acc.into_iter()
                .flat_map(|x| p.collect((*x).clone()))
                .collect()
        })
    }

    pub fn run<'f, F>(&self, v: Rc<Val>, f: F) -> RValRs<'f>
    where
        F: Fn(Rc<Val>) -> RValRs<'f> + Copy,
    {
        let path = self.0.iter().map(|p| p.run_indices(Rc::clone(&v)));
        let path: Result<Vec<_>, _> = path.collect();
        match path {
            Ok(path) => PathElem::run(path.iter(), v, f),
            Err(e) => Box::new(core::iter::once(Err(e))),
        }
    }
}

impl PathElem<ClosedFilter> {
    pub fn run_indices(&self, v: Rc<Val>) -> Result<PathElem<Vec<Rc<Val>>>, Error> {
        use PathElem::*;
        match self {
            Index(i) => Ok(Index(i.run(v).collect::<Result<_, _>>()?)),
            Range(from, until) => {
                let from = from.as_ref().map(|f| f.run(Rc::clone(&v)).collect());
                let until = until.as_ref().map(|u| u.run(v).collect());
                Ok(Range(from.transpose()?, until.transpose()?))
            }
        }
    }
}

impl PathElem<Vec<Rc<Val>>> {
    pub fn collect(&self, current: Val) -> RValRs {
        use core::iter::once;
        match self {
            Self::Index(indices) => match current {
                Val::Arr(a) => Box::new(indices.iter().map(move |i| {
                    let i = wrap(i.as_isize()?, a.len());
                    Ok(if i < 0 || i as usize >= a.len() {
                        Rc::new(Val::Null)
                    } else {
                        Rc::clone(&a[i as usize])
                    })
                })),
                Val::Obj(o) => Box::new(indices.iter().map(move |i| match &**i {
                    Val::Str(s) => Ok(o.get(s).map_or_else(|| Rc::new(Val::Null), Rc::clone)),
                    i => Err(Error::IndexWith(Val::Obj(o.clone()), i.clone())),
                })),
                _ => Box::new(once(Err(Error::Index(current)))),
            },
            Self::Range(None, None) => match current {
                Val::Arr(a) => Box::new(a.into_iter().map(Ok)),
                Val::Obj(o) => Box::new(o.into_iter().map(|(_k, v)| Ok(v))),
                v => Box::new(once(Err(Error::Iter(v)))),
            },
            Self::Range(from, until) => match current {
                Val::Arr(a) => {
                    let from = get_indices(from, a.len(), 0);
                    let until = get_indices(until, a.len(), a.len());
                    Box::new(skip_takes(from, until).map(move |skip_take| {
                        let (skip, take) = skip_take?;
                        let iter = a.iter().cloned().skip(skip).take(take);
                        Ok(Rc::new(Val::Arr(iter.collect())))
                    }))
                }
                Val::Str(s) => {
                    let len = s.chars().count();
                    let from = get_indices(from, len, 0);
                    let until = get_indices(until, len, len);
                    Box::new(skip_takes(from, until).map(move |skip_take| {
                        let (skip, take) = skip_take?;
                        let iter = s.chars().skip(skip).take(take);
                        Ok(Rc::new(Val::Str(iter.collect())))
                    }))
                }
                _ => Box::new(once(Err(Error::Index(current)))),
            },
        }
    }

    pub fn run<'a, 'f, P, F>(mut path: P, v: Rc<Val>, f: F) -> RValRs<'f>
    where
        P: Iterator<Item = &'a Self> + Clone,
        F: Fn(Rc<Val>) -> RValRs<'f> + Copy,
    {
        if let Some(p) = path.next() {
            let f = |v| Self::run(path.clone(), v, f);
            Box::new(core::iter::once(p.map((*v).clone(), f).map(Rc::new)))
        } else {
            f(v)
        }
    }

    pub fn map<F, I>(&self, v: Val, f: F) -> Result<Val, Error>
    where
        F: Fn(Rc<Val>) -> I,
        I: Iterator<Item = RValR>,
    {
        match self {
            Self::Index(indices) => match v {
                Val::Obj(mut o) => {
                    indices.iter().try_for_each(|i| {
                        if let Val::Str(s) = &**i {
                            let some = |v: &Rc<Val>| f(Rc::clone(v)).next().transpose();
                            let none = || f(Rc::new(Val::Null)).next().transpose();
                            o.insert_or_remove(s.clone(), some, none)
                        } else {
                            Err(Error::IndexWith(Val::Obj(o.clone()), (&**i).clone()))
                        }
                    })?;
                    Ok(Val::Obj(o))
                }
                Val::Arr(_a) => {
                    todo!()
                }
                _ => Err(Error::Index(v)),
            },
            Self::Range(None, None) => match v {
                Val::Arr(a) => Ok(Val::Arr(
                    a.into_iter().flat_map(f).collect::<Result<_, _>>()?,
                )),
                Val::Obj(_o) => todo!(),
                v => Err(Error::Iter(v)),
            },
            _ => todo!(),
        }
    }
}

impl<N> Path<Filter<N>> {
    pub fn try_map<F, M, E>(self, m: &F) -> Result<Path<Filter<M>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let path = self.0.into_iter().map(|p| p.try_map(m));
        Ok(Path(path.collect::<Result<_, _>>()?))
    }
}

impl<N> PathElem<Filter<N>> {
    fn try_map<F, M, E>(self, m: &F) -> Result<PathElem<Filter<M>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let m = |f: Filter<N>| f.try_map(m);
        use PathElem::*;
        match self {
            Index(i) => Ok(Index(m(i)?)),
            Range(from, until) => Ok(Range(from.map(m).transpose()?, until.map(m).transpose()?)),
        }
    }
}

impl<F> From<PathElem<F>> for Path<F> {
    fn from(p: PathElem<F>) -> Self {
        Path(Vec::from([p]))
    }
}

type Indices<'a> = Box<dyn Iterator<Item = Result<usize, Error>> + 'a>;
type SkipTakeR = Result<(usize, usize), Error>;

fn get_indices(f: &Option<Vec<Rc<Val>>>, len: usize, default: usize) -> Indices<'_> {
    match f {
        Some(f) => Box::new(f.iter().map(move |i| Ok(get_index(i.as_isize()?, len)))),
        None => Box::new(core::iter::once(Ok(default))),
    }
}

fn skip_takes<'a>(from: Indices<'a>, until: Indices<'a>) -> impl Iterator<Item = SkipTakeR> + 'a {
    let until: Vec<_> = until.collect();
    use itertools::Itertools;
    let from_until = from.into_iter().cartesian_product(until);
    from_until.map(move |(from, until)| {
        let from = from?;
        let until = until?;
        let take = if until > from { until - from } else { 0 };
        Ok((from, take))
    })
}

fn get_index(i: isize, len: usize) -> usize {
    // make index 0 if it is smaller than 0
    wrap(i, len).try_into().unwrap_or(0)
}

fn wrap(i: isize, len: usize) -> isize {
    if i < 0 {
        len as isize + i
    } else {
        i
    }
}

#[test]
fn wrap_test() {
    let len = 4;
    assert_eq!(wrap(0, len), 0);
    assert_eq!(wrap(8, len), 8);
    assert_eq!(wrap(-1, len), 3);
    assert_eq!(wrap(-4, len), 0);
    assert_eq!(wrap(-8, len), -4);
}

/*
enum OnError {
    Empty,
    Fail,
}
*/
