use crate::{ClosedFilter, Error, Filter, RValR, RValRs, Val};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryInto;

#[derive(Clone, Debug)]
pub struct Path<F>(pub Vec<(PathElem<F>, Opt)>);

#[derive(Clone, Debug)]
pub enum PathElem<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

#[derive(Copy, Clone, Debug)]
pub enum Opt {
    Optional,
    Essential,
}

impl Opt {
    /// If the value is optional, return `x`, else fail with `f(x)`.
    fn fail<T, E>(self, x: T, f: impl FnOnce(T) -> E) -> Result<T, E> {
        match self {
            Self::Optional => Ok(x),
            Self::Essential => Err(f(x)),
        }
    }

    fn collect<T, E>(self, iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
        match self {
            Self::Optional => Ok(iter.filter_map(|x| x.ok()).collect()),
            Self::Essential => iter.collect(),
        }
    }
}

impl<F> Path<F> {
    pub fn new(path: Vec<(PathElem<F>, Opt)>) -> Self {
        Self(path)
    }
}

type PathOptR = Result<(PathElem<Vec<Rc<Val>>>, Opt), Error>;

impl Path<ClosedFilter> {
    pub fn collect(&self, v: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        let init = Vec::from([Rc::clone(&v)]);
        self.run_indices(&v).try_fold(init, |acc, p_opt| {
            let (p, opt) = p_opt?;
            opt.collect(acc.into_iter().flat_map(|x| p.collect((*x).clone())))
        })
    }

    pub fn run<'f, F>(&self, v: Rc<Val>, f: F) -> RValRs<'f>
    where
        F: Fn(Rc<Val>) -> RValRs<'f> + Copy,
    {
        match self.run_indices(&v).collect::<Result<Vec<_>, _>>() {
            Ok(path) => PathElem::run(path.iter(), v, f),
            Err(e) => Box::new(core::iter::once(Err(e))),
        }
    }

    fn run_indices<'a>(&'a self, v: &'a Rc<Val>) -> impl Iterator<Item = PathOptR> + 'a {
        let path = self.0.iter();
        path.map(move |(p, opt)| Ok((p.run_indices(Rc::clone(&v))?, *opt)))
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
                    Ok(if let Some(i) = abs_index(i.as_isize()?, a.len()) {
                        Rc::clone(&a[i])
                    } else {
                        Rc::new(Val::Null)
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
                    let len = a.len();
                    let from = rel_bounds(from).map(move |i| Ok(abs_bound(i?, len, 0)));
                    let until = rel_bounds(until).map(move |i| Ok(abs_bound(i?, len, len)));
                    Box::new(prod(from, until).map(move |(from, until)| {
                        let (skip, take) = skip_take(from?, until?);
                        let iter = a.iter().cloned().skip(skip).take(take);
                        Ok(Rc::new(Val::Arr(iter.collect())))
                    }))
                }
                Val::Str(s) => {
                    let len = s.chars().count();
                    let from = rel_bounds(from).map(move |i| Ok(abs_bound(i?, len, 0)));
                    let until = rel_bounds(until).map(move |i| Ok(abs_bound(i?, len, len)));
                    Box::new(prod(from, until).map(move |(from, until)| {
                        let (skip, take) = skip_take(from?, until?);
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
        P: Iterator<Item = &'a (Self, Opt)> + Clone,
        F: Fn(Rc<Val>) -> RValRs<'f> + Copy,
    {
        if let Some((p, opt)) = path.next() {
            let f = |v| Self::run(path.clone(), v, f);
            Box::new(core::iter::once(p.map((*v).clone(), *opt, f).map(Rc::new)))
        } else {
            f(v)
        }
    }

    pub fn map<F, I>(&self, v: Val, opt: Opt, f: F) -> Result<Val, Error>
    where
        F: Fn(Rc<Val>) -> I,
        I: Iterator<Item = RValR>,
    {
        use core::iter::once;
        use Opt::{Essential, Optional};
        match self {
            Self::Index(indices) => match v {
                Val::Obj(mut o) => {
                    let some = |v: &Rc<Val>| f(Rc::clone(v)).next().transpose();
                    let none = || f(Rc::new(Val::Null)).next().transpose();

                    for i in indices.iter() {
                        match (&**i, opt) {
                            (Val::Str(s), _) => o.insert_or_remove(s.clone(), some, none)?,
                            (i, Essential) => return Err(Error::IndexWith(Val::Obj(o), i.clone())),
                            (_, Optional) => continue,
                        }
                    }
                    Ok(Val::Obj(o))
                }
                Val::Arr(mut a) => {
                    for i in indices.iter() {
                        let abs_or = |i| abs_index(i, a.len()).ok_or(Error::IndexOutOfBounds(i));
                        let i = match (i.as_isize().and_then(abs_or), opt) {
                            (Ok(i), _) => i,
                            (Err(e), Essential) => return Err(e),
                            (Err(_), Optional) => continue,
                        };

                        if let Some(y) = f(Rc::clone(&a[i])).next().transpose()? {
                            a[i] = y;
                        } else {
                            a.remove(i);
                        }
                    }
                    Ok(Val::Arr(a))
                }
                _ => opt.fail(v, Error::Index),
            },
            Self::Range(None, None) => match v {
                Val::Arr(a) => Ok(Val::Arr(
                    a.into_iter().flat_map(f).collect::<Result<_, _>>()?,
                )),
                Val::Obj(o) => Ok(Val::Obj(
                    o.into_iter()
                        .flat_map(|(k, v)| match f(v).next().transpose() {
                            Ok(y) => Box::new(y.map(|y| Ok((k, y))).into_iter()),
                            Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                        })
                        .collect::<Result<_, _>>()?,
                )),
                _ => opt.fail(v, Error::Iter),
            },
            Self::Range(from, until) => match v {
                Val::Arr(mut a) => {
                    for (from, until) in prod(rel_bounds(from), rel_bounds(until)) {
                        let (from, until) = match (from.and_then(|from| Ok((from, until?))), opt) {
                            (Ok(from_until), _) => from_until,
                            (Err(e), Essential) => return Err(e),
                            (Err(_), Optional) => continue,
                        };

                        let len = a.len();
                        let from = abs_bound(from, len, 0);
                        let until = abs_bound(until, len, len);

                        let (skip, take) = skip_take(from, until);
                        let iter = a.iter().cloned().skip(skip).take(take);
                        let arr = Rc::new(Val::Arr(iter.collect()));
                        let y = match f(arr).next().transpose()? {
                            None => Vec::new(),
                            Some(y) => match &*y {
                                Val::Arr(y) => y.clone(),
                                _ => return Err(Error::SliceAssign((*y).clone())),
                            },
                        };
                        a.splice(skip..skip + take, y);
                    }
                    Ok(Val::Arr(a))
                }
                _ => opt.fail(v, Error::Iter),
            },
        }
    }
}

impl<N> Path<Filter<N>> {
    pub fn try_map<F, M, E>(self, m: &F) -> Result<Path<Filter<M>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let path = self.0.into_iter().map(|(p, opt)| Ok((p.try_map(m)?, opt)));
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
        Path(Vec::from([(p, Opt::Essential)]))
    }
}

type RelBounds<'a> = Box<dyn Iterator<Item = Result<Option<isize>, Error>> + 'a>;
fn rel_bounds(f: &Option<Vec<Rc<Val>>>) -> RelBounds<'_> {
    match f {
        Some(f) => Box::new(f.iter().map(move |i| Ok(Some(i.as_isize()?)))),
        None => Box::new(core::iter::once(Ok(None))),
    }
}

fn prod<'a, T: 'a + Clone>(
    l: impl Iterator<Item = T> + 'a,
    r: impl Iterator<Item = T> + 'a,
) -> impl Iterator<Item = (T, T)> + 'a {
    let r: Vec<_> = r.collect();
    use itertools::Itertools;
    l.into_iter().cartesian_product(r)
}

fn skip_take(from: usize, until: usize) -> (usize, usize) {
    (from, if until > from { until - from } else { 0 })
}

/// If a range bound is given, absolutise and clip it between 0 and `len`,
/// else return `default`.
fn abs_bound(i: Option<isize>, len: usize, default: usize) -> usize {
    let abs = |i| core::cmp::min(wrap(i, len).try_into().unwrap_or(0), len);
    i.map(abs).unwrap_or(default)
}

/// Absolutise an index and return result if it is inside [0, len).
fn abs_index(i: isize, len: usize) -> Option<usize> {
    wrap(i, len).try_into().ok().filter(|i| *i < len)
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
