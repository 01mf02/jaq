use crate::val::{Val, ValR, ValRs};
use crate::{Error, Int};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
pub use jaq_parse::path::Opt;

#[derive(Clone, Debug)]
pub struct Path<F>(pub Vec<(Part<F>, Opt)>);

#[derive(Clone, Debug)]
pub enum Part<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

impl<F> Path<F> {
    pub fn new(path: Vec<(Part<F>, Opt)>) -> Self {
        Self(path)
    }
}

impl Part<Vec<Val>> {
    pub fn collect(&self, current: Val) -> ValRs {
        use core::iter::once;
        match self {
            Self::Index(indices) => match current {
                Val::Arr(a) => Box::new(indices.iter().map(move |i| {
                    Ok(abs_index(i.as_int()?, a.len())
                        .map(|i| a[i].clone())
                        .unwrap_or(Val::Null))
                })),
                Val::Obj(o) => Box::new(indices.iter().map(move |i| match i {
                    Val::Str(s) => Ok(o.get(&**s).cloned().unwrap_or(Val::Null)),
                    i => Err(Error::IndexWith(Val::Obj(o.clone()), i.clone())),
                })),
                _ => Box::new(once(Err(Error::Index(current)))),
            },
            Self::Range(None, None) => match current {
                Val::Arr(a) => Box::new((*a).clone().into_iter().map(Ok)),
                Val::Obj(o) => Box::new((*o).clone().into_iter().map(|(_k, v)| Ok(v))),
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
                        Ok(Val::Arr(Rc::new(iter.collect())))
                    }))
                }
                Val::Str(s) => {
                    let len = s.chars().count();
                    let from = rel_bounds(from).map(move |i| Ok(abs_bound(i?, len, 0)));
                    let until = rel_bounds(until).map(move |i| Ok(abs_bound(i?, len, len)));
                    Box::new(prod(from, until).map(move |(from, until)| {
                        let (skip, take) = skip_take(from?, until?);
                        let iter = s.chars().skip(skip).take(take);
                        Ok(Val::Str(Rc::new(iter.collect())))
                    }))
                }
                _ => Box::new(once(Err(Error::Index(current)))),
            },
        }
    }

    pub fn run<'a, 'f, P, F>(mut path: P, v: Val, f: F) -> ValRs<'f>
    where
        P: Iterator<Item = &'a (Self, Opt)> + Clone,
        F: Fn(Val) -> ValRs<'f> + Copy,
    {
        if let Some((p, opt)) = path.next() {
            let f = |v| Self::run(path.clone(), v, f);
            Box::new(core::iter::once(p.map(v, *opt, f)))
        } else {
            f(v)
        }
    }

    pub fn map<F, I>(&self, mut v: Val, opt: Opt, f: F) -> Result<Val, Error>
    where
        F: Fn(Val) -> I,
        I: Iterator<Item = ValR>,
    {
        use core::iter::once;
        use Opt::{Essential, Optional};
        match self {
            Self::Index(indices) => match v {
                Val::Obj(ref mut o) => {
                    let o = Rc::make_mut(o);
                    for i in indices.iter() {
                        use indexmap::map::Entry::*;
                        match (i, opt) {
                            (Val::Str(s), _) => match o.entry(Rc::clone(s)) {
                                Occupied(mut e) => {
                                    match f(e.get().clone()).next().transpose()? {
                                        Some(y) => e.insert(y),
                                        None => e.remove(),
                                    };
                                }
                                Vacant(e) => {
                                    if let Some(y) = f(Val::Null).next().transpose()? {
                                        e.insert(y);
                                    }
                                }
                            },
                            (i, Essential) => return Err(Error::IndexWith(v, i.clone())),
                            (_, Optional) => (),
                        }
                    }
                    Ok(v)
                }
                Val::Arr(ref mut a) => {
                    let a = Rc::make_mut(a);
                    for i in indices.iter() {
                        let abs_or = |i| abs_index(i, a.len()).ok_or(Error::IndexOutOfBounds(i));
                        let i = match (i.as_int().and_then(abs_or), opt) {
                            (Ok(i), _) => i,
                            (Err(e), Essential) => return Err(e),
                            (Err(_), Optional) => continue,
                        };

                        if let Some(y) = f(a[i].clone()).next().transpose()? {
                            a[i] = y;
                        } else {
                            a.remove(i);
                        }
                    }
                    Ok(v)
                }
                _ => opt.fail(v, Error::Index),
            },
            Self::Range(None, None) => match v {
                Val::Arr(a) => Ok(Val::Arr(Rc::new(
                    a.iter().cloned().flat_map(f).collect::<Result<_, _>>()?,
                ))),
                Val::Obj(o) => Ok(Val::Obj(Rc::new(
                    o.iter()
                        .flat_map(|(k, v)| match f(v.clone()).next().transpose() {
                            Ok(y) => Box::new(y.map(|y| Ok((k.clone(), y))).into_iter()),
                            Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                        })
                        .collect::<Result<_, _>>()?,
                ))),
                _ => opt.fail(v, Error::Iter),
            },
            Self::Range(from, until) => match v {
                Val::Arr(ref mut a) => {
                    let a = Rc::make_mut(a);
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
                        let arr = Val::Arr(Rc::new(iter.collect()));
                        let y = match f(arr).next().transpose()? {
                            None => Vec::new(),
                            Some(Val::Arr(y)) => (*y).clone(),
                            Some(y) => return Err(Error::SliceAssign(y)),
                        };
                        a.splice(skip..skip + take, y);
                    }
                    Ok(v)
                }
                _ => opt.fail(v, Error::Iter),
            },
        }
    }
}

impl<F> Path<F> {
    pub fn map<F2>(self, f: impl Fn(F) -> F2) -> Path<F2> {
        let path = self.0.into_iter().map(|(p, opt)| (p.map2(&f), opt));
        Path(path.collect())
    }
}

impl<F> Part<F> {
    pub fn map2<F2>(self, f: impl Fn(F) -> F2) -> Part<F2> {
        use Part::*;
        match self {
            Index(i) => Index(f(i)),
            Range(from, until) => Range(from.map(&f), until.map(&f)),
        }
    }
}

impl<F> From<Part<F>> for Path<F> {
    fn from(p: Part<F>) -> Self {
        Path(Vec::from([(p, Opt::Essential)]))
    }
}

type RelBounds<'a> = Box<dyn Iterator<Item = Result<Option<Int>, Error>> + 'a>;
fn rel_bounds(f: &Option<Vec<Val>>) -> RelBounds<'_> {
    match f {
        Some(f) => Box::new(f.iter().map(move |i| Ok(Some(i.as_int()?)))),
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
fn abs_bound(i: Option<Int>, len: usize, default: usize) -> usize {
    let abs = |i| core::cmp::min(wrap(i, len).unwrap_or(0), len);
    i.map(abs).unwrap_or(default)
}

/// Absolutise an index and return result if it is inside [0, len).
fn abs_index(i: Int, len: usize) -> Option<usize> {
    wrap(i, len).filter(|i| *i < len)
}

fn wrap(i: Int, len: usize) -> Option<usize> {
    if i.is_positive() {
        Some(i.abs())
    } else if len < i.abs() {
        None
    } else {
        Some(len - i.abs())
    }
}

#[test]
fn wrap_test() {
    let len = 4;
    assert_eq!(wrap(Int::from(0), len), Some(0));
    assert_eq!(wrap(Int::from(8), len), Some(8));
    assert_eq!(wrap(-Int::from(1), len), Some(3));
    assert_eq!(wrap(-Int::from(4), len), Some(0));
    assert_eq!(wrap(-Int::from(8), len), None);
}
