use crate::box_iter::{box_once, flat_map_with, map_with, BoxIter};
use crate::error::{Error, Type};
use crate::results::then;
use crate::val::{Val, ValR, ValRs};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
pub use jaq_syn::path::Opt;

#[derive(Clone, Debug)]
pub struct Path<F>(pub Vec<(Part<F>, Opt)>);

#[derive(Clone, Debug)]
pub enum Part<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

impl Path<Vec<Val>> {
    pub fn update<'f>(&self, v: Val, f: impl Fn(Val) -> ValRs<'f> + Copy) -> ValRs<'f> {
        Part::update(self.0.iter(), v, f)
    }
}

impl Part<Val> {
    fn collect(self, current: Val) -> Box<dyn Iterator<Item = ValR>> {
        match self {
            Self::Index(idx) => box_once(match current {
                Val::Arr(a) => match idx {
                    Val::Int(i) => Ok(abs_index(i, a.len())
                        .map(|i| a[i].clone())
                        .unwrap_or(Val::Null)),
                    i => Err(Error::Index(Val::Arr(a.clone()), i.clone())),
                },
                Val::Obj(o) => match idx {
                    Val::Str(s) => Ok(o.get(&*s).cloned().unwrap_or(Val::Null)),
                    i => Err(Error::Index(Val::Obj(o.clone()), i.clone())),
                },
                _ => Err(Error::Type(current, Type::Iter)),
            }),
            Self::Range(None, None) => then(current.try_into_iter(), |iter| Box::new(iter.map(Ok))),
            Self::Range(from, upto) => box_once(match current {
                Val::Arr(a) => {
                    let len = a.len();
                    let from = from.as_ref().map(|i| i.as_int()).transpose();
                    let upto = upto.as_ref().map(|i| i.as_int()).transpose();
                    from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
                        let from = abs_bound(from, len, 0);
                        let upto = abs_bound(upto, len, len);
                        let (skip, take) = skip_take(from, upto);
                        Val::arr(a.iter().skip(skip).take(take).cloned().collect())
                    })
                }
                Val::Str(s) => {
                    let len = s.chars().count();
                    let from = from.as_ref().map(|i| i.as_int()).transpose();
                    let upto = upto.as_ref().map(|i| i.as_int()).transpose();
                    from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
                        let from = abs_bound(from, len, 0);
                        let upto = abs_bound(upto, len, len);
                        let (skip, take) = skip_take(from, upto);
                        Val::str(s.chars().skip(skip).take(take).collect())
                    })
                }
                _ => Err(Error::Type(current, Type::Range)),
            }),
        }
    }

    pub fn update2<'a, 'f, P, F>(mut path: P, v: Val, f: F) -> ValRs<'f>
    where
        P: Iterator<Item = &'a (Self, Opt)> + Clone,
        F: Fn(Val) -> ValRs<'f> + Copy,
    {
        if let Some((p, opt)) = path.next() {
            let f = |v| Self::update2(path.clone(), v, f);
            Box::new(core::iter::once(p.map(v, *opt, f)))
        } else {
            f(v)
        }
    }

    pub fn map<F, I>(&self, mut v: Val, opt: Opt, f: F) -> ValR
    where
        F: Fn(Val) -> I,
        I: Iterator<Item = ValR>,
    {
        match self {
            Self::Index(idx) => match v {
                Val::Obj(ref mut o) => {
                    let o = Rc::make_mut(o);
                    use indexmap::map::Entry::{Occupied, Vacant};
                    let i = match idx {
                        Val::Str(s) => s,
                        i => return opt.fail(v, |v| Error::Index(v, i.clone())),
                    };
                    match o.entry(Rc::clone(i)) {
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
                    }
                    Ok(v)
                }
                Val::Arr(ref mut a) => {
                    let a = Rc::make_mut(a);
                    let abs_or = |i| abs_index(i, a.len()).ok_or(Error::IndexOutOfBounds(i));
                    let i = match idx.as_int().and_then(abs_or) {
                        Ok(i) => i,
                        Err(e) => return opt.fail(v, |_| e),
                    };

                    if let Some(y) = f(a[i].clone()).next().transpose()? {
                        a[i] = y;
                    } else {
                        a.remove(i);
                    }
                    Ok(v)
                }
                _ => opt.fail(v, |v| Error::Type(v, Type::Iter)),
            },
            Self::Range(None, None) => match v.try_map(&f)? {
                y @ (Val::Arr(_) | Val::Obj(_)) => Ok(y),
                v => opt.fail(v, |v| Error::Type(v, Type::Iter)),
            },
            Self::Range(from, upto) => match v {
                Val::Arr(ref mut a) => {
                    let a = Rc::make_mut(a);
                    let from = from.as_ref().map(|i| i.as_int()).transpose();
                    let upto = upto.as_ref().map(|i| i.as_int()).transpose();
                    let (from, upto) = match from.and_then(|from| Ok((from, upto?))) {
                        Ok(from_upto) => from_upto,
                        Err(e) => return opt.fail(v, |_| e),
                    };
                    let len = a.len();
                    let from = abs_bound(from, len, 0);
                    let upto = abs_bound(upto, len, len);
                    let (skip, take) = skip_take(from, upto);
                    let arr = Val::arr(a.iter().skip(skip).take(take).cloned().collect());
                    let y = f(arr).map(|y| y?.into_arr()).next().transpose()?;
                    a.splice(skip..skip + take, (*y.unwrap_or_default()).clone());
                    Ok(v)
                }
                _ => opt.fail(v, |v| Error::Type(v, Type::Arr)),
            },
        }
    }
}

impl Part<Vec<Val>> {
    pub fn update<'a, 'f, P, F>(mut path: P, v: Val, f: F) -> ValRs<'f>
    where
        P: Iterator<Item = &'a (Self, Opt)> + Clone,
        F: Fn(Val) -> ValRs<'f> + Copy,
    {
        if let Some((p, opt)) = path.next() {
            let f = |v| Self::update(path.clone(), v, f);
            Box::new(core::iter::once(p.map(v, *opt, f)))
        } else {
            f(v)
        }
    }

    pub fn map<F, I>(&self, mut v: Val, opt: Opt, f: F) -> ValR
    where
        F: Fn(Val) -> I,
        I: Iterator<Item = ValR>,
    {
        use Opt::{Essential, Optional};
        match self {
            Self::Index(indices) => match v {
                Val::Obj(ref mut o) => {
                    let o = Rc::make_mut(o);
                    for i in indices.iter() {
                        use indexmap::map::Entry::{Occupied, Vacant};
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
                            (i, Essential) => return Err(Error::Index(v, i.clone())),
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
                _ => opt.fail(v, |v| Error::Type(v, Type::Iter)),
            },
            Self::Range(None, None) => match v.try_map(&f)? {
                y @ (Val::Arr(_) | Val::Obj(_)) => Ok(y),
                v => opt.fail(v, |v| Error::Type(v, Type::Iter)),
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
                        let arr = Val::arr(a.iter().skip(skip).take(take).cloned().collect());
                        let y = f(arr).map(|y| y?.into_arr()).next().transpose()?;
                        a.splice(skip..skip + take, (*y.unwrap_or_default()).clone());
                    }
                    Ok(v)
                }
                _ => opt.fail(v, |v| Error::Type(v, Type::Arr)),
            },
        }
    }
}

impl<F> Path<F> {
    pub fn eval<'a>(&'a self, run: impl Fn(&'a F) -> ValRs<'a>) -> Result<Path<Vec<Val>>, Error> {
        let path = self.0.iter().map(|(p, opt)| Ok((p.eval(&run)?, *opt)));
        Ok(Path(path.collect::<Result<_, _>>()?))
    }
}

pub fn apply_path<'a, F: Clone + 'a>(
    mut iter: impl Iterator<Item = (Part<F>, Opt)> + Clone + 'a,
    run: impl Fn(F) -> ValRs<'a> + Clone + 'a,
    val: Val,
) -> ValRs<'a> {
    if let Some((part, opt)) = iter.next() {
        let cond = move |v: &ValR| matches!(opt, Opt::Essential) || v.is_ok();
        let part = flat_map_with(part.eval2(run.clone()), val, move |part, val| {
            then(part, |part| Box::new(part.collect(val).filter(cond)))
        });
        flat_map_with(part, (iter, run), move |v, (iter, run)| {
            then(v, |v| apply_path(iter, run, v))
        })
    } else {
        box_once(Ok(val))
    }
}

impl<'a, T: Clone + 'a> Part<T> {
    pub fn eval2<U: Clone + 'a, E: Clone + 'a, F>(self, run: F) -> BoxIter<'a, Result<Part<U>, E>>
    where
        F: Fn(T) -> BoxIter<'a, Result<U, E>> + 'a,
    {
        use Part::{Index, Range};
        match self {
            Index(i) => Box::new(run(i).map(|i| Ok(Index(i?)))),
            Range(None, None) => box_once(Ok(Range(None, None))),
            Range(Some(from), None) => Box::new(run(from).map(|from| Ok(Range(Some(from?), None)))),
            Range(None, Some(upto)) => Box::new(run(upto).map(|upto| Ok(Range(None, Some(upto?))))),
            Range(Some(from), Some(upto)) => flat_map_with(run(from), upto, move |from, upto| {
                map_with(run(upto), from, move |upto, from| {
                    Ok(Range(Some(from?), Some(upto?)))
                })
            }),
        }
    }
}

impl<F> Part<F> {
    pub fn as_ref(&self) -> Part<&F> {
        match self {
            Self::Index(i) => Part::Index(i),
            Self::Range(from, upto) => Part::Range(from.as_ref(), upto.as_ref()),
        }
    }

    fn eval<'a>(&'a self, run: impl Fn(&'a F) -> ValRs<'a>) -> Result<Part<Vec<Val>>, Error> {
        use Part::{Index, Range};
        match self {
            Index(i) => Ok(Index(run(i).collect::<Result<_, _>>()?)),
            Range(from, until) => {
                let from = from.as_ref().map(|f| run(f).collect());
                let until = until.as_ref().map(|u| run(u).collect());
                Ok(Range(from.transpose()?, until.transpose()?))
            }
        }
    }
}

impl<F> From<Part<F>> for Path<F> {
    fn from(p: Part<F>) -> Self {
        Self(Vec::from([(p, Opt::Essential)]))
    }
}

type RelBounds<'a> = Box<dyn Iterator<Item = Result<Option<isize>, Error>> + 'a>;
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
    l.flat_map(move |l| r.clone().into_iter().map(move |r| (l.clone(), r)))
}

fn skip_take(from: usize, until: usize) -> (usize, usize) {
    (from, if until > from { until - from } else { 0 })
}

/// If a range bound is given, absolutise and clip it between 0 and `len`,
/// else return `default`.
fn abs_bound(i: Option<isize>, len: usize, default: usize) -> usize {
    let abs = |i| core::cmp::min(wrap(i, len).unwrap_or(0), len);
    i.map(abs).unwrap_or(default)
}

/// Absolutise an index and return result if it is inside [0, len).
fn abs_index(i: isize, len: usize) -> Option<usize> {
    wrap(i, len).filter(|i| *i < len)
}

fn wrap(i: isize, len: usize) -> Option<usize> {
    if i >= 0 {
        Some(i as usize)
    } else if len < -i as usize {
        None
    } else {
        Some(len - (-i as usize))
    }
}

#[test]
fn wrap_test() {
    let len = 4;
    assert_eq!(wrap(0, len), Some(0));
    assert_eq!(wrap(8, len), Some(8));
    assert_eq!(wrap(-1, len), Some(3));
    assert_eq!(wrap(-4, len), Some(0));
    assert_eq!(wrap(-8, len), None);
}
