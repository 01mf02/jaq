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

impl<'a, U: Clone + 'a> Path<U> {
    pub fn combinations<I, F>(self, mut iter: I) -> BoxIter<'a, Self>
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

pub fn run<'a, I>(mut iter: I, val: Val) -> ValRs<'a>
where
    I: Iterator<Item = (Part<Val>, Opt)> + Clone + 'a,
{
    if let Some((part, opt)) = iter.next() {
        let essential = matches!(opt, Opt::Essential);
        let ys = part.run(val).filter(move |v| essential || v.is_ok());
        flat_map_with(ys, iter, move |v, iter| then(v, |v| run(iter, v)))
    } else {
        box_once(Ok(val))
    }
}

pub fn update<'f, P, F>(mut iter: P, last: (Part<Val>, Opt), v: Val, f: F) -> ValR
where
    P: Iterator<Item = (Part<Val>, Opt)> + Clone,
    F: Fn(Val) -> ValRs<'f> + Copy,
{
    if let Some((part, opt)) = iter.next() {
        use core::iter::once;
        part.update(v, opt, |v| once(update(iter.clone(), last.clone(), v, f)))
    } else {
        last.0.update(v, last.1, f)
    }
}

impl Part<Val> {
    fn run(self, current: Val) -> Box<dyn Iterator<Item = ValR>> {
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

    fn update<F, I>(&self, mut v: Val, opt: Opt, f: F) -> ValR
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
                    std::dbg!(Rc::strong_count(&a));
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

impl<T> Part<T> {
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Part<U> {
        use Part::{Index, Range};
        match self {
            Index(i) => Index(f(i)),
            Range(from, upto) => Range(from.map(&mut f), upto.map(&mut f)),
        }
    }
}

impl<T, E> Path<Result<T, E>> {
    pub fn transpose(self) -> Result<Path<T>, E> {
        self.0
            .into_iter()
            .map(|(part, opt)| Ok((part.transpose()?, opt)))
            .collect::<Result<_, _>>()
            .map(Path)
    }
}

impl<T, E> Part<Result<T, E>> {
    pub fn transpose(self) -> Result<Part<T>, E> {
        match self {
            Self::Index(i) => Ok(Part::Index(i?)),
            Self::Range(from, upto) => Ok(Part::Range(from.transpose()?, upto.transpose()?)),
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
}

impl<F> From<Part<F>> for Path<F> {
    fn from(p: Part<F>) -> Self {
        Self(Vec::from([(p, Opt::Essential)]))
    }
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
