use crate::{Error, Filter, RValRs, Val};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryInto;

#[derive(Debug)]
pub struct Path(Vec<PathElem>);

#[derive(Debug)]
pub enum PathElem {
    Index(Filter),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<Filter>, Option<Filter>),
}

fn wrap(i: isize, len: usize) -> isize {
    if i < 0 {
        len as isize + i
    } else {
        i
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn wrap() {
        use super::wrap;
        let len = 4;
        assert_eq!(wrap(0, len), 0);
        assert_eq!(wrap(8, len), 8);
        assert_eq!(wrap(-1, len), 3);
        assert_eq!(wrap(-4, len), 0);
        assert_eq!(wrap(-8, len), -4);
    }
}

fn get_index(i: &Val, len: usize) -> Result<usize, Error> {
    let i = i.as_isize().ok_or(Error::IndexIsize)?;
    // make index 0 if it is smaller than 0
    Ok(wrap(i, len).try_into().unwrap_or(0))
}

fn get_indices(
    f: &Option<Filter>,
    v: Rc<Val>,
    len: usize,
    default: usize,
) -> Box<dyn Iterator<Item = Result<usize, Error>> + '_> {
    match f {
        Some(f) => Box::new(f.run(v).map(move |i| Ok(get_index(&*i?, len)?))),
        None => Box::new(core::iter::once(Ok(default))),
    }
}

impl Path {
    pub fn new(path: impl Iterator<Item = PathElem>) -> Self {
        Self(path.collect())
    }

    pub fn run(&self, v: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        let mut path = self.0.iter();
        path.try_fold(Vec::from([Rc::clone(&v)]), |acc, p| {
            acc.into_iter()
                .flat_map(|x| p.follow(Rc::clone(&v), (*x).clone()))
                .collect()
        })
    }
}

impl PathElem {
    pub fn follow(&self, root: Rc<Val>, current: Val) -> RValRs {
        use core::iter::once;
        match self {
            Self::Index(filter) => {
                let index = filter.run(root);
                match current {
                    Val::Arr(a) => Box::new(index.map(move |i| {
                        let i = wrap(i?.as_isize().ok_or(Error::IndexIsize)?, a.len());
                        Ok(if i < 0 || i as usize >= a.len() {
                            Rc::new(Val::Null)
                        } else {
                            Rc::clone(&a[i as usize])
                        })
                    })),
                    Val::Obj(o) => Box::new(index.map(move |i| match &*i? {
                        Val::Str(s) => Ok(o.get(s).map_or_else(|| Rc::new(Val::Null), Rc::clone)),
                        i => Err(Error::IndexWith(Val::Obj(o.clone()), i.clone())),
                    })),
                    _ => Box::new(once(Err(Error::Index(current)))),
                }
            }
            Self::Range(None, None) => match current {
                Val::Arr(a) => Box::new(a.into_iter().map(Ok)),
                Val::Obj(o) => Box::new(o.into_iter().map(|(_k, v)| Ok(v))),
                v => Box::new(once(Err(Error::Iter(v)))),
            },
            Self::Range(from, until) => match current {
                Val::Arr(a) => {
                    let from = get_indices(from, Rc::clone(&root), a.len(), 0);
                    let until = get_indices(until, root, a.len(), a.len());
                    let until: Vec<_> = until.collect();
                    use itertools::Itertools;
                    let from_until = from.into_iter().cartesian_product(until);
                    Box::new(from_until.map(move |(from, until)| {
                        let from = from?;
                        let until = until?;
                        let take = if until > from { until - from } else { 0 };
                        let iter = a.iter().cloned().skip(from).take(take);
                        Ok(Rc::new(Val::Arr(iter.collect())))
                    }))
                }
                Val::Str(_) => todo!(),
                _ => Box::new(once(Err(Error::Index(current)))),
            },
        }
    }
}

/*
enum OnError {
    Empty,
    Fail,
}
*/
