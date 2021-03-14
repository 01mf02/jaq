use crate::filter::{Filter, FilterT};
use crate::val::{Val, Vals};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryInto;

pub type Path = Vec<PathElem>;

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

fn get_index(i: &Val, len: usize) -> Result<usize, ()> {
    let i = i.as_isize().ok_or(())?;
    // make index 0 if it is smaller than 0
    Ok(wrap(i, len).try_into().unwrap_or(0))
}

fn get_indices(
    f: &Option<Filter>,
    v: Rc<Val>,
    len: usize,
    default: usize,
) -> Box<dyn Iterator<Item = Result<usize, ()>> + '_> {
    match f {
        Some(f) => Box::new(f.run(v).map(move |i| get_index(&i, len))),
        None => Box::new(core::iter::once(Ok(default))),
    }
}

impl PathElem {
    pub fn follow(&self, root: Rc<Val>, current: Val) -> Vals {
        match self {
            Self::Index(filter) => {
                let index = filter.run(root);
                match current {
                    Val::Arr(a) => Box::new(index.map(move |i| {
                        let i = wrap(i.as_isize().unwrap(), a.len());
                        if i < 0 || i as usize >= a.len() {
                            Rc::new(Val::Null)
                        } else {
                            Rc::clone(&a[i as usize])
                        }
                    })),
                    Val::Obj(o) => Box::new(index.map(move |i| match &*i {
                        Val::Str(s) => Rc::clone(&o.get(s).unwrap()),
                        _ => todo!(),
                    })),
                    _ => panic!("index"),
                }
            }
            Self::Range(None, None) => match current {
                Val::Arr(a) => Box::new(a.into_iter()),
                Val::Obj(o) => Box::new(o.into_iter().map(|(_k, v)| v)),
                Val::Str(s) => panic!("cannot iterate over string (\"{}\")", s),
                Val::Num(n) => panic!("cannot iterate over number ({})", n),
                Val::Bool(b) => panic!("cannot iterate over boolean ({})", b),
                Val::Null => panic!("cannot iterate over null"),
            },
            Self::Range(from, until) => match current {
                Val::Arr(a) => {
                    let from = get_indices(from, Rc::clone(&root), a.len(), 0);
                    let until = get_indices(until, root, a.len(), a.len());
                    let until: Vec<_> = until.collect();
                    use itertools::Itertools;
                    let from_until = from.into_iter().cartesian_product(until);
                    Box::new(from_until.map(move |(from, until)| {
                        let from = from.unwrap();
                        let until = until.unwrap();
                        let take = if until > from { until - from } else { 0 };
                        Rc::new(Val::Arr(a.iter().cloned().skip(from).take(take).collect()))
                    }))
                }
                Val::Str(_) => todo!(),
                _ => panic!("cannot obtain slice of element (neither array nor string)"),
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
