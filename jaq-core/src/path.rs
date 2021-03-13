use crate::filter::{Filter, FilterT};
use crate::val::{Val, Vals};
use std::convert::TryInto;
use std::rc::Rc;

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

fn get_index(i: &Val, len: usize) -> usize {
    wrap(i.as_isize().unwrap(), len).try_into().unwrap_or(0)
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
                _ => todo!(),
            },
            Self::Range(from, until) => match current {
                Val::Arr(a) => {
                    use core::iter::once;
                    let len = a.len();
                    let from = match from {
                        Some(from) => {
                            Box::new(from.run(Rc::clone(&root)).map(move |i| get_index(&i, len)))
                        }
                        None => Box::new(once(0 as usize)) as Box<dyn Iterator<Item = _>>,
                    };
                    let until = match until {
                        Some(until) => Box::new(until.run(root).map(|i| get_index(&i, len))),
                        None => Box::new(once(a.len())) as Box<dyn Iterator<Item = _>>,
                    };
                    let until: Vec<_> = until.collect();
                    use itertools::Itertools;
                    let from_until = from.into_iter().cartesian_product(until);
                    Box::new(from_until.map(move |(from, until)| {
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
