use crate::filter::Filter;
use crate::val::{Val, Vals};
use std::rc::Rc;

pub type Path = Vec<PathElem>;

#[derive(Debug)]
pub enum PathElem {
    Index(Filter),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<Filter>, Option<Filter>),
}

impl PathElem {
    pub fn follow(&self, root: Rc<Val>, current: Val) -> Vals {
        match self {
            Self::Index(filter) => {
                let index = filter.run(root);
                match current {
                    Val::Arr(vals) => Box::new(index.map(move |i| match *i {
                        Val::Num(i) => Rc::clone(&vals[i as usize]),
                        _ => panic!("cannot index array with non-numeric value"),
                    })),
                    Val::Obj(o) => Box::new(index.map(move |i| match &*i {
                        Val::Str(s) => Rc::clone(&o[s]),
                        _ => todo!(),
                    })),
                    _ => panic!("index"),
                }
            }
            Self::Range(None, None) => match current {
                Val::Arr(a) => Box::new(a.into_iter()),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

/*
enum OnError {
    Empty,
    Fail,
}
*/
