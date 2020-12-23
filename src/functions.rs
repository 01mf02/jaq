use crate::filter::{Filter, FilterT};
use crate::val::{Val, Vals};
use std::rc::Rc;

#[derive(Debug)]
pub enum NewFunc {
    Not,
    All,
    Any,
    Add,
    Length,
    Map(Box<Filter>),
}

#[derive(Debug)]
pub enum RefFunc {
    Empty,
    Select(Box<Filter>),
    Recurse(Box<Filter>),
}

impl FilterT for NewFunc {
    fn run(&self, v: Rc<Val>) -> Vals {
        use NewFunc::*;
        match self {
            Any => Val::Bool(v.iter().unwrap().any(|v| v.as_bool())).into(),
            All => Val::Bool(v.iter().unwrap().all(|v| v.as_bool())).into(),
            Not => Val::Bool(!v.as_bool()).into(),
            Add => {
                let iter = v.iter().unwrap().map(|x| (*x).clone());
                iter.fold(Val::Null, |acc, x| (acc + x).unwrap()).into()
            }
            Length => Val::Num(v.len().unwrap()).into(),
            Map(f) => {
                let iter = v.iter().unwrap().flat_map(move |x| f.run(x));
                Box::new(core::iter::once(Rc::new(Val::Arr(iter.collect()))))
            }
        }
    }
}

impl FilterT for RefFunc {
    fn run(&self, v: Rc<Val>) -> Vals {
        use core::iter::{empty, once};
        use RefFunc::*;
        match self {
            Empty => Box::new(empty()),
            Select(f) => {
                let iter = f.run(Rc::clone(&v)).flat_map(|y| {
                    if y.as_bool() {
                        Box::new(once(Rc::clone(&v)))
                    } else {
                        Box::new(empty()) as Box<dyn Iterator<Item = _>>
                    }
                });
                Box::new(iter.collect::<Vec<_>>().into_iter())
            }
            Recurse(f) => Box::new(crate::Recurse::new(f, v)),
        }
    }
}
