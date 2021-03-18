use crate::filter::{Filter, FilterT};
use crate::val::{RVals, Val};
use alloc::{boxed::Box, rc::Rc};

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
    // TODO: all these function return at most a single value
    fn run(&self, v: Rc<Val>) -> RVals {
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
                let arr: Result<Vec<_>, _> = iter.collect();
                let arr = arr.map(|arr| Rc::new(Val::Arr(arr)));
                Box::new(core::iter::once(arr))
            }
        }
    }
}

impl FilterT for RefFunc {
    fn run(&self, v: Rc<Val>) -> RVals {
        use RefFunc::*;
        match self {
            Empty => Box::new(core::iter::empty()),
            Select(f) => Box::new(f.run(Rc::clone(&v)).filter_map(move |y| match y {
                Ok(y) => {
                    if y.as_bool() {
                        Some(Ok(Rc::clone(&v)))
                    } else {
                        None
                    }
                }
                Err(e) => Some(Err(e)),
            })),
            Recurse(f) => Box::new(crate::Recurse::new(f, v)),
        }
    }
}
