use crate::filter::{Filter, FilterT};
use crate::val::{Val, Vals};
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
        use RefFunc::*;
        match self {
            Empty => Box::new(core::iter::empty()),
            Select(f) => Box::new(f.run(Rc::clone(&v)).filter_map(move |y| {
                if y.as_bool() {
                    Some(Rc::clone(&v))
                } else {
                    None
                }
            })),
            Recurse(f) => Box::new(crate::Recurse::new(f, v)),
        }
    }
}
