use crate::{Error, Filter, RValRs, Val};
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
    First(Box<Filter>),
    Last(Box<Filter>),
    Select(Box<Filter>),
    Recurse(Box<Filter>),
}

impl NewFunc {
    pub fn run(&self, v: Rc<Val>) -> Result<Val, Error> {
        use NewFunc::*;
        match self {
            Any => Ok(Val::Bool(v.iter()?.any(|v| v.as_bool()))),
            All => Ok(Val::Bool(v.iter()?.all(|v| v.as_bool()))),
            Not => Ok(Val::Bool(!v.as_bool())),
            Add => v
                .iter()?
                .map(|x| (*x).clone())
                .try_fold(Val::Null, |acc, x| acc + x),
            Length => Ok(Val::Num(v.len()?)),
            Map(f) => Ok(Val::Arr(
                v.iter()?.flat_map(|x| f.run(x)).collect::<Result<_, _>>()?,
            )),
        }
    }
}

impl RefFunc {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        use RefFunc::*;
        match self {
            Empty => Box::new(core::iter::empty()),
            First(f) => Box::new(f.run(v).take(1)),
            Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(None) => Box::new(core::iter::empty()),
                Ok(Some(y)) => Box::new(core::iter::once(Ok(y))),
                Err(e) => Box::new(core::iter::once(Err(e))),
            },
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
