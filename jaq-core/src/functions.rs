use crate::{Error, Filter, RValRs, Val, ValR};
use alloc::{boxed::Box, rc::Rc};

#[derive(Debug)]
pub enum NewFunc {
    Not,
    All,
    Any,
    Add,
    Length,
    Map(Box<Filter>),
    Fold(Box<Filter>, Box<Filter>),
}

#[derive(Debug)]
pub enum RefFunc {
    Empty,
    First(Box<Filter>),
    Last(Box<Filter>),
    Select(Box<Filter>),
    Recurse(Box<Filter>),
    Fold(Box<Filter>, Box<Filter>, Box<Filter>),
}

impl NewFunc {
    pub fn run(&self, v: Rc<Val>) -> ValR {
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
            Fold(init, f) => {
                let init: Result<Vec<_>, _> = init.run(Rc::clone(&v)).collect();
                let out: Result<_, _> = v.iter()?.try_fold(init?, |acc, x| {
                    acc.into_iter()
                        .flat_map(|acc| {
                            let obj = [("acc".to_string(), acc), ("x".to_string(), Rc::clone(&x))];
                            let obj = Val::Obj(Vec::from(obj).into_iter().collect());
                            f.run(Rc::new(obj))
                        })
                        .collect()
                });
                Ok(Val::Arr(out?))
            }
        }
    }
}

impl RefFunc {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::{empty, once};
        use RefFunc::*;
        match self {
            Empty => Box::new(empty()),
            First(f) => Box::new(f.run(v).take(1)),
            Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(Some(y)) => Box::new(once(Ok(y))),
                Ok(None) => Box::new(empty()),
                Err(e) => Box::new(once(Err(e))),
            },
            Select(f) => Box::new(f.run(Rc::clone(&v)).filter_map(move |y| match y {
                Ok(y) if y.as_bool() => Some(Ok(Rc::clone(&v))),
                Ok(_) => None,
                Err(e) => Some(Err(e)),
            })),
            Recurse(f) => Box::new(crate::Recurse::new(f, v)),
            Fold(xs, init, f) => {
                let mut xs = xs.run(Rc::clone(&v));
                let init: Result<Vec<_>, _> = init.run(Rc::clone(&v)).collect();
                match init.and_then(|init| xs.try_fold(init, |acc, x| Ok(f.fold_step(acc, x?)?))) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }
        }
    }
}

impl Filter {
    fn fold_step(&self, acc: Vec<Rc<Val>>, x: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        acc.into_iter()
            .map(|acc| {
                let obj = [("acc".to_string(), acc), ("x".to_string(), Rc::clone(&x))];
                Val::Obj(Vec::from(obj).into_iter().collect())
            })
            .flat_map(|obj| self.run(Rc::new(obj)))
            .collect()
    }
}
