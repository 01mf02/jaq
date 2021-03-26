use crate::{Error, Filter, RValRs, Val, ValR};
use alloc::{boxed::Box, rc::Rc, string::ToString, vec::Vec};

#[derive(Debug)]
pub enum NewFunc {
    Not,
    All,
    Any,
    Add,
    Length,
    Type,
    Map(Box<Filter>),
}

#[derive(Debug)]
pub enum RefFunc {
    First(Box<Filter>),
    Last(Box<Filter>),
    Limit(Box<Filter>, Box<Filter>),
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
            Type => Ok(Val::Str(v.typ().to_string())),
            Map(f) => Ok(Val::Arr(
                v.iter()?.flat_map(|x| f.run(x)).collect::<Result<_, _>>()?,
            )),
        }
    }
}

impl RefFunc {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::{empty, once};
        use RefFunc::*;
        match self {
            First(f) => Box::new(f.run(v).take(1)),
            Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(Some(y)) => Box::new(once(Ok(y))),
                Ok(None) => Box::new(empty()),
                Err(e) => Box::new(once(Err(e))),
            },
            Limit(n, f) => {
                let n = n.run(Rc::clone(&v)).map(|n| n.and_then(|n| n.as_isize()));
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) if n < 0 => f.run(Rc::clone(&v)),
                    Ok(n) => Box::new(f.run(Rc::clone(&v)).take(n as usize)),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
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
