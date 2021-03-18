use crate::functions::{NewFunc, RefFunc};
use crate::ops::{LogicOp, MathOp};
use crate::val::{Atom, RVal, RVals, Val};
use crate::{Error, Path};
use alloc::{boxed::Box, rc::Rc, vec::Vec};

pub trait FilterT: core::fmt::Debug {
    fn run(&self, v: Rc<Val>) -> RVals;
}

#[derive(Debug)]
pub enum Filter {
    Atom(Atom),
    Array(Box<Filter>),
    Object(Vec<(Filter, Filter)>),
    Math(Box<Filter>, MathOp, Box<Filter>),
    Logic(Box<Filter>, LogicOp, Box<Filter>),
    Function(NewFunc),
    Ref(Ref<Box<Filter>>),
}

#[derive(Debug)]
pub enum Ref<F> {
    Pipe(F, F),
    Comma(F, F),
    Empty,
    Path(Path),
    IfThenElse(Box<Filter>, F, F),
    Function(RefFunc),
}

impl Val {
    fn as_obj_key(&self) -> Result<String, Error> {
        self.as_string().ok_or_else(|| Error::ObjKey(self.clone()))
    }
}

type Product = (RVal, RVal);

impl FilterT for Filter {
    fn run(&self, v: Rc<Val>) -> RVals {
        match self {
            Self::Atom(a) => Val::from(a.clone()).into(),
            Self::Array(f) => match f.run(v).collect() {
                Ok(arr) => Val::Arr(arr).into(),
                Err(e) => Box::new(core::iter::once(Err(e))),
            },
            Self::Object(o) => {
                let iter = o
                    .iter()
                    .map(|(kf, vf)| Filter::cartesian(kf, vf, Rc::clone(&v)).collect::<Vec<_>>());
                use itertools::Itertools;
                let iter = iter.multi_cartesian_product();
                Box::new(iter.map(|kvs| {
                    let kvs: Result<_, Error> = kvs
                        .into_iter()
                        .map(|(k, v)| Ok((k?.as_obj_key()?, v?)))
                        .collect();
                    Ok(Rc::new(Val::Obj(kvs?)))
                }))
            }
            Self::Math(l, op, r) => {
                let prod = Filter::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| Ok(op.run((*x?).clone(), (*y?).clone())?));
                Box::new(results.map(|x| Ok(Rc::new(x?))))
            }
            Self::Logic(l, op, r) => {
                let prod = Filter::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| Ok(op.run(&*x?, &*y?)));
                Box::new(results.map(|x| Ok(Rc::new(Val::Bool(x?)))))
            }
            Self::Function(f) => f.run(v),
            Self::Ref(r) => r.run(v),
        }
    }
}

impl<F: FilterT> FilterT for Ref<F> {
    fn run(&self, v: Rc<Val>) -> RVals {
        use core::iter::once;
        match self {
            Self::Pipe(l, r) => Box::new(l.run(v).flat_map(move |y| match y {
                Ok(y) => r.run(y),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Comma(l, r) => Box::new(l.run(Rc::clone(&v)).chain(r.run(v))),
            Self::Empty => Box::new(core::iter::empty()),
            Self::Path(p) => {
                let v = p.iter().fold(Vec::from([Ok(Rc::clone(&v))]), |acc, p| {
                    acc.into_iter()
                        .flat_map(|x| match x {
                            Ok(x) => p.follow(Rc::clone(&v), (*x).clone()),
                            Err(e) => Box::new(once(Err(e))),
                        })
                        .collect()
                });
                Box::new(v.into_iter())
            }
            Self::IfThenElse(cond, truth, falsity) => {
                Box::new(cond.run(Rc::clone(&v)).flat_map(move |y| match y {
                    Ok(y) => (if y.as_bool() { truth } else { falsity }).run(Rc::clone(&v)),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
            Self::Function(f) => f.run(v),
        }
    }
}

impl<F: FilterT> FilterT for Box<F> {
    fn run(&self, v: Rc<Val>) -> RVals {
        (**self).run(v)
    }
}

impl<F: FilterT> FilterT for &Box<F> {
    fn run(&self, v: Rc<Val>) -> RVals {
        (*self).run(v)
    }
}

impl Filter {
    pub fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Product> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }
}

impl From<Atom> for Filter {
    fn from(a: Atom) -> Self {
        Self::Atom(a)
    }
}
