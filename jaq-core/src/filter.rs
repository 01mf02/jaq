use crate::functions::{NewFunc, RefFunc};
use crate::ops::{LogicOp, MathOp};
use crate::val::{Atom, RValR, RValRs, Val};
use crate::{Error, Path};
use alloc::{boxed::Box, rc::Rc, vec::Vec};

pub trait FilterT: core::fmt::Debug {
    fn run(&self, v: Rc<Val>) -> RValRs;
}

#[derive(Debug)]
pub enum Filter {
    New(NewFilter),
    Ref(Ref<Box<Filter>>),
}

#[derive(Debug)]
pub enum NewFilter {
    Atom(Atom),
    Array(Box<Filter>),
    Object(Vec<(Filter, Filter)>),
    Math(Box<Filter>, MathOp, Box<Filter>),
    Logic(Box<Filter>, LogicOp, Box<Filter>),
    Function(NewFunc),
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

type Product = (RValR, RValR);

impl NewFilter {
    fn run(&self, v: Rc<Val>) -> Box<dyn Iterator<Item = Result<Val, Error>> + '_> {
        use core::iter::once;
        match self {
            Self::Atom(a) => Box::new(once(Ok(Val::from(a.clone())))),
            Self::Array(f) => Box::new(once(f.run(v).collect::<Result<_, _>>().map(Val::Arr))),
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
                    Ok(Val::Obj(kvs?))
                }))
            }
            Self::Math(l, op, r) => Box::new(
                Filter::cartesian(l, r, v)
                    .map(move |(x, y)| Ok(op.run((*x?).clone(), (*y?).clone())?)),
            ),
            Self::Logic(l, op, r) => Box::new(
                Filter::cartesian(l, r, v).map(move |(x, y)| Ok(Val::Bool(op.run(&*x?, &*y?)))),
            ),
            Self::Function(f) => Box::new(once(f.run(v))),
        }
    }
}

impl FilterT for Filter {
    fn run(&self, v: Rc<Val>) -> RValRs {
        match self {
            Self::New(n) => Box::new(n.run(v).map(|x| x.map(Rc::new))),
            Self::Ref(r) => r.run(v),
        }
    }
}

impl<F: FilterT> FilterT for Ref<F> {
    fn run(&self, v: Rc<Val>) -> RValRs {
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
    fn run(&self, v: Rc<Val>) -> RValRs {
        (**self).run(v)
    }
}

impl<F: FilterT> FilterT for &Box<F> {
    fn run(&self, v: Rc<Val>) -> RValRs {
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
        Self::New(NewFilter::Atom(a))
    }
}
