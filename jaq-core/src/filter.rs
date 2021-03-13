use crate::functions::{NewFunc, RefFunc};
use crate::ops::{LogicOp, MathOp};
use crate::path::Path;
use crate::val::{Atom, Val, Vals};
use std::rc::Rc;

pub trait FilterT: core::fmt::Debug {
    fn run(&self, v: Rc<Val>) -> Vals;
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

type Product = (Rc<Val>, Rc<Val>);

impl FilterT for Filter {
    fn run(&self, v: Rc<Val>) -> Vals {
        match self {
            Self::Atom(a) => Val::from(a.clone()).into(),
            Self::Array(f) => Val::Arr(f.run(v).collect()).into(),
            Self::Object(o) => {
                let iter = o
                    .iter()
                    .map(|(kf, vf)| Filter::cartesian(kf, vf, Rc::clone(&v)).collect::<Vec<_>>());
                use itertools::Itertools;
                let iter = iter.multi_cartesian_product();
                Box::new(iter.map(|kvs| {
                    Rc::new(Val::Obj(
                        kvs.into_iter()
                            .map(|(k, v)| (k.as_str().unwrap().to_string(), v))
                            .collect(),
                    ))
                }))
            }
            Self::Math(l, op, r) => {
                let prod = Filter::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| op.run((*x).clone(), (*y).clone()).unwrap());
                Box::new(results.map(Rc::new))
            }
            Self::Logic(l, op, r) => {
                let prod = Filter::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| op.run(&x, &y));
                Box::new(results.map(|x| Rc::new(Val::Bool(x))))
            }
            Self::Function(f) => f.run(v),
            Self::Ref(r) => r.run(v),
        }
    }
}

impl<F: FilterT> FilterT for Ref<F> {
    fn run(&self, v: Rc<Val>) -> Vals {
        match self {
            Self::Pipe(l, r) => Box::new(l.run(v).flat_map(move |y| r.run(y))),
            Self::Comma(l, r) => Box::new(l.run(Rc::clone(&v)).chain(r.run(v))),
            Self::Empty => Box::new(core::iter::empty()),
            Self::Path(p) => {
                let v = p.iter().fold(vec![Rc::clone(&v)], |acc, p| {
                    acc.into_iter()
                        .flat_map(|x| p.follow(Rc::clone(&v), (*x).clone()))
                        .collect()
                });
                Box::new(v.into_iter())
            }
            Self::IfThenElse(cond, truth, falsity) => {
                Box::new(cond.run(Rc::clone(&v)).flat_map(move |x| {
                    if x.as_bool() {
                        truth.run(Rc::clone(&v))
                    } else {
                        falsity.run(Rc::clone(&v))
                    }
                }))
            }
            Self::Function(f) => f.run(v),
        }
    }
}

impl<F: FilterT> FilterT for Box<F> {
    fn run(&self, v: Rc<Val>) -> Vals {
        (**self).run(v)
    }
}

impl<F: FilterT> FilterT for &Box<F> {
    fn run(&self, v: Rc<Val>) -> Vals {
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
