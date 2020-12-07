use crate::ops::{LogicOp, MathOp};
use crate::path::Path;
use crate::val::{Val, Vals};
use std::rc::Rc;

#[derive(Debug)]
pub enum Filter {
    Atom(Atom),
    Array(Box<Filter>),
    Object(Vec<(Filter, Filter)>),
    Math(Box<Filter>, MathOp, Box<Filter>),
    Logic(Box<Filter>, LogicOp, Box<Filter>),
    Pipe(Box<Filter>, Box<Filter>),
    Comma(Box<Filter>, Box<Filter>),
    Empty,
    Path(Path),
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>),
    Function(String, Vec<Filter>),
}

#[derive(Clone, Debug)]
pub enum Atom {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
}

impl From<Atom> for Val {
    fn from(a: Atom) -> Self {
        match a {
            Atom::Null => Self::Null,
            Atom::Bool(b) => Self::Bool(b),
            Atom::Num(n) => Self::Num(n),
            Atom::Str(s) => Self::Str(s),
        }
    }
}

type Product = (Rc<Val>, Rc<Val>);

impl From<Val> for Vals<'_> {
    fn from(v: Val) -> Self {
        Box::new(core::iter::once(Rc::new(v)))
    }
}

impl Filter {
    pub fn run<'a: 'iter, 'iter>(&'a self, v: Rc<Val>) -> Vals<'iter> {
        match self {
            Self::Atom(a) => Val::from(a.clone()).into(),
            Self::Array(f) => Val::Arr(f.run(v).collect()).into(),
            Self::Object(o) => {
                let iter = o
                    .iter()
                    .map(|(kf, vf)| Self::cartesian(kf, vf, Rc::clone(&v)).collect::<Vec<_>>());
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
                let prod = Self::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| op.run((*x).clone(), (*y).clone()).unwrap());
                Box::new(results.map(Rc::new))
            }
            Self::Logic(l, op, r) => {
                let prod = Self::cartesian(l, r, v);
                let results = prod.map(move |(x, y)| op.run(&x, &y));
                Box::new(results.map(|x| Rc::new(Val::Bool(x))))
            }
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
            Self::IfThenElse(_cond, _truth, _falsity) => {
                todo!()
            }
            Self::Function(name, args) => match (name.as_str(), args.len()) {
                // TODO: Map to Self::Empty
                ("empty", 0) => Box::new(core::iter::empty()),
                ("any", 0) => Val::Bool(v.iter().unwrap().any(|v| v.as_bool())).into(),
                ("all", 0) => Val::Bool(v.iter().unwrap().all(|v| v.as_bool())).into(),
                ("not", 0) => Val::Bool(!v.as_bool()).into(),
                ("add", 0) => {
                    let iter = v.iter().unwrap().map(|x| (*x).clone());
                    v.fold(Val::Null, |acc, x| (acc + x).unwrap()).into()
                }
                (name, len) => panic!("unrecognised function: {}/{}", name, len),
            }, //_ => todo!(),
        }
    }

    pub fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Product> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }
}
