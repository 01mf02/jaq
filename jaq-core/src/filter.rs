use crate::ops::{LogicOp, MathOp, OrdOp};
pub use crate::preprocess::{ClosedFilter, OpenFilter, PreFilter};
use crate::val::{Atom, Val};
use crate::{Error, Map, Path, RValR, RValRs, ValRs};
use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};

#[derive(Clone, Debug)]
pub enum Filter<N> {
    New(New<Filter<N>>),
    Ref(Ref<Filter<N>>),
    Named(N),
}

#[derive(Clone, Debug)]
pub enum New<F> {
    Atom(Atom),
    Array(Box<F>),
    Object(Vec<(F, F)>),
    Logic(Box<F>, LogicOp, Box<F>),
    Math(Box<F>, MathOp, Box<F>),
    Ord(Box<F>, OrdOp, Box<F>),
}

#[derive(Clone, Debug)]
pub enum Ref<F> {
    Pipe(Box<F>, Box<F>),
    Comma(Box<F>, Box<F>),
    Empty,
    Path(Path<F>),
    Assign(Path<F>, Box<F>),
    Update(Path<F>, Box<F>),
    IfThenElse(Box<F>, Box<F>, Box<F>),
}

impl Val {
    fn as_obj_key(&self) -> Result<String, Error> {
        self.as_string().ok_or_else(|| Error::ObjKey(self.clone()))
    }
}

type Product = (RValR, RValR);

impl ClosedFilter {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        match self {
            Self::New(n) => Box::new(n.run(v).map(|x| x.map(Rc::new))),
            Self::Ref(r) => r.run(v),
            Self::Named(n) => n.run(v),
        }
    }

    fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Product> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }
}

impl New<ClosedFilter> {
    fn run(&self, v: Rc<Val>) -> ValRs {
        use core::iter::once;
        match self {
            Self::Atom(a) => Box::new(once(Ok(Val::from(a.clone())))),
            Self::Array(f) => Box::new(once(f.run(v).collect::<Result<_, _>>().map(Val::Arr))),
            Self::Object(o) if o.is_empty() => Box::new(once(Ok(Val::Obj(Map::new())))),
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
            Self::Logic(l, op, r) => Box::new(l.run(Rc::clone(&v)).flat_map(move |l| match l {
                Ok(l) => op.run(l.as_bool(), || r.run(Rc::clone(&v))),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Math(l, op, r) => Box::new(
                Filter::cartesian(l, r, v).map(move |(x, y)| op.run((*x?).clone(), (*y?).clone())),
            ),
            Self::Ord(l, op, r) => Box::new(
                Filter::cartesian(l, r, v).map(move |(x, y)| Ok(Val::Bool(op.run(&*x?, &*y?)))),
            ),
        }
    }
}

impl Ref<ClosedFilter> {
    fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::{empty, once};
        match self {
            Self::Pipe(l, r) => Box::new(l.run(v).flat_map(move |y| match y {
                Ok(y) => r.run(y),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Comma(l, r) => Box::new(l.run(Rc::clone(&v)).chain(r.run(v))),
            Self::Empty => Box::new(empty()),
            Self::Path(p) => match p.collect(v) {
                Ok(y) => Box::new(y.into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Update(path, f) => path.run(v, |v| f.run(v)),
            Self::Assign(path, f) => path.run(Rc::clone(&v), |_| f.run(Rc::clone(&v))),
            Self::IfThenElse(cond, truth, falsity) => {
                Box::new(cond.run(Rc::clone(&v)).flat_map(move |y| match y {
                    Ok(y) => (if y.as_bool() { truth } else { falsity }).run(Rc::clone(&v)),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
        }
    }
}

impl<N> Ref<Filter<N>> {
    pub fn update_math(path: Path<Filter<N>>, op: MathOp, f: Filter<N>) -> Self {
        let id = Filter::Ref(Self::Path(Path::new(Vec::new())));
        let math = Filter::New(New::Math(Box::new(id), op, Box::new(f)));
        Self::Update(path, Box::new(math))
    }
}

impl<N> Filter<N> {
    pub fn try_map<F, M, E>(self, m: &F) -> Result<Filter<M>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        match self {
            Self::New(n) => Ok(Filter::New(n.try_map(m)?)),
            Self::Ref(r) => Ok(Filter::Ref(r.try_map(m)?)),
            Self::Named(n) => m(n),
        }
    }
}

impl<N> New<Filter<N>> {
    fn try_map<F, M, E>(self, m: &F) -> Result<New<Filter<M>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let m = |f: Filter<N>| f.try_map(m);
        use New::*;
        match self {
            Atom(a) => Ok(Atom(a)),
            Array(a) => Ok(Array(Box::new(m(*a)?))),
            Object(o) => Ok(Object(
                o.into_iter()
                    .map(|(k, v)| Ok((m(k)?, m(v)?)))
                    .collect::<Result<_, _>>()?,
            )),
            Logic(l, op, r) => Ok(Logic(Box::new(m(*l)?), op, Box::new(m(*r)?))),
            Math(l, op, r) => Ok(Math(Box::new(m(*l)?), op, Box::new(m(*r)?))),
            Ord(l, op, r) => Ok(Ord(Box::new(m(*l)?), op, Box::new(m(*r)?))),
        }
    }
}

impl<N> Ref<Filter<N>> {
    fn try_map<F, M, E>(self, m1: &F) -> Result<Ref<Filter<M>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let m = |f: Filter<N>| f.try_map(m1).map(Box::new);
        use Ref::*;
        match self {
            Pipe(l, r) => Ok(Pipe(m(*l)?, m(*r)?)),
            Comma(l, r) => Ok(Comma(m(*l)?, m(*r)?)),
            Empty => Ok(Empty),
            Path(p) => Ok(Path(p.try_map(m1)?)),
            Assign(p, f) => Ok(Assign(p.try_map(m1)?, m(*f)?)),
            Update(p, f) => Ok(Update(p.try_map(m1)?, m(*f)?)),
            IfThenElse(cond, truth, falsity) => Ok(IfThenElse(m(*cond)?, m(*truth)?, m(*falsity)?)),
        }
    }
}

impl<F> From<Atom> for Filter<F> {
    fn from(a: Atom) -> Self {
        Self::New(New::Atom(a))
    }
}
