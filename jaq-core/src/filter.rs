use crate::functions::NewFunc;
use crate::ops::{LogicOp, MathOp, OrdOp};
use crate::val::{Atom, Val};
use crate::{Error, Map, Num, Path, RValR, RValRs, ValRs};
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::convert::TryFrom;

#[derive(Debug)]
pub enum Filter {
    New(NewFilter),
    Ref(Ref),
}

#[derive(Debug)]
pub enum NewFilter {
    Atom(Atom),
    Array(Box<Filter>),
    Object(Vec<(Filter, Filter)>),
    Logic(Box<Filter>, LogicOp, Box<Filter>),
    Math(Box<Filter>, MathOp, Box<Filter>),
    Ord(Box<Filter>, OrdOp, Box<Filter>),
    Function(NewFunc),
}

#[derive(Debug)]
pub enum Ref {
    Pipe(Box<Filter>, Box<Filter>),
    Comma(Box<Filter>, Box<Filter>),
    Empty,
    Path(Path),
    Assign(Path, Box<Filter>),
    Update(Path, Box<Filter>),
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>),

    First(Box<Filter>),
    Last(Box<Filter>),
    Limit(Box<Filter>, Box<Filter>),
    Recurse(Box<Filter>),
    Fold(Box<Filter>, Box<Filter>, Box<Filter>),
}

impl Val {
    fn as_obj_key(&self) -> Result<String, Error> {
        self.as_string().ok_or_else(|| Error::ObjKey(self.clone()))
    }
}

type Product = (RValR, RValR);

impl NewFilter {
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
                Filter::cartesian(l, r, v)
                    .map(move |(x, y)| Ok(op.run((*x?).clone(), (*y?).clone())?)),
            ),
            Self::Ord(l, op, r) => Box::new(
                Filter::cartesian(l, r, v).map(move |(x, y)| Ok(Val::Bool(op.run(&*x?, &*y?)))),
            ),
            Self::Function(f) => Box::new(once(f.run(v))),
        }
    }

    fn one() -> Self {
        Self::Atom(Atom::Num(Num::Int(1)))
    }

    fn succ(f: Box<Filter>) -> Self {
        Self::Math(f, MathOp::Add, Self::one().into())
    }
}

impl Filter {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        match self {
            Self::New(n) => Box::new(n.run(v).map(|x| x.map(Rc::new))),
            Self::Ref(r) => r.run(v),
        }
    }
}

impl Ref {
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

            Self::First(f) => Box::new(f.run(v).take(1)),
            Self::Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(Some(y)) => Box::new(once(Ok(y))),
                Ok(None) => Box::new(empty()),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Limit(n, f) => {
                let n = n.run(Rc::clone(&v)).map(|n| Ok(usize::try_from(&*n?)?));
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) => Box::new(f.run(Rc::clone(&v)).take(n as usize)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Recurse(f) => Box::new(crate::Recurse::new(f, v)),
            Self::Fold(xs, init, f) => {
                let mut xs = xs.run(Rc::clone(&v));
                let init: Result<Vec<_>, _> = init.run(Rc::clone(&v)).collect();
                match init.and_then(|init| xs.try_fold(init, |acc, x| Ok(f.fold_step(acc, x?)?))) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }
        }
    }

    pub fn identity() -> Self {
        Self::Path(Path::default())
    }

    pub fn update_math(path: Path, op: MathOp, f: Box<Filter>) -> Self {
        Ref::Update(path, NewFilter::Math(Self::identity().into(), op, f).into())
    }

    pub fn select(f: Box<Filter>) -> Self {
        Self::IfThenElse(f, Self::identity().into(), Self::Empty.into())
    }

    pub fn nth(n: Box<Filter>, f: Box<Filter>) -> Self {
        Self::Last(Self::Limit(NewFilter::succ(n).into(), f).into())
    }
}

impl Filter {
    fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Product> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }

    pub fn fold_step(&self, acc: Vec<Rc<Val>>, x: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        acc.into_iter()
            .map(|acc| {
                let obj = [("acc".to_string(), acc), ("x".to_string(), Rc::clone(&x))];
                Val::Obj(Vec::from(obj).into_iter().collect())
            })
            .flat_map(|obj| self.run(Rc::new(obj)))
            .collect()
    }
}

impl From<Atom> for Filter {
    fn from(a: Atom) -> Self {
        Self::New(NewFilter::Atom(a))
    }
}

impl From<Path> for Filter {
    fn from(p: Path) -> Self {
        Self::Ref(Ref::Path(p))
    }
}

impl From<Ref> for Box<Filter> {
    fn from(r: Ref) -> Self {
        Box::new(Filter::Ref(r))
    }
}

impl From<NewFilter> for Box<Filter> {
    fn from(n: NewFilter) -> Self {
        Box::new(Filter::New(n))
    }
}
