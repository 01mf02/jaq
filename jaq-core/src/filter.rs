use crate::ops::{LogicOp, MathOp, OrdOp};
use crate::{Error, Path, RValR, RValRs, Val};
use alloc::{boxed::Box, rc::Rc, string::String, string::ToString, vec::Vec};

#[derive(Clone, Debug)]
pub enum Filter {
    Pos(usize),
    Float(f64),
    Str(String),
    Array(Box<Self>),
    Object(Vec<(Self, Self)>),

    Neg(Box<Self>),
    Pipe(Box<Self>, Box<Self>),
    Comma(Box<Self>, Box<Self>),
    IfThenElse(Box<Self>, Box<Self>, Box<Self>),

    Path(Path<Self>),
    Assign(Path<Self>, Box<Self>),
    Update(Path<Self>, Box<Self>),

    Logic(Box<Self>, LogicOp, Box<Self>),
    Math(Box<Self>, MathOp, Box<Self>),
    Ord(Box<Self>, OrdOp, Box<Self>),

    Empty,
    Length,
    Type,
    First(Box<Self>),
    Last(Box<Self>),
    Recurse(Box<Self>),
    Limit(Box<Self>, Box<Self>),
    Range(Box<Self>, Box<Self>),
    Fold(Box<Self>, Box<Self>, Box<Self>),

    Var(usize),
}

type Prod = (RValR, RValR);

impl Filter {
    pub fn builtins() -> Vec<((String, usize), Self)> {
        let var = |v| Box::new(Self::Var(v));
        macro_rules! make_builtin {
            ($name: expr, 0, $cons: expr) => {
                (($name.to_string(), 0), $cons)
            };
            ($name: expr, 1, $cons: expr) => {
                (($name.to_string(), 1), $cons(var(0)))
            };
            ($name: expr, 2, $cons: expr) => {
                (($name.to_string(), 2), $cons(var(0), var(1)))
            };
            ($name: expr, 3, $cons: expr) => {
                (($name.to_string(), 3), $cons(var(0), var(1), var(2)))
            };
        }
        Vec::from([
            make_builtin!("length", 0, Self::Length),
            make_builtin!("type", 0, Self::Type),
            make_builtin!("first", 1, Self::First),
            make_builtin!("last", 1, Self::Last),
            make_builtin!("recurse", 1, Self::Recurse),
            make_builtin!("limit", 2, Self::Limit),
            make_builtin!("range", 2, Self::Range),
            make_builtin!("fold", 3, Self::Fold),
        ])
    }

    pub fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::once;
        use core::ops::Neg;
        match self {
            Self::Pos(n) => Box::new(once(Ok(Rc::new(Val::Pos(*n))))),
            Self::Float(x) => Box::new(once(Ok(Rc::new(Val::Float(*x))))),
            Self::Str(s) => Box::new(once(Ok(Rc::new(Val::Str(s.clone()))))),
            Self::Array(f) => Box::new(once(
                f.run(v)
                    .collect::<Result<_, _>>()
                    .map(|v| Rc::new(Val::Arr(v))),
            )),
            Self::Object(o) if o.is_empty() => {
                Box::new(once(Ok(Rc::new(Val::Obj(Default::default())))))
            }
            Self::Object(o) => {
                let iter = o
                    .iter()
                    .map(|(kf, vf)| Self::cartesian(kf, vf, Rc::clone(&v)).collect::<Vec<_>>());
                use itertools::Itertools;
                let iter = iter.multi_cartesian_product();
                Box::new(iter.map(|kvs| {
                    kvs.into_iter()
                        .map(|(k, v)| Ok((k?.as_obj_key()?, v?)))
                        .collect::<Result<_, _>>()
                        .map(|kvs| Rc::new(Val::Obj(kvs)))
                }))
            }
            Self::Neg(f) => Box::new(f.run(v).map(|v| (*v?).clone().neg().map(Rc::new))),
            Self::Pipe(l, r) => Box::new(l.run(v).flat_map(|y| match y {
                Ok(y) => r.run(y),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Comma(l, r) => Box::new(l.run(Rc::clone(&v)).chain(r.run(v))),
            Self::IfThenElse(if_, then, else_) => {
                Box::new(if_.run(Rc::clone(&v)).flat_map(move |y| match y {
                    Ok(y) => (if y.as_bool() { then } else { else_ }).run(Rc::clone(&v)),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
            Self::Path(path) => match path.collect(v) {
                Ok(y) => Box::new(y.into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Assign(path, f) => path.run(Rc::clone(&v), |_| f.run(Rc::clone(&v))),
            Self::Update(path, f) => path.run(v, |v| f.run(v)),
            Self::Logic(l, op, r) => Box::new(l.run(Rc::clone(&v)).flat_map(move |l| match l {
                Ok(l) => op.run(l.as_bool(), || r.run(Rc::clone(&v))),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Math(l, op, r) => Box::new(
                Self::cartesian(l, r, v)
                    .map(|(x, y)| op.run((*x?).clone(), (*y?).clone()).map(Rc::new)),
            ),
            Self::Ord(l, op, r) => Box::new(
                Self::cartesian(l, r, v).map(|(x, y)| Ok(Rc::new(Val::Bool(op.run(&*x?, &*y?))))),
            ),
            Self::Empty => Box::new(core::iter::empty()),
            Self::Length => Box::new(once(v.len().map(Rc::new))),
            Self::Type => Box::new(once(Ok(Rc::new(Val::Str(v.typ().to_string()))))),
            Self::First(f) => Box::new(f.run(v).take(1)),
            Self::Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(y) => Box::new(y.map(Ok).into_iter()),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Limit(n, f) => {
                let n = n.run(Rc::clone(&v)).map(|n| n?.as_usize());
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) => Box::new(f.run(Rc::clone(&v)).take(n as usize)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Range(from, until) => {
                let prod = Filter::cartesian(from, until, v);
                let ranges = prod.map(|(from, until)| from?.range(&*until?));
                Box::new(ranges.flat_map(|range| match range {
                    Ok(range) => Box::new(range.map(Rc::new).map(Ok)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Recurse(f) => Box::new(crate::Recurse::new(f, v)),
            Self::Fold(xs, init, f) => {
                let mut xs = xs.run(Rc::clone(&v));
                let init: Result<Vec<_>, _> = init.run(Rc::clone(&v)).collect();
                match init.and_then(|init| xs.try_fold(init, |acc, x| f.fold_step(acc, x?))) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }

            Self::Var(_) => todo!(),
        }
    }

    pub fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Prod> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }

    fn fold_step(&self, acc: Vec<Rc<Val>>, x: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        acc.into_iter()
            .map(|acc| {
                let obj = [("acc".to_string(), acc), ("x".to_string(), Rc::clone(&x))];
                Val::Obj(Vec::from(obj).into_iter().collect())
            })
            .flat_map(|obj| self.run(Rc::new(obj)))
            .collect()
    }

    pub fn update_math(path: Path<Self>, op: MathOp, f: Self) -> Self {
        let id = Self::Path(Path::new(Vec::new()));
        let math = Self::Math(Box::new(id), op, Box::new(f));
        Self::Update(path, Box::new(math))
    }

    pub fn subst(self, args: &[Self]) -> Self {
        let sub = |f: Box<Self>| Box::new(f.subst(args));
        match self {
            Self::Pos(_) | Self::Float(_) | Self::Str(_) => self,
            Self::Array(f) => Self::Array(sub(f)),
            Self::Object(kvs) => Self::Object(
                kvs.into_iter()
                    .map(|(k, v)| (k.subst(args), v.subst(args)))
                    .collect(),
            ),
            Self::Neg(f) => Self::Neg(sub(f)),
            Self::Pipe(l, r) => Self::Pipe(sub(l), sub(r)),
            Self::Comma(l, r) => Self::Comma(sub(l), sub(r)),
            Self::IfThenElse(if_, then, else_) => Self::IfThenElse(sub(if_), sub(then), sub(else_)),
            Self::Path(path) => Self::Path(path.map(|f| f.subst(args))),
            Self::Assign(path, f) => Self::Assign(path.map(|f| f.subst(args)), sub(f)),
            Self::Update(path, f) => Self::Update(path.map(|f| f.subst(args)), sub(f)),
            Self::Logic(l, op, r) => Self::Logic(sub(l), op, sub(r)),
            Self::Math(l, op, r) => Self::Math(sub(l), op, sub(r)),
            Self::Ord(l, op, r) => Self::Ord(sub(l), op, sub(r)),
            Self::Empty | Self::Length | Self::Type => self,
            Self::First(f) => Self::First(sub(f)),
            Self::Last(f) => Self::Last(sub(f)),
            Self::Recurse(f) => Self::Recurse(sub(f)),
            Self::Limit(n, f) => Self::Limit(sub(n), sub(f)),
            Self::Range(lower, upper) => Self::Range(sub(lower), sub(upper)),
            Self::Fold(xs, init, f) => Self::Fold(sub(xs), sub(init), sub(f)),
            Self::Var(v) => args[v].clone(),
        }
    }
}
