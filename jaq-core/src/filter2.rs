use crate::ops::{LogicOp, MathOp, OrdOp};
use crate::{Error, Path, RValR, RValRs, Val, ValRs};
use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use jaq_parse::parse::{Expr, Spanned};

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

    Var(usize),
}

type Prod = (RValR, RValR);

impl Filter {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::once;
        use core::ops::Neg;
        match self {
            Self::Pos(n) => Box::new(once(Ok(Rc::new(Val::Pos(*n))))),
            Self::Float(x) => Box::new(once(Ok(Rc::new(Val::Float(*x))))),
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
            Self::Math(l, op, r) => Box::new(
                Self::cartesian(l, r, v)
                    .map(|(x, y)| op.run((*x?).clone(), (*y?).clone()).map(Rc::new)),
            ),
            Self::Ord(l, op, r) => Box::new(
                Self::cartesian(l, r, v).map(|(x, y)| Ok(Rc::new(Val::Bool(op.run(&*x?, &*y?))))),
            ),
            Self::Var(_) => todo!(),
            _ => todo!(),
        }
    }

    pub fn cartesian(&self, other: &Self, v: Rc<Val>) -> impl Iterator<Item = Prod> + '_ {
        let l = self.run(Rc::clone(&v));
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
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
            Self::Empty => self,
            Self::Var(v) => args[v].clone(),
        }
    }
}
