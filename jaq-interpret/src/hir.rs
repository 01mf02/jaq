//! High-level Intermediate Representation of definitions and filters.
//!
//! This is quite close to the output of parsing,
//! but replaces names by unique integers.
//! That way, the subsequent transformation step(s)
//! always succeed and do not have to fight with shadowing.

use crate::Bind;
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt;
use jaq_syn::filter::{BinaryOp, Filter as Expr, Fold};
use jaq_syn::{Arg, Spanned};

pub type Filter = jaq_syn::filter::Filter<Call, VarIdx, Num>;
pub type Main = jaq_syn::Main<Filter>;
pub type Def = jaq_syn::Def<Main>;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct RelId(pub usize);
pub type VarIdx = usize;
pub type ArgIdx = usize;

#[derive(Debug, Clone)]
pub enum Call {
    Def { id: RelId, skip: usize },
    Arg(ArgIdx),
    Native(crate::filter::Native),
}

#[derive(Debug, Copy, Clone)]
pub enum Num {
    Float(f64),
    Int(isize),
}

impl Num {
    fn parse(n: &str) -> Result<Self, Self> {
        if n.contains(['.', 'e', 'E']) {
            n.parse().map(Num::Float).map_err(|_| Self::Float(0.))
        } else {
            n.parse().map(Num::Int).map_err(|_| Self::Int(0))
        }
    }
}

pub enum Error {
    Undefined(Arg),
    Num(Num),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Undefined(Bind::Var(_)) => "undefined variable",
            Self::Undefined(Bind::Fun(_)) => "undefined filter",
            Self::Num(Num::Float(_)) => "cannot interpret as floating-point number",
            Self::Num(Num::Int(_)) => "cannot interpret as machine-size integer",
            #[cfg(feature = "unstable-flag")]
            _ => unimplemented!(),
        }
        .fmt(f)
    }
}

struct Callable {
    typ: Relative,
    sig: jaq_syn::Call,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Relative {
    Parent,
    Sibling,
}

/// Convert variables to indices.
#[derive(Default)]
pub struct Ctx {
    pub errs: Vec<Spanned<Error>>,
    /// accessible defined filters
    callable: Vec<Callable>,
    /// accessible native filters
    pub native: Vec<(String, usize, crate::filter::Native)>,
    /// locally bound variables (not bound by filter definition)
    vars: Vec<String>,
}

impl Ctx {
    /// Return all currently bound variables / arguments outside-in.
    fn bound(&self) -> impl DoubleEndedIterator<Item = Bind<&String, &String>> {
        let by_def = self.callable.iter().filter_map(|Callable { typ, sig }| {
            (*typ == Relative::Parent).then_some(sig.args.iter().map(|a| a.as_ref()))
        });
        by_def.flatten().chain(self.vars.iter().map(Bind::Var))
    }

    fn resolve_call(&self, name: &str, arity: usize) -> Option<Call> {
        let mut bound = self.vars.len();

        for (id, Callable { typ, sig }) in self.callable.iter().enumerate().rev() {
            let id = RelId(id);
            if *typ == Relative::Parent {
                for arg in sig.args.iter().rev() {
                    if arity == 0 && arg.as_deref() == Bind::Fun(name) {
                        return Some(Call::Arg(bound));
                    }
                    bound += 1;
                }
            }
            if name == sig.name && arity == sig.args.len() {
                return Some(Call::Def { id, skip: bound });
            }
        }

        self.native
            .iter()
            .find(|(name_, arity_, _)| *name_ == name && *arity_ == arity)
            .map(|(_, _, native)| Call::Native(native.clone()))
    }

    pub fn main(&mut self, main: jaq_syn::Main) -> Main {
        let defs: Vec<_> = main.defs.into_iter().map(|def| self.def(def)).collect();
        assert!(self.vars.is_empty());
        let body = self.expr(main.body);
        assert!(self.vars.is_empty());

        self.callable
            .drain(self.callable.len() - defs.len()..)
            .for_each(|callable| assert_eq!(callable.typ, Relative::Sibling));

        jaq_syn::Main::new(defs, body)
    }

    pub fn def(&mut self, def: jaq_syn::Def) -> Def {
        self.callable.push(Callable {
            typ: Relative::Parent,
            sig: def.lhs.clone(),
        });
        let rhs = self.main(def.rhs);
        self.callable.last_mut().unwrap().typ = Relative::Sibling;
        jaq_syn::Def::new(def.lhs, rhs)
    }

    fn expr(&mut self, f: Spanned<Expr>) -> Spanned<Filter> {
        let get = |ctx: &mut Self, f| Box::new(ctx.expr(f));
        let undefined = |arg| (Error::Undefined(arg), f.1.clone());
        let result = match f.0 {
            Expr::Call(name, args) => {
                let args: Vec<_> = args.into_iter().map(|arg| self.expr(arg)).collect();

                self.resolve_call(&name, args.len()).map_or_else(
                    || {
                        self.errs.push(undefined(Arg::new_filter(name)));
                        Expr::Id
                    },
                    |call| Expr::Call(call, args),
                )
            }
            Expr::Var(v) => {
                let idx = self.bound().rev().position(|i| i == Bind::Var(&v));
                Expr::Var(idx.unwrap_or_else(|| {
                    self.errs.push(undefined(Arg::Var(v)));
                    0
                }))
            }
            Expr::Binary(l, BinaryOp::Pipe(Some(x)), r) => {
                let l = get(self, *l);
                self.vars.push(x.clone());
                let r = get(self, *r);
                assert!(self.vars.pop().as_ref() == Some(&x));
                Expr::Binary(l, BinaryOp::Pipe(Some(x)), r)
            }
            Expr::Fold(typ, Fold { xs, x, init, f, .. }) => {
                let (xs, init) = (get(self, *xs), get(self, *init));
                self.vars.push(x.clone());
                let f = get(self, *f);
                assert!(self.vars.pop().as_ref() == Some(&x));
                Expr::Fold(typ, Fold::new(xs, x, init, f))
            }
            Expr::Id => Expr::Id,
            Expr::Num(n) => Expr::Num(Num::parse(&n).unwrap_or_else(|n| {
                self.errs.push((Error::Num(n), f.1.clone()));
                n
            })),
            Expr::Str(s) => Expr::Str(Box::new((*s).map(|f| self.expr(f)))),
            Expr::Array(a) => Expr::Array(a.map(|a| get(self, *a))),
            Expr::Object(o) => {
                Expr::Object(o.into_iter().map(|kv| kv.map(|f| self.expr(f))).collect())
            }
            Expr::Try(f) => Expr::Try(get(self, *f)),
            Expr::Neg(f) => Expr::Neg(get(self, *f)),
            Expr::Recurse => Expr::Recurse,

            Expr::Binary(l, op, r) => Expr::Binary(get(self, *l), op, get(self, *r)),
            Expr::Ite(if_thens, else_) => {
                let if_thens = if_thens
                    .into_iter()
                    .map(|(i, t)| (self.expr(i), self.expr(t)));
                Expr::Ite(if_thens.collect(), else_.map(|else_| get(self, *else_)))
            }
            Expr::TryCatch(try_, catch_) => {
                Expr::TryCatch(get(self, *try_), catch_.map(|c| get(self, *c)))
            }
            Expr::Path(f, path) => {
                let f = get(self, *f);
                let path = path
                    .into_iter()
                    .map(|(p, opt)| (p.map(|p| self.expr(p)), opt));
                Expr::Path(f, path.collect())
            }
            #[cfg(feature = "unstable-flag")]
            _ => unimplemented!(),
        };
        (result, f.1)
    }
}
