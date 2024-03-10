//! Mid-level Intermediate Representation of definitions and filters.
//!
//! This mainly analyses occurrences of recursion, which is
//! important to efficiently execute tail-recursive filters.

use crate::hir::{self, ArgIdx, Num, RelId, VarIdx};
use alloc::{boxed::Box, vec::Vec};
use jaq_syn::filter::{BinaryOp, Filter as Expr, Fold};
use jaq_syn::Spanned;

pub type Filter = jaq_syn::filter::Filter<Call, VarIdx, Num>;

pub struct Main {
    pub defs: Vec<Def>,
    pub body: Spanned<Filter>,
}

pub struct Def {
    pub lhs: jaq_syn::Call,
    pub rhs: Main,
    /// is the filter tail-recursive?
    pub tailrec: bool,
}

#[derive(Debug, Clone)]
pub enum Call {
    Def { id: RelId, skip: usize, tail: bool },
    Arg(ArgIdx),
    Native(crate::filter::Native),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Relative {
    Parent { tailrec: bool },
    Sibling { tailrec: Tailrec },
}

#[derive(Default)]
pub struct Ctx {
    /// accessible defined filters
    callable: Vec<Relative>,
}

/// which filters can be called tail-recursively at the current point
pub type Tailrec = alloc::collections::BTreeSet<RelId>;

impl Ctx {
    pub fn main(&mut self, main: hir::Main, tr: Tailrec) -> Main {
        for _ in &main.defs {
            self.callable.push(Relative::Sibling {
                tailrec: tr.clone(),
            });
        }
        //std::dbg!("handle body", &main.body, &self.callable);
        let body = self.expr(main.body, tr);
        //std::dbg!("defs: ", &main.defs);
        let defs = main.defs.into_iter().rev().map(|def| {
            //std::dbg!("handle def", &def);
            let tailrec = match self.callable.pop().unwrap() {
                Relative::Sibling { tailrec } => tailrec,
                _ => panic!(),
            };
            self.def(def, tailrec)
        });
        let mut defs: Vec<_> = defs.collect();
        defs.reverse();
        Main { defs, body }
    }

    pub fn def(&mut self, def: hir::Def, mut tr: Tailrec) -> Def {
        //std::dbg!("treating def:", &def.lhs);
        tr.insert(RelId(self.callable.len()));
        self.callable.push(Relative::Parent { tailrec: false });

        Def {
            lhs: def.lhs,
            rhs: self.main(def.rhs, tr),
            tailrec: match self.callable.pop().unwrap() {
                Relative::Parent { tailrec } => tailrec,
                _ => panic!(),
            },
        }
    }

    fn expr(&mut self, f: Spanned<hir::Filter>, tr: Tailrec) -> Spanned<Filter> {
        // no tail-recursion
        let notr = Tailrec::default;
        let get = |ctx: &mut Self, f, tr| Box::new(ctx.expr(f, tr));
        let result = match f.0 {
            Expr::Call(call, args) => {
                let args: Vec<_> = args.into_iter().map(|arg| self.expr(arg, notr())).collect();
                //std::dbg!(&call);
                //std::dbg!(&self.callable);
                let call = match call {
                    hir::Call::Arg(a) => Call::Arg(a),
                    hir::Call::Native(n) => Call::Native(n),
                    hir::Call::Def { id, skip } => {
                        let tail = match &mut self.callable[id.0] {
                            Relative::Parent { ref mut tailrec } => {
                                let tail = tr.contains(&id);
                                *tailrec = *tailrec || tail;
                                tail
                            }
                            Relative::Sibling { ref mut tailrec } => {
                                *tailrec = tailrec.intersection(&tr).cloned().collect();
                                false
                            }
                        };
                        Call::Def { id, skip, tail }
                    }
                };
                Expr::Call(call, args)
            }
            Expr::Var(v) => Expr::Var(v),
            Expr::Binary(l, BinaryOp::Comma, r) => {
                let l = get(self, *l, tr.clone());
                let r = get(self, *r, tr);
                Expr::Binary(l, BinaryOp::Comma, r)
            }
            Expr::Binary(l, op @ (BinaryOp::Alt | BinaryOp::Pipe(_)), r) => {
                let l = get(self, *l, notr());
                let r = get(self, *r, tr);
                Expr::Binary(l, op, r)
            }
            Expr::Binary(l, op, r) => {
                Expr::Binary(get(self, *l, notr()), op, get(self, *r, notr()))
            }

            Expr::Fold(typ, Fold { xs, x, init, f, .. }) => {
                let xs = get(self, *xs, notr());
                let init = get(self, *init, notr());
                let f = get(self, *f, notr());
                Expr::Fold(typ, Fold::new(xs, x, init, f))
            }
            Expr::Id => Expr::Id,
            Expr::Recurse => Expr::Recurse,
            Expr::Num(n) => Expr::Num(n),
            Expr::Str(s) => Expr::Str(Box::new((*s).map(|f| self.expr(f, notr())))),
            Expr::Array(a) => Expr::Array(a.map(|a| get(self, *a, notr()))),
            Expr::Object(o) => Expr::Object(
                o.into_iter()
                    .map(|kv| kv.map(|f| self.expr(f, notr())))
                    .collect(),
            ),
            Expr::Try(f) => Expr::Try(get(self, *f, notr())),
            Expr::Neg(f) => Expr::Neg(get(self, *f, notr())),

            Expr::Ite(if_thens, else_) => {
                let if_thens = if_thens
                    .into_iter()
                    .map(|(i, t)| (self.expr(i, notr()), self.expr(t, tr.clone())));
                Expr::Ite(if_thens.collect(), else_.map(|else_| get(self, *else_, tr)))
            }
            Expr::TryCatch(try_, catch_) => {
                Expr::TryCatch(get(self, *try_, notr()), catch_.map(|c| get(self, *c, tr)))
            }
            Expr::Path(f, path) => {
                let f = get(self, *f, notr());
                let path = path
                    .into_iter()
                    .map(|(p, opt)| (p.map(|p| self.expr(p, notr())), opt));
                Expr::Path(f, path.collect())
            }
            #[cfg(feature = "unstable-flag")]
            _ => unimplemented!(),
        };
        (result, f.1)
    }
}
