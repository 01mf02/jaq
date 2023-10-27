//! Low-level Intermediate Representation of filters.

use crate::filter::{self, Ast as Filter, Def};
use crate::mir::{self, MirFilter, RelId, Relative};
use crate::path::{self, Path};
use alloc::{boxed::Box, vec::Vec};
use jaq_syn::filter::{AssignOp, BinaryOp, Fold, KeyVal};
use jaq_syn::{MathOp, Str};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct AbsId(usize);

#[derive(Default)]
pub struct Ctx {
    defs: Vec<Def>,
    callable: Vec<Callable>,
}

pub struct Callable {
    typ: Relative,
    sig: jaq_syn::Call,
    id: AbsId,
}

pub fn root_def(def: mir::Def) -> filter::Owned {
    let mut ctx = Ctx::default();
    ctx.def(def);
    filter::Owned::new(ctx.defs[0].rhs.clone(), ctx.defs)
}

// TODO: remove itertools dependency

impl Ctx {
    fn get_callable(&self, RelId(id): RelId) -> &Callable {
        &self.callable[id]
    }

    fn get_def(&mut self, AbsId(id): AbsId) -> &mut Def {
        &mut self.defs[id]
    }

    fn main(&mut self, main: mir::Main) -> Filter {
        let defs_len = main.defs.len();
        main.defs.into_iter().for_each(|def| self.def(def));
        let body = self.filter(main.body);

        self.callable
            .drain(self.callable.len() - defs_len..)
            .for_each(|callable| assert_eq!(callable.typ, Relative::Sibling));

        body
    }

    fn def(&mut self, def: mir::Def) {
        let id = AbsId(self.defs.len());
        self.defs.push(Def {
            rec: false,
            rhs: Filter::default(),
        });
        self.callable.push(Callable {
            typ: Relative::Parent,
            sig: def.lhs.clone(),
            id,
        });
        self.get_def(id).rhs = self.main(def.rhs);
        let last = self.callable.last_mut().unwrap();
        assert!(last.id == id);
        last.typ = Relative::Sibling;
    }

    /// Convert a MIR filter to a LIR filter.
    fn filter(&mut self, f: MirFilter) -> Filter {
        let get = |f, ctx: &mut Self| Box::new(ctx.filter(f));
        let of_str = |s: Str<_>, ctx: &mut Self| {
            let fmt = s.fmt.map_or(Filter::ToString, |fmt| *get(*fmt, ctx));
            use jaq_syn::string::Part;
            let mut iter = s.parts.into_iter().rev().map(|part| match part {
                Part::Str(s) => Filter::Str(s),
                Part::Fun(f) => Filter::Pipe(Box::new(*get(f, ctx)), false, Box::new(fmt.clone())),
            });
            let last = iter.next();
            iter.fold(last.unwrap_or_else(|| Filter::Str("".into())), |acc, x| {
                Filter::Math(Box::new(x), MathOp::Add, Box::new(acc))
            })
        };
        use mir::Filter as Expr;

        match f.0 {
            Expr::Var(v) => Filter::Var(v),
            Expr::Call(call, args) => {
                let args: Vec<_> = args.into_iter().map(|a| *get(a, self)).collect();
                match call {
                    mir::Call::Arg(a) if args.is_empty() => Filter::Var(a),
                    mir::Call::Arg(_) => panic!("higher-order argument encountered"),
                    mir::Call::Native(n) => Filter::Native(n, args),
                    mir::Call::Def { id, skip } => {
                        let callable = self.get_callable(id);
                        let args = callable.sig.args.iter().zip(args);
                        let args = args.map(|(ty, a)| ty.as_ref().map(|_| a)).collect();
                        let id = callable.id;
                        if callable.typ == Relative::Parent {
                            self.get_def(id).rec = true;
                        }
                        let AbsId(id) = id;
                        Filter::Call { skip, args, id }
                    }
                }
            }

            Expr::Fold(typ, Fold { xs, init, f, .. }) => {
                Filter::Fold(typ, get(*xs, self), get(*init, self), get(*f, self))
            }

            Expr::Id => Filter::Id,
            Expr::Num(mir::Num::Float(f)) => Filter::Float(f),
            Expr::Num(mir::Num::Int(i)) => Filter::Int(i),
            Expr::Str(s) => of_str(*s, self),
            Expr::Array(a) => {
                Filter::Array(a.map_or_else(|| Box::new(Filter::empty()), |a| get(*a, self)))
            }
            Expr::Object(o) => {
                let kvs = o.into_iter().map(|kv| match kv {
                    KeyVal::Filter(k, v) => (*get(k, self), *get(v, self)),
                    KeyVal::Str(k, v) => {
                        let k = of_str(k, self);
                        let v = match v {
                            None => Filter::Path(
                                Box::new(Filter::Id),
                                Path::from(path::Part::Index(k.clone())),
                            ),
                            Some(v) => *get(v, self),
                        };
                        (k, v)
                    }
                });
                Filter::Object(kvs.collect())
            }
            Expr::Try(f) => Filter::Try(get(*f, self), Box::new(Filter::empty())),
            Expr::Neg(f) => Filter::Neg(get(*f, self)),
            Expr::Recurse => Filter::recurse0(),

            Expr::Binary(l, op, r) => {
                let (l, r) = (get(*l, self), get(*r, self));
                match op {
                    BinaryOp::Pipe(bind) => Filter::Pipe(l, bind.is_some(), r),
                    BinaryOp::Comma => Filter::Comma(l, r),
                    BinaryOp::Alt => Filter::Alt(l, r),
                    BinaryOp::Or => Filter::Logic(l, true, r),
                    BinaryOp::And => Filter::Logic(l, false, r),
                    BinaryOp::Math(op) => Filter::Math(l, op, r),
                    BinaryOp::Ord(op) => Filter::Ord(l, op, r),
                    BinaryOp::Assign(AssignOp::Assign) => Filter::Assign(l, r),
                    BinaryOp::Assign(AssignOp::Update) => Filter::Update(l, r),
                    BinaryOp::Assign(AssignOp::UpdateWith(op)) => Filter::UpdateMath(l, op, r),
                }
            }

            Expr::Ite(if_thens, else_) => {
                let else_ = else_.map_or(Filter::Id, |else_| *get(*else_, self));
                if_thens.into_iter().rev().fold(else_, |acc, (if_, then_)| {
                    Filter::Ite(get(if_, self), get(then_, self), Box::new(acc))
                })
            }
            Expr::TryCatch(try_, catch_) => Filter::Try(
                get(*try_, self),
                catch_.map_or_else(|| Box::new(Filter::empty()), |c| get(*c, self)),
            ),
            Expr::Path(f, path) => {
                let f = get(*f, self);
                use jaq_syn::path::Part;
                let path = path.into_iter().map(|(p, opt)| match p {
                    Part::Index(i) => (path::Part::Index(*get(i, self)), opt),
                    Part::Range(lower, upper) => {
                        let lower = lower.map(|f| *get(f, self));
                        let upper = upper.map(|f| *get(f, self));
                        (path::Part::Range(lower, upper), opt)
                    }
                });
                Filter::Path(f, Path(path.collect()))
            }
        }
    }
}
