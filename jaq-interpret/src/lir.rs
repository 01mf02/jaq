//! Low-level Intermediate Representation of filters.

use crate::filter::{self, Ast as Filter, CallTyp, Id as AbsId, Tailrec};
use crate::path::{self, Path};
use crate::{hir, mir};
use alloc::vec::Vec;
use jaq_syn::filter::{AssignOp, BinaryOp, Fold, KeyVal};
use jaq_syn::{MathOp, Spanned, Str};

pub struct Ctx {
    defs: Vec<Filter>,
    callable: Vec<Callable>,
}

pub struct Callable {
    sig: jaq_syn::Call,
    id: AbsId,
    rec: mir::Rec,
}

const IDENTITY: AbsId = AbsId(0);
const TOSTRING: AbsId = AbsId(IDENTITY.0 + 1);
const EMPTY: AbsId = AbsId(TOSTRING.0 + 2);
const RECURSE: AbsId = AbsId(EMPTY.0 + 4);

pub fn root_def(def: mir::Def) -> filter::Owned {
    let mut ctx = Ctx::default();
    let id = ctx.def(def);
    filter::Owned::new(id, ctx.defs)
}

impl Default for Ctx {
    fn default() -> Self {
        let mut ctx = Self {
            defs: Vec::new(),
            callable: Vec::new(),
        };

        for (f, id) in [(Filter::Id, IDENTITY), (Filter::ToString, TOSTRING)] {
            let id_ = ctx.id_of_ast(f);
            assert_eq!(id, id_);
        }

        let empty = ctx.empty();
        let empty_id = ctx.id_of_ast(empty);
        assert_eq!(empty_id, EMPTY);

        let recurse = ctx.recurse();
        let recurse_id = ctx.id_of_ast(recurse);
        assert_eq!(recurse_id, RECURSE);

        ctx
    }
}

/// Construct a call to `..`.
fn recurse(typ: CallTyp) -> Filter {
    Filter::Call(filter::Call {
        id: RECURSE,
        typ,
        skip: 0,
        args: Vec::new(),
    })
}

impl Ctx {
    /// `{}[]` returns zero values.
    fn empty(&mut self) -> Filter {
        // `{}`
        let obj = Filter::Object(Vec::new());
        // `[]`
        let path = (path::Part::Range(None, None), path::Opt::Essential);
        Filter::Path(self.id_of_ast(obj), Path(Vec::from([path])))
    }

    /// `..`, also known as `recurse/0`, is defined as `., (.[]? | ..)`
    fn recurse(&mut self) -> Filter {
        // `[]?`
        let path = (path::Part::Range(None, None), path::Opt::Optional);
        // `.[]?` (returns array/object elements or nothing instead)
        let path = Filter::Path(IDENTITY, Path(Vec::from([path])));

        // `..`
        let f = recurse(CallTyp::Inside(Some(Tailrec(true))));
        // .[]? | ..
        let pipe = Filter::Pipe(self.id_of_ast(path), false, self.id_of_ast(f));
        // ., (.[]? | ..)
        Filter::Comma(IDENTITY, self.id_of_ast(pipe))
    }

    fn get_callable(&self, hir::RelId(id): hir::RelId) -> &Callable {
        &self.callable[id]
    }

    fn get_def(&mut self, AbsId(id): AbsId) -> &mut Filter {
        &mut self.defs[id]
    }

    fn main(&mut self, main: mir::Main) -> Filter {
        let defs_len = main.defs.len();
        main.defs.into_iter().for_each(|def| {
            self.def(def);
        });
        let body = self.filter(main.body);

        self.callable.drain(self.callable.len() - defs_len..);

        body
    }

    fn def(&mut self, def: mir::Def) -> AbsId {
        let id = AbsId(self.defs.len());
        self.defs.push(Filter::default());
        self.callable.push(Callable {
            sig: def.lhs.clone(),
            id,
            rec: def.rec,
        });
        *self.get_def(id) = self.main(def.rhs);
        let last = self.callable.last_mut().unwrap();
        assert!(last.id == id);
        id
    }

    fn id_of_ast(&mut self, f: filter::Ast) -> AbsId {
        let len = self.defs.len();
        self.defs.push(f);
        AbsId(len)
    }

    /// Convert a MIR filter to a LIR filter.
    fn filter(&mut self, f: Spanned<mir::Filter>) -> Filter {
        let get = |f, ctx: &mut Self| {
            let f = ctx.filter(f);
            ctx.id_of_ast(f)
        };
        let of_str = |s: Str<_>, ctx: &mut Self| {
            let fmt = s.fmt.map_or(TOSTRING, |fmt| get(*fmt, ctx));
            use jaq_syn::string::Part;
            let iter = s.parts.into_iter().map(|part| match part {
                Part::Str(s) => Filter::Str(s),
                Part::Fun(f) => Filter::Pipe(get(f, ctx), false, fmt),
            });
            let mut iter = iter.collect::<Vec<_>>().into_iter().rev();
            let last = iter.next();
            iter.fold(last.unwrap_or_else(|| Filter::Str("".into())), |acc, x| {
                Filter::Math(ctx.id_of_ast(x), MathOp::Add, ctx.id_of_ast(acc))
            })
        };
        use mir::Filter as Expr;

        match f.0 {
            Expr::Var(v) => Filter::Var(v),
            Expr::Call(call, args) => {
                let args: Vec<_> = args.into_iter().map(|a| get(a, self)).collect();
                match call {
                    mir::Call::Arg(a) if args.is_empty() => Filter::Var(a),
                    mir::Call::Arg(_) => panic!("higher-order argument encountered"),
                    mir::Call::Native(n) => Filter::Native(n, args),
                    mir::Call::Def { id, skip, rec } => {
                        let callable = self.get_callable(id);
                        let args = callable.sig.args.iter().zip(args);
                        let typ = match rec {
                            // call from outside
                            None => CallTyp::Outside(Tailrec(callable.rec.tailrec)),
                            // call from inside
                            Some(Tailrec(false)) if !callable.rec.tailrec => CallTyp::Inside(None),
                            Some(_) => CallTyp::Inside(rec),
                        };
                        Filter::Call(filter::Call {
                            id: callable.id,
                            typ,
                            skip,
                            args: args.map(|(ty, a)| ty.as_ref().map(|_| a)).collect(),
                        })
                    }
                }
            }

            Expr::Fold(typ, Fold { xs, init, f, .. }) => {
                Filter::Fold(typ, get(*xs, self), get(*init, self), get(*f, self))
            }

            Expr::Id => Filter::Id,
            Expr::Num(hir::Num::Float(f)) => Filter::Float(f),
            Expr::Num(hir::Num::Int(i)) => Filter::Int(i),
            Expr::Str(s) => of_str(*s, self),
            Expr::Array(a) => Filter::Array(a.map_or(EMPTY, |a| get(*a, self))),
            Expr::Object(o) => {
                let kvs = o.into_iter().map(|kv| match kv {
                    KeyVal::Filter(k, v) => (get(k, self), get(v, self)),
                    KeyVal::Str(k, v) => {
                        let k = of_str(k, self);
                        let k = self.id_of_ast(k);
                        let v = match v {
                            None => self.id_of_ast(Filter::Path(
                                IDENTITY,
                                Path::from(path::Part::Index(k)),
                            )),
                            Some(v) => get(v, self),
                        };
                        (k, v)
                    }
                });
                Filter::Object(kvs.collect())
            }
            Expr::Try(f) => Filter::Try(get(*f, self), EMPTY),
            Expr::Neg(f) => Filter::Neg(get(*f, self)),
            Expr::Recurse => recurse(CallTyp::Outside(Tailrec(true))),

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
                let else_ = else_.map_or(Filter::Id, |else_| self.filter(*else_));
                if_thens.into_iter().rev().fold(else_, |acc, (if_, then_)| {
                    Filter::Ite(get(if_, self), get(then_, self), self.id_of_ast(acc))
                })
            }
            Expr::TryCatch(try_, catch_) => {
                Filter::Try(get(*try_, self), catch_.map_or(EMPTY, |c| get(*c, self)))
            }
            Expr::Path(f, path) => {
                let f = get(*f, self);
                use jaq_syn::path::Part;
                let path = path.into_iter().map(|(p, opt)| match p {
                    Part::Index(i) => (path::Part::Index(get(i, self)), opt),
                    Part::Range(lower, upper) => {
                        let lower = lower.map(|f| get(f, self));
                        let upper = upper.map(|f| get(f, self));
                        (path::Part::Range(lower, upper), opt)
                    }
                });
                Filter::Path(f, Path(path.collect()))
            }
        }
    }
}
