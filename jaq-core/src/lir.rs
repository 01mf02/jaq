use crate::filter::Filter;
use crate::mir::{self, FilterId};
use crate::path::{self, Path};
use alloc::{boxed::Box, vec::Vec};
use jaq_parse::filter::{AssignOp, BinaryOp, Fold, KeyVal};
use jaq_parse::{Arg, Error, Spanned};

#[derive(Clone)]
struct View {
    vars: Vec<usize>,
    args: Vec<usize>,
    recs: Vec<usize>,
}

impl View {
    fn truncate(&mut self, id: FilterId, defs: &mir::Defs) {
        let vars = defs.vars(id).count();
        let args = defs.args(id).count() - vars;
        self.vars.truncate(vars);
        self.args.truncate(args);
        self.recs.truncate(defs.recs(id).count());
    }
}

struct Ctx {
    /// number of variables in the execution context at the current point
    vars: usize,
    args: Vec<(Spanned<mir::Filter>, FilterId, View)>,
    /// list of recursively defined filters with their arity (only variable arguments)
    recs: Vec<(usize, mir::Filter)>,
}

impl Ctx {
    // TODO: operate on borrowed filter
    fn filter(
        &mut self,
        f: Spanned<mir::Filter>,
        id: FilterId,
        mut view: View,
        defs: &mir::Defs,
    ) -> Filter {
        let get = |f, ctx: &mut Self| Box::new(ctx.filter(f, id, view.clone(), defs));
        use mir::Filter as Expr;

        match f.0 {
            Expr::Var(v) => Filter::Var(self.vars - view.vars[v]),
            Expr::Call(mir::Call::Arg(a), args) => {
                assert!(args.is_empty());
                let (f, id, view) = self.args[view.args[a]].clone();
                self.filter(f, id, view, defs)
            }
            Expr::Call(mir::Call::Def(did), args) => {
                let last_common = todo!();
                let new_view = view.clone();
                new_view.truncate(last_common, defs);

                // recursion!
                if let Some(rec_idx) = defs.recs(id).position(|rid| rid == did) {
                    let rvars = defs.args(did).filter(|a| a.get_var().is_some()).count();
                    Filter::Call {
                        id: view.recs[rec_idx],
                        skip: self.vars - rvars,
                    }
                } else {
                    todo!()
                }
            }

            // TODO: move down with the other binary operators
            Expr::Binary(l, BinaryOp::Pipe(None), r) => {
                Filter::Pipe(get(*l, self), false, get(*r, self))
            }

            // variable-binding operators
            Expr::Binary(l, BinaryOp::Pipe(Some(x)), r) => {
                let l = get(*l, self);

                view.vars.push(self.vars);
                self.vars += 1;
                let r = Box::new(self.filter(*r, id, view, defs));
                self.vars -= 1;

                //let r = self.bind(view, |ctx, view| Box::new(ctx.filter(*r, id, view, defs)));
                Filter::Pipe(l, true, r)
            }
            Expr::Fold(typ, Fold { xs, x, init, f }) => {
                let (xs, init) = (get(*xs, self), get(*init, self));

                view.vars.push(self.vars);
                self.vars += 1;
                let f = Box::new(self.filter(*f, id, view, defs));
                self.vars -= 1;

                Filter::Fold(typ, xs, init, f)
            }

            Expr::Id => Filter::Id,
            Expr::Num(mir::Num::Float(f)) => Filter::Float(f),
            Expr::Num(mir::Num::Int(i)) => Filter::Int(i),
            Expr::Str(s) => Filter::Str(s),
            Expr::Array(a) => Filter::Array(a.map(|a| get(*a, self))),
            Expr::Object(o) => {
                let kvs = o.into_iter().map(|kv| match kv {
                    KeyVal::Filter(k, v) => (*get(k, self), *get(v, self)),
                    KeyVal::Str(k, v) => {
                        let k = Filter::Str(k);
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
            Expr::Try(f) => Filter::Try(get(*f, self)),
            Expr::Neg(f) => Filter::Neg(get(*f, self)),
            Expr::Recurse => Filter::recurse(),

            Expr::Binary(l, BinaryOp::Comma, r) => Filter::Comma(get(*l, self), get(*r, self)),
            Expr::Binary(l, BinaryOp::Alt, r) => Filter::Alt(get(*l, self), get(*r, self)),
            Expr::Binary(l, BinaryOp::Or, r) => Filter::Logic(get(*l, self), true, get(*r, self)),
            Expr::Binary(l, BinaryOp::And, r) => Filter::Logic(get(*l, self), false, get(*r, self)),
            Expr::Binary(l, BinaryOp::Math(op), r) => {
                Filter::Math(get(*l, self), op, get(*r, self))
            }
            Expr::Binary(l, BinaryOp::Ord(op), r) => Filter::Ord(get(*l, self), op, get(*r, self)),
            Expr::Binary(l, BinaryOp::Assign(op), r) => {
                let (l, r) = (get(*l, self), get(*r, self));
                match op {
                    AssignOp::Assign => Filter::Assign(l, r),
                    AssignOp::Update => Filter::Update(l, r),
                    AssignOp::UpdateWith(op) => Filter::UpdateMath(l, op, r),
                }
            }
            Expr::Ite(if_thens, else_) => {
                let if_thens = if_thens.into_iter().rev();
                if_thens.fold(*get(*else_, self), |acc, (if_, then_)| {
                    Filter::Ite(get(if_, self), get(then_, self), Box::new(acc))
                })
            }
            Expr::Path(f, path) => {
                let f = get(*f, self);
                use jaq_parse::path::Part;
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

    fn def(&mut self, id: FilterId, view: View, defs: &mir::Defs) -> Filter {
        todo!()
    }
}
