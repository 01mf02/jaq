//! Low-level Intermediate Representation of filters.
//!
//! Welcome to the machine room. Be careful to wear safety equipment.
//! The invariants in this module can be difficult to preserve.
//! `assert!` your way around here and watch your step.

use crate::filter::Filter;
use crate::mir::{self, DefId, MirFilter};
use crate::path::{self, Path};
use alloc::{boxed::Box, vec::Vec};
use jaq_parse::filter::{AssignOp, BinaryOp, Fold, KeyVal};

#[derive(Debug, Clone, Default)]
struct View {
    /// indices of variables in the execution context that are currently visible
    vars: Vec<usize>,
    args: Vec<usize>,
    recs: Vec<usize>,
}

impl View {
    fn truncate(&mut self, id: DefId, defs: &mir::Defs) {
        let vars = defs.vars(id).count();
        let args = defs.args(id).count() - vars;
        assert!(vars <= self.vars.len());
        assert!(args <= self.args.len());
        self.vars.truncate(vars);
        self.args.truncate(args);
    }
}

type Arity = usize;

#[derive(Default)]
struct Ctx {
    /// number of variables in the execution context at the current point
    vars: usize,
    /// non-variable arguments, earliest bound first
    args: Vec<(MirFilter, DefId, View)>,
    /// list of recursively defined filters with their arity (only variable arguments)
    /// and the number of bound variables (including variable arguments)
    /// that must be in the context at the time of calling
    recs: Vec<Rec>,
}

pub struct Rec {
    id: DefId,
    vars_len: usize,
    filter: Filter,
}

pub fn root_def(defs: &mir::Defs) -> (Filter, Vec<(Arity, Filter)>) {
    //std::dbg!(defs);
    let root_id = 0;
    //let vars = defs.get(root_id).args.len();
    let mut ctx = Ctx::default();
    let view = View::default();
    let f = ctx.def(root_id, view, defs);
    let recs = ctx.recs.into_iter();
    let recs = recs.map(|rec| (defs.get(rec.id).arity(), rec.filter));
    (f, recs.collect())
}

// this has to be fulfilled for the IDs of the filters in any `view.recs`
fn sorted_and_unique<T: Ord + core::hash::Hash + Clone>(iter: impl Iterator<Item = T>) -> bool {
    use itertools::Itertools;
    let v: Vec<_> = iter.collect();
    let mut sorted = v.clone();
    sorted.sort();
    v.iter().all_unique() && v == sorted
}

impl Ctx {
    fn def(&mut self, id: DefId, mut view: View, defs: &mir::Defs) -> Filter {
        let def = defs.get(id);
        //std::dbg!("processing def", (&def.name, id, &view));

        let var_args = def.args.iter().filter(|a| a.get_var().is_some()).count();
        view.vars.extend(self.vars..self.vars + var_args);
        self.vars += var_args;

        let view_vars_len = view.vars.len();
        for rec_id in def.children.iter().filter(|cid| defs.get(**cid).recursive) {
            //std::dbg!("processing recursive child", rec_id);
            let rec = defs.get(*rec_id);
            assert!(rec.args.iter().all(|a| a.get_var().is_some()));
            let new_rec_idx = self.recs.len();
            view.recs.push(new_rec_idx);
            // put in a bogus filter that we replace later
            self.recs.push(Rec {
                id: *rec_id,
                vars_len: self.vars,
                filter: Filter::Id,
            });
            // std::dbg!(&self.recs);
            let f = self.def(*rec_id, view.clone(), defs);
            self.recs[new_rec_idx].filter = f;
        }
        assert_eq!(view.vars.len(), view_vars_len);

        //std::dbg!("rec defs done for", id, &view);
        //std::dbg!("now for the body of the definition", &def.body);

        let out = self.filter(def.body.clone(), id, view, defs);
        self.vars -= var_args;
        out
    }

    /// Convert a MIR filter contained in a definition `id` to a LIR filter.
    // TODO: operate on borrowed filter
    // the problem here is that for calls to arguments,
    // we need to access self.args, but we cannot pass the filter contained to it
    // to this function, because it mutably borrows self
    fn filter(&mut self, f: MirFilter, id: DefId, mut view: View, defs: &mir::Defs) -> Filter {
        let get = |f, ctx: &mut Self| Box::new(ctx.filter(f, id, view.clone(), defs));
        use mir::Filter as Expr;

        //std::dbg!(self.vars);
        //std::dbg!(&view.vars);
        //std::dbg!(&f.0);
        match f.0 {
            Expr::Var(v) => Filter::Var(self.vars - view.vars[v] - 1),
            Expr::Call(mir::Call::Native(n), args) => {
                let args = args.into_iter().map(|a| *get(a, self));
                Filter::Native(n, args.collect())
            }
            Expr::Call(mir::Call::Arg(a), args) => {
                //std::dbg!("arg call");
                assert!(args.is_empty());
                assert!(sorted_and_unique(view.args.iter()));
                // all accessible arguments are in the context
                assert!(view.args.iter().all(|aidx| *aidx < self.args.len()));
                // only actual arguments are accessible
                assert_eq!(
                    view.args.len(),
                    defs.args(id).filter(|a| a.get_arg().is_some()).count()
                );
                let (f, id, view) = self.args[view.args[a]].clone();
                self.filter(f, id, view, defs)
            }
            Expr::Call(mir::Call::Def(did), args) => {
                let args_len = self.args.len();
                let dargs = &defs.get(did).args;
                let (var_arg_idxs, nonvar_arg_idxs) = defs.get(did).var_nonvar_arg_idxs();
                let var_args = var_arg_idxs.iter().map(|i| args[*i].clone());
                let nonvar_args = nonvar_arg_idxs.iter().map(|i| args[*i].clone());

                let var_args: Vec<_> = var_args
                    .map(|a| {
                        //std::dbg!(&view, self.vars);
                        let arg = self.filter(a, id, view.clone(), defs);
                        self.vars += 1;
                        // TODO: increase view.vars? probably not ...
                        arg
                    })
                    .collect();
                self.vars -= var_args.len();

                //std::dbg!(id);

                assert!(sorted_and_unique(
                    view.recs.iter().map(|ridx| self.recs[*ridx].id)
                ));
                // recursion!
                let out = if let Some(rec_idx) =
                    view.recs.iter().find(|ridx| self.recs[**ridx].id == did)
                {
                    //std::dbg!("call a recursive filter!", did);
                    //  std::dbg!(&self.recs);
                    //  std::dbg!(&view.recs);
                    // arguments bound in the called filter and its ancestors
                    let vars_len = self.recs[*rec_idx].vars_len;
                    Filter::Call {
                        id: *rec_idx,
                        skip: var_args.len() + self.vars - vars_len,
                    }
                } else {
                    let last_common = defs.last_common_ancestor(id, did);
                    let mut new_view = view.clone();
                    new_view.truncate(last_common, defs);
                    // we ban all recursive filters from the view that come "after"
                    // the filter that we are calling
                    // this relies on the property that filter IDs preserve order of definition
                    new_view.recs = new_view
                        .recs
                        .into_iter()
                        .take_while(|ridx| self.recs[*ridx].id < did)
                        .collect();
                    //std::dbg!(&view, &new_view);

                    for arg in nonvar_args {
                        new_view.args.push(self.args.len());
                        self.args.push((arg, id, view.clone()));
                    }

                    self.def(did, new_view, defs)
                };

                // here, we revert the order, because leftmost variable arguments are bound first, which means
                // they will appear *outermost* in the filter, thus have to be added *last* to the filter

                let filter = var_args.into_iter().rev().fold(out, |acc, arg| {
                    Filter::Pipe(Box::new(arg), true, Box::new(acc))
                });

                // std::dbg!("return from filter construction");
                self.args.truncate(self.args.len() - nonvar_arg_idxs.len());
                // we must be back to the original args len here
                assert_eq!(self.args.len(), args_len);

                filter
            }

            // variable-binding operators
            Expr::Binary(l, BinaryOp::Pipe(Some(_x)), r) => {
                let l = get(*l, self);

                view.vars.push(self.vars);
                self.vars += 1;
                let r = Box::new(self.filter(*r, id, view, defs));
                self.vars -= 1;

                //let r = self.bind(view, |ctx, view| Box::new(ctx.filter(*r, id, view, defs)));
                Filter::Pipe(l, true, r)
            }
            Expr::Fold(typ, Fold { xs, init, f, .. }) => {
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

            Expr::Binary(l, BinaryOp::Pipe(None), r) => {
                Filter::Pipe(get(*l, self), false, get(*r, self))
            }
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
}
