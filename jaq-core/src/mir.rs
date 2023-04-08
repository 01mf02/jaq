//! Mid-level Intermediate Representation of definitions and filters.
//!
//! This is quite close to the output of parsing,
//! but replaces names by unique integers.
//! This makes the subsequent transformation step ,
//! That way, the subsequent transformation step(s)
//! always succeed and do not have to fight with shadowing.
//! But most importantly, this allows us to record recursive calls.

use crate::parse;
use crate::path::{self, Path};
use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use parse::filter::{AssignOp, BinaryOp, Filter as Expr, Fold, KeyVal};
use parse::{Arg, Error, Spanned};

pub type FilterId = usize;
type VarIdx = usize;
type ArgIdx = usize;

/*
type Arity = usize;
type NameArityMap = BTreeMap<String, BTreeMap<Arity, (Filter, Vec<FilterId>)>>;
*/

#[derive(Clone)]
pub enum Call {
    Def(FilterId),
    Arg(ArgIdx),
}

#[derive(Clone)]
pub enum Num {
    Float(f64),
    Int(isize),
}

impl Num {
    fn parse(n: &str) -> Result<Self, Self> {
        if n.contains(['.', 'e', 'E']) {
            n.parse::<f64>().map(Num::Float).map_err(|_| Num::Float(0.))
        } else {
            n.parse::<isize>().map(Num::Int).map_err(|_| Num::Int(0))
        }
    }
}

pub struct Defs(Vec<Def>);

pub type Filter = parse::filter::Filter<Call, VarIdx, Num>;

struct Def {
    name: String,
    args: Vec<Arg>,
    children: Vec<FilterId>,
    ancestors: Vec<FilterId>,
    recursive: bool,
    body: Spanned<Filter>,
}

struct Ctx {
    errs: Vec<Error>,
    recs: Vec<FilterId>,
}

impl Defs {
    fn ancestors_and_me(&self, id: FilterId) -> impl Iterator<Item = FilterId> + '_ {
        use core::iter::once;
        self.0[id].ancestors.iter().copied().chain(once(id))
    }

    pub fn args(&self, id: FilterId) -> impl Iterator<Item = &Arg> + '_ {
        self.ancestors_and_me(id)
            .flat_map(|aid| self.0[aid].args.iter())
    }

    pub fn vars(&self, id: FilterId) -> impl Iterator<Item = &str> + '_ {
        self.args(id).filter_map(|a| a.get_var())
    }

    /// Return the filter IDs of those recursive filters that can be called from the current ID.
    pub fn recs(&self, id: FilterId) -> impl Iterator<Item = FilterId> + '_ {
        let ancestors = self.0[id].ancestors.iter();
        ancestors
            .flat_map(|aid| self.0[*aid].children.iter())
            .copied()
            .filter(|cid| self.0[*cid].recursive)
            .take_while(move |cid| *cid <= id)
    }

    fn def(&mut self, mut ancestors: Vec<FilterId>, def: parse::Def, ctx: &mut Ctx) {
        let id = self.0.len();
        self.0.push(Def {
            name: def.name,
            args: def.args,
            children: Vec::new(),
            ancestors: ancestors.clone(),
            // TODO: at the end, set all filters i with ctx.recursive[i] to defs[i].recursive
            recursive: false,
            // for recursion, we want to be able to refer to the filter even before we know
            // what is its body, which is why we insert a bogus body for now
            // that we replace later by the real body
            body: (Filter::Id, 0..0),
        });
        if let Some(parent) = ancestors.last() {
            self.0[*parent].children.push(id);
        }

        ancestors.push(id);

        for d in def.defs {
            self.def(ancestors.clone(), d, ctx);
        }

        self.0[id].body = self.filter(id, Vec::new(), def.body, ctx);
    }

    fn filter(
        &self,
        id: FilterId,
        mut vars: Vec<String>,
        filter: Spanned<parse::filter::Filter>,
        ctx: &mut Ctx,
    ) -> Spanned<Filter> {
        let mut with_vars = |f, vars, ctx: &mut _| Box::new(self.filter(id, vars, f, ctx));
        let mut get = |f, ctx: &mut _| with_vars(f, vars.clone(), ctx);

        let result = match filter.0 {
            Expr::Call(name, args) => {
                let args: Vec<_> = args.into_iter().map(|arg| *get(arg, ctx)).collect();

                for ancestor in self.ancestors_and_me(id) {
                    // we can call all previous children of ancestors
                    for child_idx in self.0[ancestor].children.iter().rev() {
                        let child = &self.0[*child_idx];
                        if child.name != name || child.args.len() != args.len() {
                            continue;
                        }

                        if self.ancestors_and_me(id).any(|aid| aid == *child_idx) {
                            if child.args.iter().any(|a| a.get_arg().is_some()) {
                                let error = "attempting to recursively call filter with non-variable argument";
                                ctx.errs.push(Error::custom(filter.1.clone(), error));
                            }

                            ctx.recs.push(*child_idx);
                        }

                        return (Filter::Call(Call::Def(*child_idx), args), filter.1);
                    }

                    // we cannot call arguments with arguments (no higher-order!)
                    if !args.is_empty() {
                        continue;
                    }

                    // calls to arguments
                    let ancestor_args = self.0[ancestor].args.iter();
                    let ancestor_args: Vec<_> = ancestor_args.filter_map(|a| a.get_arg()).collect();
                    if let Some(i) = ancestor_args.into_iter().rposition(|arg| arg == name) {
                        return (Filter::Call(Call::Arg(i), args), filter.1);
                    }
                }

                let error = "could not find function";
                ctx.errs.push(Error::custom(filter.1.clone(), error));
                Filter::Id
            }
            Expr::Var(v) => {
                let local_vars = vars.iter().map(|v| &**v);
                let arg_vars = self.args(id).filter_map(|a| a.get_var());
                let vars: Vec<_> = arg_vars.chain(local_vars).collect();
                Filter::Var(vars.iter().rposition(|i| *i == v).unwrap_or_else(|| {
                    ctx.errs
                        .push(Error::custom(filter.1.clone(), "undefined variable"));
                    0
                }))
            }
            Expr::Binary(l, BinaryOp::Pipe(Some(x)), r) => {
                let l = get(*l, ctx);
                vars.push(x.clone());
                let r = with_vars(*r, vars, ctx);
                Filter::Binary(l, BinaryOp::Pipe(Some(x)), r)
            }
            Expr::Fold(typ, Fold { xs, x, init, f }) => {
                let (xs, init) = (get(*xs, ctx), get(*init, ctx));
                vars.push(x.clone());
                let f = with_vars(*f, vars, ctx);
                Filter::Fold(typ, Fold { xs, x, init, f })
            }
            Expr::Id => Filter::Id,
            Expr::Num(n) => Filter::Num(Num::parse(&n).unwrap_or_else(|n| {
                let err = match n {
                    Num::Float(_) => "cannot interpret as floating-point number",
                    Num::Int(_) => "cannot interpret as machine-size integer",
                };
                ctx.errs.push(Error::custom(filter.1.clone(), err));
                n
            })),
            Expr::Str(s) => Filter::Str(s),
            Expr::Array(a) => Filter::Array(a.map(|a| get(*a, ctx))),
            Expr::Object(o) => {
                Filter::Object(o.into_iter().map(|kv| kv.map(|f| *get(f, ctx))).collect())
            }
            Expr::Try(f) => Filter::Try(get(*f, ctx)),
            Expr::Neg(f) => Filter::Neg(get(*f, ctx)),
            Expr::Recurse => Filter::Recurse,

            Expr::Binary(l, op, r) => Filter::Binary(get(*l, ctx), op, get(*r, ctx)),
            Expr::Ite(if_thens, else_) => {
                let if_thens = if_thens
                    .into_iter()
                    .map(|(i, t)| (*get(i, ctx), *get(t, ctx)));
                Filter::Ite(if_thens.collect(), get(*else_, ctx))
            }
            Expr::Path(f, path) => {
                let f = get(*f, ctx);
                let path = path
                    .into_iter()
                    .map(|(p, opt)| (p.map(|p| *get(p, ctx)), opt));
                Filter::Path(f, path.collect())
            }
        };
        (result, filter.1)
    }
}
