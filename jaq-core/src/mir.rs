//! Mid-level Intermediate Representation of definitions and filters.
//!
//! This is quite close to the output of parsing,
//! but replaces names by unique integers.
//! This makes the subsequent transformation step ,
//! That way, the subsequent transformation step(s)
//! always succeed and do not have to fight with shadowing.
//! But most importantly, this allows us to record recursive calls.

use crate::parse;
use alloc::{boxed::Box, string::String, vec::Vec};
use parse::filter::{BinaryOp, Filter as Expr, Fold};
use parse::{Arg, Error, Spanned};

pub type FilterId = usize;
type VarIdx = usize;
type ArgIdx = usize;

/*
type Arity = usize;
type NameArityMap = BTreeMap<String, BTreeMap<Arity, (Filter, Vec<FilterId>)>>;
*/

#[derive(Debug, Clone)]
pub enum Call {
    Def(FilterId),
    Arg(ArgIdx),
    Native(crate::filter::Native),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Default)]
pub struct Defs(Vec<Def>, Vec<(String, usize, crate::filter::Native)>);

pub type Filter = parse::filter::Filter<Call, VarIdx, Num>;

#[derive(Debug)]
pub struct Def {
    pub name: String,
    pub args: Vec<Arg>,
    pub children: Vec<FilterId>,
    ancestors: Vec<FilterId>,
    pub recursive: bool,
    pub body: Spanned<Filter>,
}

impl Def {
    pub fn arity(&self) -> usize {
        self.args.len()
    }
}

pub struct Ctx {
    errs: Vec<Error>,
    recs: Vec<FilterId>,
}

impl Defs {
    pub fn new(vars: Vec<String>) -> Self {
        use alloc::string::ToString;
        let root = Def {
            name: "".to_string(),
            args: vars.into_iter().map(Arg::make_var).collect(),
            children: Vec::new(),
            ancestors: Vec::new(),
            recursive: false,
            body: (Filter::Id, 0..0),
        };
        Self(Vec::from([root]), Vec::new())
    }

    pub fn get(&self, id: FilterId) -> &Def {
        &self.0[id]
    }

    pub fn insert_fn(&mut self, name: String, arity: usize, f: crate::filter::Native) {
        self.1.push((name, arity, f))
    }

    pub fn last_common_ancestor(&self, id1: FilterId, id2: FilterId) -> FilterId {
        let mut a1 = self.ancestors_and_me(id1);
        let mut a2 = self.ancestors_and_me(id2);
        let mut last = 0;
        while let (Some(a1), Some(a2)) = (a1.next(), a2.next()) {
            if a1 == a2 {
                last = a1
            } else {
                break;
            }
        }
        last
    }

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

    /// Retrieve the position of an argument of a filter, relative to all its ancestors.
    ///
    /// This does not try to find arguments of ancestors,
    /// but it will offset the index of the argument by the ancestor arguments.
    fn arg_position(&self, id: FilterId, name: &str) -> Option<usize> {
        let args: Vec<_> = self.0[id].args.iter().filter_map(|a| a.get_arg()).collect();
        let i = args.into_iter().rposition(|arg| arg == name)?;
        let ancestors = self.0[id].ancestors.iter();
        let ancestor_args = ancestors.flat_map(|aid| self.0[*aid].args.iter());
        Some(i + ancestor_args.filter(|a| a.get_arg().is_some()).count())
    }

    pub fn root_def(&mut self, def: parse::Def, errs: &mut Vec<Error>) {
        let mut ctx = Ctx {
            errs: core::mem::take(errs),
            recs: Vec::new(),
        };
        let root_id = 0;
        self.def(Vec::from([root_id]), def, &mut ctx);
        for rec_idx in ctx.recs {
            self.0[rec_idx].recursive = true;
        }
        *errs = ctx.errs;
    }

    pub fn root_filter(&mut self, filter: Spanned<parse::filter::Filter>, errs: &mut Vec<Error>) {
        let mut ctx = Ctx {
            errs: core::mem::take(errs),
            recs: Vec::new(),
        };
        let root_id = 0;
        self.0[root_id].body = self.filter(root_id, Vec::new(), filter, &mut ctx);
        *errs = ctx.errs;
    }

    fn def(&mut self, mut ancestors: Vec<FilterId>, def: parse::Def, ctx: &mut Ctx) {
        let id = self.0.len();
        self.0.push(Def {
            name: def.name,
            args: def.args,
            children: Vec::new(),
            ancestors: ancestors.clone(),
            // after MIR creation, we have to set all filters i with ctx.recursive[i] to defs[i].recursive
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
        let with_vars = |f, vars, ctx: &mut _| Box::new(self.filter(id, vars, f, ctx));
        let get = |f, ctx: &mut _| with_vars(f, vars.clone(), ctx);

        let result = match filter.0 {
            Expr::Call(name, args) => {
                //std::dbg!(&name, &args);
                let args: Vec<_> = args.into_iter().map(|arg| *get(arg, ctx)).collect();

                let ancestors: Vec<_> = self.ancestors_and_me(id).collect();
                //std::dbg!(id, &ancestors);
                for ancestor in &ancestors {
                    //std::dbg!("check ancestor", ancestor);
                    // we can call all previous children of ancestors
                    // we `rev()` here because later definitions shadow earlier ones
                    for child_idx in self.0[*ancestor].children.iter().rev() {
                        let child = &self.0[*child_idx];
                        if child.name != name || child.args.len() != args.len() {
                            continue;
                        }

                        //std::dbg!(child_idx);
                        // recursion
                        if ancestors.iter().any(|aid| aid == child_idx) {
                            //std::dbg!("recursion!");
                            //std::dbg!(&child.args);
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
                    if let Some(i) = self.arg_position(*ancestor, &name) {
                        return (Filter::Call(Call::Arg(i), args), filter.1);
                    }
                }

                let mut natives = self.1.iter();
                if let Some((_, _, native)) =
                    natives.find(|(name_, arity, _)| *name_ == name && *arity == args.len())
                {
                    Filter::Call(Call::Native(native.clone()), args)
                } else {
                    let error = "could not find function";
                    ctx.errs.push(Error::custom(filter.1.clone(), error));
                    Filter::Id
                }
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
