//! Mid-level Intermediate Representation of definitions and filters.
//!
//! This is quite close to the output of parsing,
//! but replaces names by unique integers.
//! That way, the subsequent transformation step(s)
//! always succeed and do not have to fight with shadowing.
//! But most importantly, this allows us to record recursive calls.

use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt;
use jaq_syn::filter::{BinaryOp, Filter as Expr, Fold};
use jaq_syn::{Arg, Spanned};

type HirFilter = Spanned<jaq_syn::filter::Filter>;
pub type MirFilter = Spanned<Filter>;

pub type DefId = usize;
type VarIdx = usize;
type ArgIdx = usize;

#[derive(Debug, Clone)]
pub enum Call {
    Def(DefId),
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
            n.parse::<f64>().map(Num::Float).map_err(|_| Num::Float(0.))
        } else {
            n.parse::<isize>().map(Num::Int).map_err(|_| Num::Int(0))
        }
    }
}

pub type Filter = jaq_syn::filter::Filter<Call, VarIdx, Num>;

#[derive(Debug)]
pub struct Def {
    // TODO: convert name and args to Call
    pub name: String,
    pub args: Vec<Arg>,
    pub children: Vec<DefId>,
    ancestors: Vec<DefId>,
    pub recursive: bool,
    pub body: Spanned<Filter>,
}

impl Def {
    pub fn arity(&self) -> usize {
        self.args.len()
    }

    /// Return the indices of variable and nonvariable arguments of the definition.
    ///
    /// Example: if we have the arguments $f; g; $h; i, then we obtain
    /// the variable indices [0, 2] and
    /// the nonvariable indices [1, 3].
    ///
    /// Does not consider ancestors.
    pub fn var_nonvar_arg_idxs(&self) -> (Vec<usize>, Vec<usize>) {
        (0..self.args.len()).partition(|i| self.args[*i].is_var())
    }
}

/// Link names and arities to corresponding filters.
///
/// For example, if we define a filter `def map(f): [.[] | f]`,
/// then the definitions will associate `map/1` to its definition.
pub struct Defs(Vec<Def>);

impl Defs {
    /// Create new definitions that have access to global variables of the given names.
    pub fn new(vars: Vec<String>) -> Self {
        use alloc::string::ToString;
        let root = Def {
            name: "".to_string(),
            args: vars.into_iter().map(Arg::new_var).collect(),
            children: Vec::new(),
            ancestors: Vec::new(),
            recursive: false,
            body: (Filter::Id, 0..0),
        };
        Self(Vec::from([root]))
    }

    pub fn get(&self, id: DefId) -> &Def {
        &self.0[id]
    }

    pub fn smallest_common_ancestor(&self, id1: DefId, id2: DefId) -> DefId {
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

    /// Return the IDs of the ancestors of a definition and itself.
    fn ancestors_and_me(&self, id: DefId) -> impl Iterator<Item = DefId> + '_ {
        use core::iter::once;
        self.0[id].ancestors.iter().copied().chain(once(id))
    }

    /// Return all arguments bound in a definition and its ancestors.
    pub fn args(&self, id: DefId) -> impl Iterator<Item = &Arg> + '_ {
        self.ancestors_and_me(id)
            .flat_map(|aid| self.0[aid].args.iter())
    }

    /// Retrieve the position of an argument of a filter, relative to all its ancestors.
    ///
    /// This does not try to find arguments of ancestors,
    /// but it will offset the index of the argument by the ancestor arguments.
    fn nonvar_arg_position(&self, id: DefId, name: &str) -> Option<usize> {
        let args = self.0[id].args.iter();
        let filter_args: Vec<_> = args.filter_map(|a| a.get_filter()).collect();
        let i = filter_args.into_iter().rposition(|arg| arg == name)?;
        let ancestors = self.0[id].ancestors.iter();
        let ancestor_args = ancestors.flat_map(|aid| self.0[*aid].args.iter());
        Some(i + ancestor_args.filter(|a| !a.is_var()).count())
    }
}

const ROOT_ID: usize = 0;

/// HIR to MIR transformation.
pub struct Ctx {
    /// errors occurred during transformation
    pub errs: Vec<Spanned<Error>>,
    /// IDs of recursive definitions
    recs: Vec<DefId>,
    /// accessible defined filters
    pub(crate) defs: Defs,
    /// accessible native filters
    native: Vec<(String, usize, crate::filter::Native)>,
}

pub enum Error {
    RecCallWithFilterArg,
    Undefined(Arg),
    Num(Num),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::RecCallWithFilterArg => "recursive call of a filter with filter argument",
            Self::Undefined(a) if a.is_var() => "undefined variable",
            Self::Undefined(_) => "undefined filter",
            Self::Num(Num::Float(_)) => "cannot interpret as floating-point number",
            Self::Num(Num::Int(_)) => "cannot interpret as machine-size integer",
        }
        .fmt(f)
    }
}

impl Ctx {
    /// Initialise new context with list of global variables.
    ///
    /// When running a filter produced by this context,
    /// values corresponding to the variables have to be supplied in the execution context.
    pub fn new(vars: Vec<String>) -> Self {
        Self {
            errs: Vec::new(),
            recs: Vec::new(),
            native: Vec::new(),
            defs: Defs::new(vars),
        }
    }

    /// Add a native filter with given name and arity.
    pub fn insert_native(&mut self, name: String, arity: usize, f: crate::filter::Native) {
        self.native.push((name, arity, f))
    }

    /// Add native filters with given names and arities.
    pub fn insert_natives(
        &mut self,
        natives: impl IntoIterator<Item = (String, usize, crate::filter::Native)>,
    ) {
        natives
            .into_iter()
            .for_each(|(name, arity, f)| self.insert_native(name, arity, f))
    }

    /// Import parsed definitions, such as obtained from the standard library.
    ///
    /// Errors that might occur include undefined variables, for example.
    pub fn insert_defs(&mut self, defs: impl IntoIterator<Item = jaq_syn::Def>) {
        defs.into_iter().for_each(|def| self.root_def(def));
    }

    /// Insert a root definition.
    pub fn root_def(&mut self, def: jaq_syn::Def) {
        self.def(Vec::from([ROOT_ID]), def);
        for rec_idx in &self.recs {
            self.defs.0[*rec_idx].recursive = true;
        }
    }

    /// Insert a root filter.
    pub fn root_filter(&mut self, filter: HirFilter) {
        self.defs.0[ROOT_ID].body = self.filter(ROOT_ID, Vec::new(), filter);
    }

    fn def(&mut self, mut ancestors: Vec<DefId>, def: jaq_syn::Def) {
        // generate a fresh definition ID
        let id: DefId = self.defs.0.len();
        self.defs.0.push(Def {
            name: def.lhs.name,
            args: def.lhs.args,
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
            self.defs.0[*parent].children.push(id);
        }

        ancestors.push(id);

        for d in def.rhs.defs {
            self.def(ancestors.clone(), d);
        }

        self.defs.0[id].body = self.filter(id, Vec::new(), def.rhs.body);
    }

    fn filter(&mut self, id: DefId, mut vars: Vec<String>, f: HirFilter) -> MirFilter {
        let with_vars = |f, vars, ctx: &mut Self| Box::new(ctx.filter(id, vars, f));
        let get = |f, ctx: &mut Self| with_vars(f, vars.clone(), ctx);

        let result = match f.0 {
            Expr::Call(name, args) => {
                //std::dbg!(&name, &args);
                let args: Vec<_> = args.into_iter().map(|arg| *get(arg, self)).collect();

                let ancestors: Vec<_> = self.defs.ancestors_and_me(id).collect();
                //std::dbg!(id, &ancestors);
                for ancestor in &ancestors {
                    //std::dbg!("check ancestor", ancestor);
                    // we can call all previous children of ancestors
                    // we `rev()` here because later definitions shadow earlier ones
                    for child_idx in self.defs.0[*ancestor].children.iter().rev() {
                        let child = &self.defs.0[*child_idx];
                        if child.name != name || child.args.len() != args.len() {
                            continue;
                        }

                        //std::dbg!(child_idx);
                        // recursion
                        if ancestors.iter().any(|aid| aid == child_idx) {
                            //std::dbg!("recursion!");
                            //std::dbg!(&child.args);
                            if child.args.iter().any(|a| !a.is_var()) {
                                self.errs.push((Error::RecCallWithFilterArg, f.1.clone()));
                            }

                            self.recs.push(*child_idx);
                        }

                        return (Filter::Call(Call::Def(*child_idx), args), f.1);
                    }

                    // we cannot call arguments with arguments (no higher-order!)
                    if !args.is_empty() {
                        continue;
                    }

                    // calls to nonvariable arguments
                    if let Some(i) = self.defs.nonvar_arg_position(*ancestor, &name) {
                        return (Filter::Call(Call::Arg(i), args), f.1);
                    }
                }

                let mut natives = self.native.iter();
                if let Some((_, _, native)) =
                    natives.find(|(name_, arity, _)| *name_ == name && *arity == args.len())
                {
                    Filter::Call(Call::Native(native.clone()), args)
                } else {
                    self.errs
                        .push((Error::Undefined(Arg::new_filter(name)), f.1.clone()));
                    Filter::Id
                }
            }
            Expr::Var(v) => {
                let local_vars = vars.iter().map(|v| &**v);
                let arg_vars = self.defs.args(id).filter_map(|a| a.get_var());
                let vars: Vec<_> = arg_vars.chain(local_vars).collect();
                Filter::Var(vars.iter().rposition(|i| *i == v).unwrap_or_else(|| {
                    self.errs
                        .push((Error::Undefined(Arg::new_var(v)), f.1.clone()));
                    0
                }))
            }
            Expr::Binary(l, BinaryOp::Pipe(Some(x)), r) => {
                let l = get(*l, self);
                vars.push(x.clone());
                let r = with_vars(*r, vars, self);
                Filter::Binary(l, BinaryOp::Pipe(Some(x)), r)
            }
            Expr::Fold(typ, Fold { xs, x, init, f }) => {
                let (xs, init) = (get(*xs, self), get(*init, self));
                vars.push(x.clone());
                let f = with_vars(*f, vars, self);
                Filter::Fold(typ, Fold { xs, x, init, f })
            }
            Expr::Id => Filter::Id,
            Expr::Num(n) => Filter::Num(Num::parse(&n).unwrap_or_else(|n| {
                self.errs.push((Error::Num(n), f.1.clone()));
                n
            })),
            Expr::Str(s) => Filter::Str(s.map(|f| *get(f, self))),
            Expr::Array(a) => Filter::Array(a.map(|a| get(*a, self))),
            Expr::Object(o) => {
                Filter::Object(o.into_iter().map(|kv| kv.map(|f| *get(f, self))).collect())
            }
            Expr::Try(f) => Filter::Try(get(*f, self)),
            Expr::Neg(f) => Filter::Neg(get(*f, self)),
            Expr::Recurse => Filter::Recurse,

            Expr::Binary(l, op, r) => Filter::Binary(get(*l, self), op, get(*r, self)),
            Expr::Ite(if_thens, else_) => {
                let if_thens = if_thens
                    .into_iter()
                    .map(|(i, t)| (*get(i, self), *get(t, self)));
                Filter::Ite(if_thens.collect(), else_.map(|else_| get(*else_, self)))
            }
            Expr::TryCatch(try_, catch_) => {
                Filter::TryCatch(get(*try_, self), catch_.map(|c| get(*c, self)))
            }
            Expr::Path(f, path) => {
                let f = get(*f, self);
                let path = path
                    .into_iter()
                    .map(|(p, opt)| (p.map(|p| *get(p, self)), opt));
                Filter::Path(f, path.collect())
            }
        };
        (result, f.1)
    }
}
