//! Program compilation.

use crate::load::{self, lex, parse};
use crate::{ops, Bind as Arg, Filter};
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::{boxed::Box, string::String, vec::Vec};

type NativeId = usize;
type ModId = usize;
type VarId = usize;
type VarSkip = usize;
type Arity = usize;

/// Index of a term in the look-up table.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermId(pub(crate) usize);

/// Look-up table for terms and functions.
#[derive(Clone, Debug)]
pub struct Lut<F> {
    /// `terms[tid]` yields the term corresponding to the term ID `tid`
    pub(crate) terms: Vec<Term>,
    pub(crate) funs: Vec<F>,
}

impl<F> Default for Lut<F> {
    fn default() -> Self {
        Lut {
            terms: Vec::new(),
            funs: Vec::new(),
        }
    }
}

impl<F> Default for Filter<F> {
    fn default() -> Self {
        Self(TermId(0), Lut::new([Term::Id].into()))
    }
}

impl<F> Lut<F> {
    fn new(terms: Vec<Term>) -> Self {
        let funs = Vec::new();
        Self { terms, funs }
    }

    fn map_funs<F2>(self, f: impl Fn(F) -> F2) -> Lut<F2> {
        Lut {
            funs: self.funs.into_iter().map(f).collect(),
            terms: self.terms,
        }
    }

    fn insert_term(&mut self, t: Term) -> TermId {
        let tid = self.terms.len();
        self.terms.push(t);
        TermId(tid)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Tailrec {
    Throw,
    Catch,
}

#[derive(Clone, Debug)]
pub(crate) enum Term<T = TermId> {
    /// Identity (`.`)
    Id,
    /// Recursion (`..`)
    Recurse,
    ToString,

    Int(isize),
    Num(String),
    Str(String),
    /// Array construction (`[f]`)
    Arr(T),
    /// Empty object (`{}`)
    ObjEmpty,
    /// Singleton object (`{f: g}`)
    ObjSingle(T, T),

    /// Bound variable (`$x`), label (`label $x`), or filter argument (`a`)
    Var(VarId),
    /// Call to a filter (`filter`, `filter(…)`)
    CallDef(TermId, Box<[Arg<T>]>, VarSkip, Option<Tailrec>),
    Native(NativeId, Box<[Arg<T>]>),

    /// Binding of a break label (`label $x | f`)
    Label(T),
    /// Negation operation (`-f`)
    Neg(T),
    /// Variable binding (`f as $x | g`) if identifier (`x`) is given, otherwise
    /// application (`f | g`)
    Pipe(T, Option<Pattern<T>>, T),
    /// Concatenation (`f, g`)
    Comma(T, T),
    /// Assignment (`f = g`)
    Assign(T, T),
    /// Update-assignment (`f |= g`)
    Update(T, T),
    /// Arithmetical update-assignment (`f += g`, `f -= g`, `f *= g`, `f /= g`, `f %= g`)
    UpdateMath(T, ops::Math, T),
    /// Alternation update-assignment (`f //= g`)
    UpdateAlt(T, T),
    /// Logical operation (`f and g`, `f or g`)
    Logic(T, bool, T),
    /// Arithmetical operation (`f + g`, `f - g`, `f * g`, `f / g`, `f % g`)
    Math(T, ops::Math, T),
    /// Comparison operation (`f < g`, `f <= g`, `f > g`, `f >= g`, `f == g`, `f != g`)
    Cmp(T, ops::Cmp, T),
    /// Alternation (`f // g`)
    Alt(T, T),
    /// Try-catch (`try f catch g`)
    TryCatch(T, T),
    /// If-then-else (`if f then g else h end`)
    Ite(T, T, T),
    /// `reduce` and `foreach`
    ///
    ///  Assuming that `xs` evaluates to `x0`, `x1`, ..., `xn`,
    /// `reduce xs as $x (init; f)` evaluates to
    ///
    /// ~~~ text
    /// init
    /// | x0 as $x | f
    /// | ...
    /// | xn as $x | f
    /// ~~~
    ///
    /// and `foreach xs as $x (init; f; project)` evaluates to
    ///
    /// ~~~ text
    /// init |
    /// x0 as $x | f | project,
    /// ...
    /// xn as $x | f | project,
    /// empty
    /// ~~~
    Fold(T, Pattern<T>, T, T, Fold<T>),
    Path(T, crate::path::Path<T>),
}

#[derive(Clone, Debug)]
pub(crate) enum Fold<T> {
    Reduce,
    Foreach(Option<T>),
}

impl<T> Default for Term<T> {
    fn default() -> Self {
        Self::Id
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Pattern<F> {
    Var,
    Idx(Vec<(F, Self)>),
}

/// Compilation error.
pub type Error<S> = (S, Undefined);

/// Compilation errors.
pub type Errors<S, P> = load::Errors<S, P, Vec<Error<S>>>;

/// Type of an undefined symbol.
#[derive(Debug)]
#[non_exhaustive]
pub enum Undefined {
    /// module
    Mod,
    /// variable
    Var,
    /// label variable
    Label,
    /// filter with arity
    Filter(Arity),
}

impl Undefined {
    /// String representation of an unexpected symbol type.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Var => "variable",
            Self::Mod => "module",
            Self::Label => "label",
            Self::Filter(_arity) => "filter",
        }
    }
}

/// jq program compiler.
///
/// This contains strings of type `S` and native functions of type `F`.
pub struct Compiler<S, F> {
    lut: Lut<(Sig<S>, F)>,

    /// `mod_map[mid]` yields all top-level definitions contained inside a module with ID `mid`
    mod_map: Vec<Vec<(Sig<S>, Def)>>,

    imported_mods: Vec<(ModId, S)>,
    included_mods: Vec<ModId>,

    global_vars: Vec<S>,
    imported_vars: Vec<(S, ModId)>,

    locals: Locals<S>,

    /// `tailrecs` stores every tail-recursive definition `id`
    tailrecs: BTreeSet<TermId>,

    errs: Vec<Error<S>>,
}

// TODO: remove S: Default
impl<S: Default, F> Default for Compiler<S, F> {
    fn default() -> Self {
        Self {
            lut: Lut::default(),
            mod_map: Vec::new(),
            imported_mods: Vec::new(),
            included_mods: Vec::new(),
            global_vars: Vec::new(),
            imported_vars: Vec::new(),
            tailrecs: BTreeSet::new(),
            locals: Locals::default(),
            errs: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct Sig<S, A = Arg> {
    name: S,
    // TODO: we could analyse for each argument whether it is TR, and
    // use this when converting args at callsite
    args: Box<[A]>,
}

fn bind_from<T>(s: &str, x: T) -> Arg<T> {
    if s.starts_with('$') {
        Arg::Var(x)
    } else {
        Arg::Fun(x)
    }
}

fn binds<T, U: Copy>(binds: &[Arg<T>], args: &[U]) -> Box<[Arg<U>]> {
    assert!(binds.len() == args.len());
    let args = binds.iter().zip(args);
    args.map(|(bind, id)| bind.as_ref().map(|_| *id)).collect()
}

#[derive(Clone, Debug)]
struct Def {
    id: TermId,
    /// true if function is recursive, i.e. it contains calls to itself
    rec: bool,
    /// true if all calls to this function from itself are tail-recursive
    tailrec: bool,
}

impl<S: Eq, A> Sig<S, A> {
    fn matches(&self, name: S, args: &[TermId]) -> bool {
        name == self.name && args.len() == self.args.len()
    }
}

impl Def {
    fn call(&self, args: Box<[Arg<TermId>]>, vars: usize) -> Term {
        // we pretend that the function call is tail-recursive,
        // and at the very end of compilation, we will correct calls
        // to non-tail-recursive functions
        Term::CallDef(self.id, args, vars, Some(Tailrec::Catch))
    }
}

/// Store a map of vectors plus the sum of the lengths of all vectors.
struct MapVecLen<S> {
    bound: MapVec<S, usize>,
    total: usize,
}

impl<S> Default for MapVecLen<S> {
    fn default() -> Self {
        Self {
            bound: MapVec::default(),
            total: 0,
        }
    }
}

impl<S: Ord> MapVecLen<S> {
    fn push(&mut self, name: S) {
        self.total += 1;
        self.bound.push(name, self.total)
    }

    fn pop(&mut self, name: &S) {
        assert_eq!(self.bound.pop(name), Some(self.total));
        self.total -= 1;
    }

    fn is_empty(&self) -> bool {
        self.bound.is_empty() && self.total == 0
    }
}

enum Fun<S> {
    Arg,
    Parent(Box<[Arg<S>]>, Def),
    // Tr is for tail-rec allowed funs
    Sibling(Box<[Arg<S>]>, Def, Tr),
}

/// Single binding.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Bind<V, L = V, F = V> {
    /// binding to a variable
    Var(V),
    /// binding to a break label
    Label(L),
    /// binding to a filter
    Fun(F),
}

struct Locals<S> {
    // usize = number of vars
    funs: MapVec<(S, Arity), (Fun<S>, usize)>,
    vars: MapVecLen<Bind<S>>,
    parents: Tr,
}

impl<S> Default for Locals<S> {
    fn default() -> Self {
        Self {
            funs: MapVec::default(),
            vars: MapVecLen::default(),
            parents: Tr::default(),
        }
    }
}

struct MapVec<K, V>(BTreeMap<K, Vec<V>>);

impl<K, V> Default for MapVec<K, V> {
    fn default() -> Self {
        Self(BTreeMap::new())
    }
}

impl<K: Ord, V> MapVec<K, V> {
    fn push(&mut self, k: K, v: V) {
        self.0.entry(k).or_default().push(v)
    }

    fn pop(&mut self, k: &K) -> Option<V> {
        let vs = self.0.get_mut(k)?;
        let v = vs.pop()?;
        if vs.is_empty() {
            self.0.remove(k);
        }
        Some(v)
    }

    fn get_last(&self, k: &K) -> Option<&V> {
        self.0.get(k)?.last()
    }

    fn get_last_mut(&mut self, k: &K) -> Option<&mut V> {
        self.0.get_mut(k)?.last_mut()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<S: Copy + Ord> Locals<S> {
    fn push_sibling(&mut self, name: S, args: Box<[Arg<S>]>, def: Def) {
        let tr = self.parents.clone();
        self.funs.push(
            (name, args.len()),
            (Fun::Sibling(args, def, tr), self.vars.total),
        );
    }

    fn pop_sibling(&mut self, name: S, arity: Arity) -> (Box<[Arg<S>]>, Def, Tr) {
        let (y, vars) = match self.funs.pop(&(name, arity)) {
            Some((Fun::Sibling(args, def, tr), vars)) => ((args, def, tr), vars),
            _ => panic!(),
        };
        assert_eq!(self.vars.total, vars);
        y
    }

    fn push_parent(&mut self, name: S, args: Box<[Arg<S>]>, def: Def) {
        self.parents.insert(def.id);

        let vars = self.vars.total;

        for arg in args.iter() {
            match arg {
                Arg::Var(v) => self.vars.push(Bind::Var(*v)),
                Arg::Fun(f) => self.push_arg(*f),
            }
        }

        self.funs
            .push((name, args.len()), (Fun::Parent(args, def), vars));
    }

    fn pop_parent(&mut self, name: S, arity: Arity) -> Def {
        let (args, def, vars) = match self.funs.pop(&(name, arity)) {
            Some((Fun::Parent(args, def), vars)) => (args, def, vars),
            _ => panic!(),
        };
        for arg in args.iter().rev() {
            match arg {
                Arg::Var(v) => self.vars.pop(&Bind::Var(*v)),
                Arg::Fun(f) => self.pop_arg(*f),
            }
        }
        assert_eq!(self.vars.total, vars);
        assert!(self.parents.remove(&def.id));
        def
    }

    fn push_arg(&mut self, name: S) {
        self.vars.push(Bind::Fun(name));
        self.funs.push((name, 0), (Fun::Arg, self.vars.total));
    }

    fn pop_arg(&mut self, name: S) {
        let vars = match self.funs.pop(&(name, 0)) {
            Some((Fun::Arg, vars)) => vars,
            _ => panic!(),
        };
        assert_eq!(self.vars.total, vars);
        self.vars.pop(&Bind::Fun(name));
    }

    fn call(&mut self, name: S, args: &[TermId], tr: &Tr) -> Option<Term> {
        Some(match self.funs.get_last_mut(&(name, args.len()))? {
            (Fun::Arg, vars) => Term::Var(self.vars.total - *vars),
            (Fun::Sibling(args_, def, tr_), vars) => {
                // we are at a position that may only call `tr` tail-recursively and
                // we call a sibling that may only call `tr_` tail-recursively,
                // so we update the sibling with additional `tr`
                tr_.retain(|id| tr.contains(id));
                def.call(binds(args_, args), self.vars.total - *vars)
            }
            (Fun::Parent(args_, def), vars) => {
                // we have a recursive call!
                def.rec = true;
                // if the current position does not allow for
                // a tail-recursive call to this function, then
                // we know for sure that the function is not tail-recursive!
                if !tr.contains(&def.id) {
                    def.tailrec = false;
                }
                let call = Some(Tailrec::Throw);
                Term::CallDef(def.id, binds(args_, args), self.vars.total - *vars, call)
            }
        })
    }

    fn is_empty(&self) -> bool {
        self.funs.is_empty() && self.vars.is_empty() && self.parents.is_empty()
    }
}

// any ID in Tr is an ID of a function f, and
// any call to some `f` in Tr from the current position is *not* tail-recursive
type Tr = BTreeSet<TermId>;

impl<'s, F> Compiler<&'s str, F> {
    /// Supply functions with given signatures.
    pub fn with_funs(mut self, funs: impl IntoIterator<Item = (&'s str, Box<[Arg]>, F)>) -> Self {
        self.lut.funs = funs
            .into_iter()
            .map(|(name, args, f)| (Sig { name, args }, f))
            .collect();
        self
    }

    /// Assume the existence of global variables with given names.
    ///
    /// The names all have to start with `$`.
    /// For execution, the corresponding values have to be provided via [`crate::Ctx::new`].
    pub fn with_global_vars(self, global_vars: impl IntoIterator<Item = &'s str>) -> Self {
        Self {
            global_vars: global_vars.into_iter().collect(),
            ..self
        }
    }

    /// Compile the given modules.
    pub fn compile<P>(
        mut self,
        mods: load::Modules<&'s str, P>,
    ) -> Result<Filter<F>, Errors<&'s str, P>> {
        self.imported_vars = mods
            .iter()
            .enumerate()
            .flat_map(|(mid, (_file, m))| m.vars.iter().map(move |(_path, x, _meta)| (*x, mid)))
            .collect();

        let mut errs = Vec::new();
        for (file, m) in mods {
            self.module(m);
            if !self.errs.is_empty() {
                errs.push((file, core::mem::take(&mut self.errs)));
            }
            assert!(self.locals.is_empty());
        }

        // uncomment the following line to disable tail-call optimisation (TCO)
        //self.tailrecs.clear();

        // only after the end, we know which definitions are actually tail-recursive
        // before, we assumed that every call is tail-recursive
        // (that way, we can conveniently record whether a call to a function
        // happens from inside or outside the function)
        // therefore, we only have to adapt calls to non-tail-recursive functions here
        for t in self.lut.terms.iter_mut() {
            match t {
                Term::CallDef(id, .., tr) if !self.tailrecs.contains(id) => *tr = None,
                _ => (),
            }
        }

        /*
        for (i, t) in self.lut.terms.iter().enumerate() {
            std::println!("{i} -> {t:?}");
        }
        */

        if errs.is_empty() {
            // the main filter corresponds to the last definition of the last module
            let (main_sig, main_def) = self.mod_map.last().unwrap().last().unwrap();
            assert!(main_sig.matches("main", &[]));
            assert!(!main_def.rec && main_def.tailrec);
            //std::println!("main: {:?}", main_def.id);
            Ok(Filter(main_def.id, self.lut.map_funs(|(_sig, f)| f)))
        } else {
            Err(errs)
        }
    }

    fn with_label<T>(&mut self, label: &'s str, f: impl FnOnce(&mut Self) -> T) -> T {
        self.locals.vars.push(Bind::Label(label));
        let y = f(self);
        self.locals.vars.pop(&Bind::Label(label));
        y
    }

    fn with_vars<T>(&mut self, vars: &[&'s str], f: impl FnOnce(&mut Self) -> T) -> T {
        vars.iter()
            .for_each(|v| self.locals.vars.push(Bind::Var(v)));
        let y = f(self);
        vars.iter()
            .rev()
            .for_each(|v| self.locals.vars.pop(&Bind::Var(v)));
        y
    }

    fn module(&mut self, m: load::Module<&'s str>) {
        self.imported_mods.clear();
        self.included_mods.clear();
        for (mid, as_) in m.mods {
            match as_ {
                None => self.included_mods.push(mid),
                Some(as_) => self.imported_mods.push((mid, as_)),
            }
        }

        m.body.iter().for_each(|def| self.def_pre(def));
        let defs = m.body.into_iter().rev().map(|def| self.def_post(def));
        let mut defs: Vec<_> = defs.collect();
        defs.reverse();
        self.mod_map.push(defs)
    }

    /// Create a placeholder sibling for a definition.
    ///
    /// Once we have processed all places where the sibling can be called from outside,
    /// we can then call `def_post`.
    fn def_pre(&mut self, d: &parse::Def<&'s str>) {
        let tid = self.lut.insert_term(Term::Id);
        let args = d.args.iter().map(|a| bind_from(a, *a)).collect();
        // by default, we assume that the function is not recursive and
        // that all recursive calls to it are tail-recursive
        let def = Def {
            id: tid,
            rec: false,
            tailrec: true,
        };
        // furthermore, we initially assume that the function can call
        // any of its ancestors without breaking their tail-recursiveness
        self.locals.push_sibling(d.name, args, def)
    }

    /// Compile a placeholder sibling with its corresponding definition.
    fn def_post(&mut self, d: parse::Def<&'s str>) -> (Sig<&'s str, Arg>, Def) {
        let (args, def, mut tr) = self.locals.pop_sibling(d.name, d.args.len());
        let tid = def.id;
        self.locals.push_parent(d.name, args, def);
        // at the beginning, we assume that any function can call itself tail-recursively
        assert!(tr.insert(tid));
        self.lut.terms[tid.0] = self.term(d.body, &tr);
        let def = self.locals.pop_parent(d.name, d.args.len());
        // only if there is at least one recursive call and all calls are tail-recursive,
        // then the definition is tail-recursive
        if def.rec && def.tailrec {
            self.tailrecs.insert(def.id);
        }
        let sig = Sig {
            name: d.name,
            args: d.args.iter().map(|a| bind_from(a, ())).collect(),
        };
        (sig, def)
    }

    fn term(&mut self, t: parse::Term<&'s str>, tr: &Tr) -> Term {
        use parse::Term::*;
        match t {
            Id => Term::Id,
            Recurse => Term::Recurse,
            Arr(t) => Term::Arr(self.iterm(t.map_or_else(|| Call("!empty", Vec::new()), |t| *t))),
            Neg(t) => Term::Neg(self.iterm(*t)),
            Pipe(l, None, r) => Term::Pipe(self.iterm(*l), None, self.iterm_tr(*r, tr)),
            Pipe(l, Some(pat), r) => {
                let vars: Vec<_> = pat.vars().copied().collect();
                let r = self.with_vars(&vars, |c| c.iterm_tr(*r, tr));
                Term::Pipe(self.iterm(*l), Some(self.pattern(pat)), r)
            }
            Label(x, t) => Term::Label(self.with_label(x, |c| c.iterm(*t))),
            Break(x) => self.break_(x),
            IfThenElse(if_thens, else_) => {
                let else_ = else_.map_or(Term::Id, |else_| self.term(*else_, tr));
                if_thens.into_iter().rev().fold(else_, |acc, (if_, then_)| {
                    Term::Ite(
                        self.iterm(if_),
                        self.iterm_tr(then_, tr),
                        self.lut.insert_term(acc),
                    )
                })
            }
            Var(x) => self.var(x),
            Call(name, args) => {
                let args: Box<[_]> = args.into_iter().map(|t| self.iterm(t)).collect();
                if let Some((module, name)) = name.split_once("::") {
                    self.call_mod(module, name, &args)
                } else {
                    self.call(name, &args, tr)
                }
            }
            Def(defs, t) => {
                defs.iter().for_each(|def| self.def_pre(def));
                let t = self.term(*t, tr);
                // we have to process the siblings in *reverse*, because that way,
                // all potential call-sites of a sibling are processed before the sibling itself
                // (because a sibling can only be called by functions *after* it, not before it)
                // this is important to establish which functions can be called
                // tail-recursively from a sibling
                defs.into_iter().rev().for_each(|def| {
                    self.def_post(def);
                });
                t
            }
            Num(n) => n.parse().map_or_else(|_| Term::Num(n.into()), Term::Int),
            // map `try f catch g` to `label $x | try f catch (g, break $x)`
            // and `try f` or `f?` to `label $x | try f catch (   break $x)`
            TryCatch(try_, catch) => {
                let catch = match catch {
                    None => Break(""),
                    Some(c) => BinOp(c, parse::BinaryOp::Comma, Break("").into()),
                };
                let tc = self.with_label("", |c| Term::TryCatch(c.iterm(*try_), c.iterm(catch)));
                Term::Label(self.lut.insert_term(tc))
            }
            Fold(name, xs, pat, args) => {
                use self::Fold::{Foreach, Reduce};
                let arity = args.len();
                let mut args = args.into_iter();
                let (init, update) = match (args.next(), args.next()) {
                    (Some(init), Some(update)) => (init, update),
                    _ => return self.fail(name, Undefined::Filter(arity)),
                };
                let vars: Vec<_> = pat.vars().copied().collect();
                let xs = self.iterm(*xs);
                let pat = self.pattern(pat);
                let init = self.iterm(init);
                let update = self.with_vars(&vars, |c| c.iterm(update));

                match (name, args.next(), args.next()) {
                    ("reduce", None, None) => Term::Fold(xs, pat, init, update, Reduce),
                    ("foreach", proj, None) => {
                        let proj = proj.map(|p| self.with_vars(&vars, |c| c.iterm_tr(p, tr)));
                        Term::Fold(xs, pat, init, update, Foreach(proj))
                    }
                    _ => self.fail(name, Undefined::Filter(arity)),
                }
            }
            BinOp(l, op, r) => {
                use parse::BinaryOp::*;
                let (l, r) = match op {
                    Comma => (self.iterm_tr(*l, tr), self.iterm_tr(*r, tr)),
                    Alt => (self.iterm(*l), self.iterm_tr(*r, tr)),
                    _ => (self.iterm(*l), self.iterm(*r)),
                };
                match op {
                    Comma => Term::Comma(l, r),
                    Math(op) => Term::Math(l, op, r),
                    Assign => Term::Assign(l, r),
                    Update => Term::Update(l, r),
                    UpdateMath(op) => Term::UpdateMath(l, op, r),
                    Cmp(op) => Term::Cmp(l, op, r),
                    Or => Term::Logic(l, true, r),
                    And => Term::Logic(l, false, r),
                    Alt => Term::Alt(l, r),
                    UpdateAlt => Term::UpdateAlt(l, r),
                }
            }
            Path(t, path) => {
                let t = self.iterm(*t);
                let path = path.0.into_iter();
                let path = path.map(|(p, opt)| (p.map(|f| self.iterm(f)), opt));
                Term::Path(t, crate::path::Path(path.collect()))
            }
            Str(fmt, parts) => {
                use lex::StrPart;
                let fmt = match fmt {
                    Some(fmt) => self.iterm(Call(fmt, Vec::new())),
                    None => self.lut.insert_term(Term::ToString),
                };
                let parts = parts.into_iter().map(|part| match part {
                    StrPart::Str(s) => Term::Str(s.into()),
                    StrPart::Char(c) => Term::Str(c.into()),
                    StrPart::Term(f) => Term::Pipe(self.iterm(f), None, fmt),
                });
                let parts = parts.collect();
                self.sum_or(|| Term::Str(String::new()), parts)
            }
            Obj(o) => {
                let kvs = o.into_iter().map(|(k, v)| self.obj_entry(k, v)).collect();
                self.sum_or(|| Term::ObjEmpty, kvs)
            }
        }
    }

    /// Compile a term in a context that does *not* permit tail-recursion.
    ///
    /// One example of such a term is `t` in `1 + t` or `t | .+1`.
    fn iterm(&mut self, t: parse::Term<&'s str>) -> TermId {
        // if anything in our term calls an ancestor of our term, then we know that
        // this ancestor cannot be tail-recursive!
        self.iterm_tr(t, &Tr::new())
    }

    fn iterm_tr(&mut self, t: parse::Term<&'s str>, tr: &Tr) -> TermId {
        let t = self.term(t, tr);
        self.lut.insert_term(t)
    }

    fn pattern(&mut self, p: parse::Pattern<&'s str>) -> Pattern<TermId> {
        match p {
            parse::Pattern::Var(_) => Pattern::Var,
            parse::Pattern::Arr(a) => {
                let iter = a
                    .into_iter()
                    .enumerate()
                    .map(|(i, p)| (self.lut.insert_term(Term::Int(i as isize)), self.pattern(p)));
                Pattern::Idx(iter.collect())
            }
            parse::Pattern::Obj(o) => {
                let iter = o.into_iter().map(|(k, p)| (self.iterm(k), self.pattern(p)));
                Pattern::Idx(iter.collect())
            }
        }
    }

    fn fail(&mut self, name: &'s str, undef: Undefined) -> Term {
        self.errs.push((name, undef));
        Term::default()
    }

    fn call_mod_id(&self, mid: ModId, name: &'s str, args: &[TermId]) -> Option<Term> {
        let mut sig_defs = self.mod_map[mid].iter().rev();
        let (sig, def) = sig_defs.find(|(sig, _)| sig.matches(name, args))?;
        Some(def.call(binds(&sig.args, args), self.locals.vars.total))
    }

    /// Resolve call to `mod::filter(a1, ..., an)`.
    fn call_mod(&mut self, module: &'s str, name: &'s str, args: &[TermId]) -> Term {
        let mut imported_mods = self.imported_mods.iter().rev();
        let mid = match imported_mods.find(|(_mid, module_)| module == *module_) {
            Some((mid, _module)) => mid,
            None => return self.fail(module, Undefined::Mod),
        };
        if let Some(call) = self.call_mod_id(*mid, name, args) {
            return call;
        }
        self.fail(name, Undefined::Filter(args.len()))
    }

    /// Resolve call to `filter(a1, ..., an)`.
    fn call(&mut self, name: &'s str, args: &[TermId], tr: &Tr) -> Term {
        if let Some(t) = self.locals.call(name, args, tr) {
            return t;
        }
        for mid in self.included_mods.iter().rev() {
            if let Some(call) = self.call_mod_id(*mid, name, args) {
                return call;
            }
        }
        for (nid, (sig, _f)) in self.lut.funs.iter().enumerate() {
            if sig.matches(name, args) {
                return Term::Native(nid, binds(&sig.args, args));
            }
        }

        self.fail(name, Undefined::Filter(args.len()))
    }

    fn var(&mut self, x: &'s str) -> Term {
        let mut i = self.locals.vars.total;

        if let Some(v) = self.locals.vars.bound.get_last(&Bind::Var(x)) {
            return Term::Var(i - v);
        }
        for (x_, mid) in self.imported_vars.iter().rev() {
            if x == *x_ && *mid == self.mod_map.len() {
                return Term::Var(i);
            } else {
                i += 1;
            }
        }
        for x_ in self.global_vars.iter().rev() {
            if x == *x_ {
                return Term::Var(i);
            } else {
                i += 1;
            }
        }
        self.fail(x, Undefined::Var)
    }

    fn break_(&mut self, x: &'s str) -> Term {
        if let Some(l) = self.locals.vars.bound.get_last(&Bind::Label(x)) {
            return Term::Var(self.locals.vars.total - l);
        }
        self.fail(x, Undefined::Label)
    }

    fn obj_entry(&mut self, k: parse::Term<&'s str>, v: Option<parse::Term<&'s str>>) -> Term {
        let (k, v) = match (k, v) {
            (parse::Term::Var(x), None) => (
                self.lut.insert_term(Term::Str(x[1..].into())),
                self.iterm(parse::Term::Var(x)),
            ),
            (k, None) => {
                use crate::path::{Part, Path};
                let k = self.iterm(k);
                let path = Path::from(Part::Index(k));
                let path = Term::Path(self.lut.insert_term(Term::Id), path);
                (k, self.lut.insert_term(path))
            }
            (k, Some(v)) => (self.iterm(k), self.iterm(v)),
        };
        Term::ObjSingle(k, v)
    }

    fn sum_or(&mut self, f: impl FnOnce() -> Term, terms: Vec<Term>) -> Term {
        use ops::Math::Add;
        let mut iter = terms.into_iter().rev();
        let last = iter.next().unwrap_or_else(f);
        iter.fold(last, |acc, x| {
            Term::Math(self.lut.insert_term(x), Add, self.lut.insert_term(acc))
        })
    }
}
