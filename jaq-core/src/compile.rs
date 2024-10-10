//! Program compilation.

use crate::load::{self, lex, parse};
use crate::{ops, Bind, Filter};
use alloc::{boxed::Box, string::String, vec::Vec};

type NativeId = usize;
type ModId = usize;
type VarId = usize;
type VarSkip = usize;
type LabelSkip = usize;
type Arity = usize;

/// Index of a term in the look-up table.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

fn bind<T>(s: &str, x: T) -> Bind<T> {
    if s.starts_with('$') {
        Bind::Var(x)
    } else {
        Bind::Fun(x)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Term<T = TermId> {
    /// Identity (`.`)
    Id,
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

    /// Bound variable (`$x`) or filter argument (`a`)
    Var(VarId, LabelSkip),
    /// Call to a filter (`filter`, `filter(…)`)
    CallDef(TermId, Box<[Bind<T>]>, VarSkip, Option<Tailrec>),
    Native(NativeId, Box<[Bind<T>]>),

    Label(T),
    Break(usize),

    /// Negation operation (`-f`)
    Neg(T),
    /// Variable binding (`f as $x | g`) if identifier (`x`) is given, otherwise
    /// application (`f | g`)
    Pipe(T, bool, T),
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
    /// `reduce`, `for`, and `foreach`
    ///
    /// The first field indicates whether to yield intermediate results
    /// (`false` for `reduce` and `true` for `foreach`).
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
    /// and `for xs as $x (init; f)` evaluates to
    ///
    /// ~~~ text
    /// init
    /// | ., (x0 as $x | f
    /// | ...
    /// | ., (xn as $x | f)...)
    /// ~~~
    Fold(FoldType, T, T, T, Option<T>),

    Path(T, crate::path::Path<T>),
}

impl<T> Default for Term<T> {
    fn default() -> Self {
        Self::Id
    }
}

#[derive(Clone, Debug)]
pub(crate) enum FoldType {
    Reduce,
    Foreach,
    For,
}

/// Compilation error.
pub type Error<S> = (S, Undefined);

/// Compilation errors.
pub type Errors<S> = load::Errors<S, Vec<Error<S>>>;

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
    /// filter
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
pub struct Compiler<S, F> {
    lut: Lut<(Sig<S>, F)>,

    /// `mod_map[mid]` yields all top-level definitions contained inside a module with ID `mid`
    mod_map: Vec<Vec<(Sig<S>, Def)>>,

    imported_mods: Vec<(ModId, S)>,
    included_mods: Vec<ModId>,

    global_vars: Vec<S>,
    imported_vars: Vec<(S, ModId)>,
    local: Vec<Local<S>>,

    errs: Vec<Error<S>>,
}

impl<S, F> Default for Compiler<S, F> {
    fn default() -> Self {
        Self {
            lut: Lut::default(),
            mod_map: Vec::new(),
            imported_mods: Vec::new(),
            included_mods: Vec::new(),
            global_vars: Vec::new(),
            imported_vars: Vec::new(),
            local: Vec::new(),
            errs: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct Sig<S, A = Bind> {
    name: S,
    // TODO: we could analyse for each argument whether it is TR, and
    // use this when converting args at callsite
    args: Box<[A]>,
}

#[derive(Clone, Debug)]
struct Def {
    id: TermId,
    /// true if there is at least one tail-recursive call to this filter from within itself
    tailrec: bool,
}

impl<S, A> Sig<S, A> {
    fn map_args<A2>(self, f: impl FnMut(A) -> A2) -> Sig<S, A2> {
        Sig {
            name: self.name,
            args: self.args.into_vec().into_iter().map(f).collect(),
        }
    }
}

impl<S: Eq, A> Sig<S, A> {
    fn matches(&self, name: S, args: &[TermId]) -> bool {
        name == self.name && args.len() == self.args.len()
    }
}

impl<S> Sig<S, Bind> {
    fn bind(&self, args: &[TermId]) -> Box<[Bind<TermId>]> {
        let args = self.args.iter().zip(args);
        args.map(|(bind, id)| bind.as_ref().map(|_| *id)).collect()
    }
}

impl Def {
    fn call(&self, args: Box<[Bind<TermId>]>, vars: usize) -> Term {
        let call = self.tailrec.then_some(Tailrec::Catch);
        Term::CallDef(self.id, args, vars, call)
    }
}

#[derive(Clone, Debug)]
enum Local<S> {
    Var(S),
    Label(S),
    Parent(Sig<S, S>, Def),
    Sibling(Sig<S>, Def),
    TailrecObstacle,
}

impl<'s, F> Compiler<&'s str, F> {
    /// Supply functions with given signatures.
    pub fn with_funs(mut self, funs: impl IntoIterator<Item = (&'s str, Box<[Bind]>, F)>) -> Self {
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
    pub fn compile(mut self, mods: load::Modules<&'s str>) -> Result<Filter<F>, Errors<&'s str>> {
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
        }

        if errs.is_empty() {
            // the main filter corresponds to the last definition of the last module
            let (main_sig, main_def) = self.mod_map.last().unwrap().last().unwrap();
            assert!(main_sig.matches("main", &[]));
            assert!(!main_def.tailrec);
            Ok(Filter(main_def.id, self.lut.map_funs(|(_sig, f)| f)))
        } else {
            Err(errs)
        }
    }

    fn with<T>(&mut self, local: Local<&'s str>, f: impl FnOnce(&mut Self) -> T) -> T {
        self.local.push(local.clone());
        let y = f(self);
        self.local.pop();
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

        m.body.into_iter().for_each(|def| self.def(def));
        let defs = self.local.drain(..).map(|l| match l {
            Local::Sibling(sig, def) => (sig, def),
            _ => panic!(),
        });
        self.mod_map.push(defs.collect());
    }

    fn def(&mut self, d: parse::Def<&'s str, parse::Term<&'s str>>) {
        let tid = self.lut.insert_term(Term::Id);

        let sig = Sig {
            name: d.name,
            args: d.args.into(),
        };
        let def = Def {
            id: tid,
            tailrec: false,
        };
        self.local.push(Local::Parent(sig, def));

        self.lut.terms[tid.0] = self.term(d.body);

        let (sig, def) = match self.local.pop() {
            Some(Local::Parent(sig, def)) => (sig, def),
            _ => panic!(),
        };

        // By default, we assume that a definition `f` is not tail-recursive.
        // However, once we find that `f` is tail-recursive after all,
        // we have to adapt all calls to `f` that are recursive, but not tail-recursive.
        // For example, in `def f: 1 + f, f`, when going from left to right,
        // when handling `1 + f`, we still assume that `f` is not tail-recursive,
        // but once we treat the second call to `f` (which is tail-recursive),
        // we know that `f` is tail-recursive.
        // So we have to adapt the call to `f` in `1 + f` *afterwards*.
        // That's what we do here.
        if def.tailrec {
            for term in &mut self.lut.terms[tid.0..] {
                if let Term::CallDef(id, .., tailrec @ None) = term {
                    *tailrec = (*id == tid).then_some(Tailrec::Catch);
                }
            }
        }

        // turn the parent into a sibling
        self.local
            .push(Local::Sibling(sig.map_args(|a| bind(a, ())), def));
    }

    fn term(&mut self, t: parse::Term<&'s str>) -> Term {
        use parse::Term::*;
        match t {
            Id => Term::Id,
            Recurse => self.term(Call("!recurse", Vec::new())),
            Arr(t) => Term::Arr(self.iterm(t.map_or_else(|| Call("!empty", Vec::new()), |t| *t))),
            Neg(t) => Term::Neg(self.iterm(*t)),
            Pipe(l, Some(x), r) => Term::Pipe(
                self.iterm(*l),
                true,
                self.with(Local::Var(x), |c| c.iterm_tr(*r)),
            ),
            Pipe(l, None, r) => Term::Pipe(self.iterm(*l), false, self.iterm_tr(*r)),
            Label(x, t) => Term::Label(self.with(Local::Label(x), |c| c.iterm(*t))),
            Break(x) => self.break_(x),
            IfThenElse(if_thens, else_) => {
                let else_ = else_.map_or(Term::Id, |else_| self.term(*else_));
                if_thens.into_iter().rev().fold(else_, |acc, (if_, then_)| {
                    Term::Ite(
                        self.iterm(if_),
                        self.iterm_tr(then_),
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
                    self.call(name, args)
                }
            }
            Def(defs, t) => {
                let defs_len = defs.len();
                defs.into_iter().for_each(|d| self.def(d));
                let t = self.term(*t);
                assert!((0..defs_len).all(|_| matches!(self.local.pop(), Some(Local::Sibling(..)))));
                t
            }
            Num(n) => n.parse().map_or_else(|_| Term::Num(n.into()), Term::Int),
            // map `try f catch g` to `label $x | try f catch (g, break $x)`
            // and `try f` or `f?` to `label $x | try f catch (   break $x)`
            TryCatch(t, c) => {
                let (label, break_) = (Local::Label(""), Break(""));
                let catch = match c {
                    None => break_,
                    Some(c) => BinOp(c, parse::BinaryOp::Comma, break_.into()),
                };
                let tc = self.with(label, |c| Term::TryCatch(c.iterm(*t), c.iterm(catch)));
                Term::Label(self.lut.insert_term(tc))
            }
            Fold(name, xs, x, args) => {
                let arity = args.len();
                let fold = match name {
                    "reduce" => FoldType::Reduce,
                    "foreach" => FoldType::Foreach,
                    "for" => FoldType::For,
                    name => return self.fail(name, Undefined::Filter(arity)),
                };
                let mut args = args.into_iter();
                let (init, update) = match (args.next(), args.next()) {
                    (Some(init), Some(update)) => (init, update),
                    _ => return self.fail(name, Undefined::Filter(arity)),
                };
                let project = match (&fold, args.next(), args.next()) {
                    (FoldType::Reduce, None, None) => None,
                    (FoldType::Foreach | FoldType::For, project, None) => project,
                    _ => return self.fail(name, Undefined::Filter(arity)),
                };
                let xs = self.iterm(*xs);
                let init = self.iterm(init);
                let update = self.with(Local::Var(x), |c| c.iterm(update));
                let project = project.map(|p| self.with(Local::Var(x), |c| c.iterm(p)));

                Term::Fold(fold, xs, init, update, project)
            }
            BinOp(l, op, r) => {
                use parse::BinaryOp::*;
                let (l, r) = match op {
                    Comma => (self.iterm_tr(*l), self.iterm_tr(*r)),
                    Alt => (self.iterm(*l), self.iterm_tr(*r)),
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
                    StrPart::Term(f) => Term::Pipe(self.iterm(f), false, fmt),
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

    fn iterm(&mut self, t: parse::Term<&'s str>) -> TermId {
        self.with(Local::TailrecObstacle, |c| c.iterm_tr(t))
    }

    fn iterm_tr(&mut self, t: parse::Term<&'s str>) -> TermId {
        let t = self.term(t);
        self.lut.insert_term(t)
    }

    fn fail(&mut self, name: &'s str, undef: Undefined) -> Term {
        self.errs.push((name, undef));
        Term::default()
    }

    /// Resolve call to `mod::filter(a1, ..., an)`.
    fn call_mod(&mut self, module: &'s str, name: &'s str, args: &[TermId]) -> Term {
        let vars = self.local.iter().map(|l| match l {
            Local::Var(_) => 1,
            Local::Label(_) | Local::Sibling(..) | Local::TailrecObstacle => 0,
            Local::Parent(sig, _def) => sig.args.len(),
        });
        let vars = vars.sum();
        let mut imported_mods = self.imported_mods.iter().rev();
        let mid = match imported_mods.find(|(_mid, module_)| module == *module_) {
            Some((mid, _module)) => mid,
            None => return self.fail(module, Undefined::Mod),
        };
        for (sig, def) in self.mod_map[*mid].iter().rev() {
            if sig.matches(name, args) {
                return def.call(sig.bind(args), vars);
            }
        }
        self.fail(name, Undefined::Filter(args.len()))
    }

    /// Resolve call to `filter(a1, ..., an)`.
    fn call(&mut self, name: &'s str, args: Box<[TermId]>) -> Term {
        let mut tailrec = true;
        let mut i = 0;
        let mut labels = 0;
        for l in self.local.iter_mut().rev() {
            match l {
                Local::Var(_) => i += 1,
                Local::Label(_) => labels += 1,
                Local::Sibling(sig, def) => {
                    if sig.matches(name, &args) {
                        return def.call(sig.bind(&args), i);
                    }
                }
                Local::Parent(sig, def) => {
                    for arg in sig.args.iter().rev() {
                        if *arg == name && args.is_empty() {
                            return Term::Var(i, labels);
                        } else {
                            i += 1;
                        }
                    }
                    if sig.matches(name, &args) {
                        def.tailrec = def.tailrec || tailrec;
                        let call = tailrec.then_some(Tailrec::Throw);
                        let args = sig.args.iter().zip(args.to_vec());
                        let args = args.map(|(x, id)| bind(x, id));
                        return Term::CallDef(def.id, args.collect(), i, call);
                    }
                }
                Local::TailrecObstacle => tailrec = false,
            }
        }
        for mid in self.included_mods.iter().rev() {
            for (sig, def) in self.mod_map[*mid].iter().rev() {
                if sig.matches(name, &args) {
                    return def.call(sig.bind(&args), i);
                }
            }
        }

        for (nid, (sig, _f)) in self.lut.funs.iter().enumerate() {
            if sig.matches(name, &args) {
                return Term::Native(nid, sig.bind(&args));
            }
        }

        self.fail(name, Undefined::Filter(args.len()))
    }

    fn var(&mut self, x: &'s str) -> Term {
        let mut i = 0;
        for l in self.local.iter().rev() {
            match l {
                Local::Sibling(..) | Local::Label(_) | Local::TailrecObstacle => (),
                Local::Var(x_) if x == *x_ => return Term::Var(i, 0),
                Local::Var(_) => i += 1,
                Local::Parent(sig, _def) => {
                    for arg in sig.args.iter().rev() {
                        if *arg == x {
                            return Term::Var(i, 0);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
        }
        for (x_, mid) in self.imported_vars.iter().rev() {
            if x == *x_ && *mid == self.mod_map.len() {
                return Term::Var(i, 0);
            } else {
                i += 1;
            }
        }
        for x_ in self.global_vars.iter().rev() {
            if x == *x_ {
                return Term::Var(i, 0);
            } else {
                i += 1;
            }
        }
        self.fail(x, Undefined::Var)
    }

    fn break_(&mut self, x: &'s str) -> Term {
        let mut labels = 0;
        for l in self.local.iter().rev() {
            match l {
                Local::Label(x_) if x == *x_ => return Term::Break(labels),
                Local::Label(_) => labels += 1,
                _ => (),
            }
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
