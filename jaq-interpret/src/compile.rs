//! Program compilation.

use crate::Bind;
use alloc::{boxed::Box, string::String, vec::Vec};
use jaq_syn::{load, parse, MathOp, OrdOp};

type NativeId = usize;
type ModId = usize;
type VarId = usize;
type VarSkip = usize;
type LabelSkip = usize;
type Arity = usize;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct TermId(pub(crate) usize);

/// Function from a value to a stream of value results.
#[derive(Debug, Clone)]
pub struct Filter<F = ()>(pub(crate) TermId, pub(crate) Lut<F>);

/// Look-up table for indices stored in ASTs.
#[derive(Clone, Debug, Default)]
pub(crate) struct Lut<F> {
    /// `terms[tid]` yields the term corresponding to the term ID `tid`
    pub(crate) terms: Vec<Term>,
    pub(crate) funs: Vec<F>,
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

    pub fn with_funs<F2>(self, funs: impl IntoIterator<Item = F2>) -> Lut<F2> {
        Lut {
            funs: funs.into_iter().collect(),
            terms: self.terms,
        }
    }

    fn insert_term(&mut self, t: Term) -> TermId {
        let tid = self.terms.len();
        self.terms.push(t);
        TermId(tid)
    }
}

impl<F> Filter<F> {
    /// Provide functions for a compiled filter.
    pub fn with_funs<F2>(self, funs: impl IntoIterator<Item = F2>) -> Filter<F2> {
        Filter(self.0, self.1.with_funs(funs))
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

#[derive(Clone, Debug, Default)]
pub(crate) enum Term<T = TermId> {
    /// Identity (`.`)
    #[default]
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
    Native(NativeId, Box<[T]>),

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
    UpdateMath(T, MathOp, T),
    /// Alternation update-assignment (`f //= g`)
    UpdateAlt(T, T),
    /// Logical operation (`f and g`, `f or g`)
    Logic(T, bool, T),
    /// Arithmetical operation (`f + g`, `f - g`, `f * g`, `f / g`, `f % g`)
    Math(T, MathOp, T),
    /// Comparison operation (`f < g`, `f <= g`, `f > g`, `f >= g`, `f == g`, `f != g`)
    Ord(T, OrdOp, T),
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
    Fold(FoldType, T, T, T),

    Path(T, crate::path::Path<T>),
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
pub type Errors<S> = Vec<(load::File<S>, Vec<Error<S>>)>;

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
#[derive(Default)]
pub struct Compiler<S> {
    lut: Lut<FunSig<S>>,

    /// `mod_map[mid]` yields all top-level definitions contained inside a module with ID `mid`
    mod_map: Vec<Vec<Sig<S, Bind<()>>>>,

    imported_mods: Vec<(ModId, S)>,
    included_mods: Vec<ModId>,

    global_vars: Vec<S>,
    imported_vars: Vec<(S, ModId)>,
    local: Vec<Local<S>>,

    errs: Vec<Error<S>>,
}

type FunSig<S, A = Arity> = (S, A);

#[derive(Clone, Debug)]
struct Sig<S, A> {
    name: S,
    // TODO: we could analyse for each argument whether it is TR, and
    // use this when converting args at callsite
    args: Vec<A>,
    id: TermId,
    /// true if there is at least one tail-recursive call to this filter from within itself
    tailrec: bool,
}

impl<S, A> Sig<S, A> {
    fn map_args<A2>(self, f: impl FnMut(A) -> A2) -> Sig<S, A2> {
        Sig {
            name: self.name,
            args: self.args.into_iter().map(f).collect(),
            id: self.id,
            tailrec: self.tailrec,
        }
    }
}

impl<S: Eq, A> Sig<S, A> {
    fn matches(&self, name: S, args: &[TermId]) -> bool {
        name == self.name && args.len() == self.args.len()
    }
}

impl<S> Sig<S, Bind<()>> {
    fn call(&self, args: &[TermId], vars: usize) -> Term {
        let call = self.tailrec.then_some(Tailrec::Catch);
        let args = self.args.iter().zip(args);
        let args = args.map(|(bind, id)| bind.as_ref().map(|_| *id));
        Term::CallDef(self.id, args.collect(), vars, call)
    }
}

impl<'s> Sig<&'s str, &'s str> {
    fn call_parent(&mut self, args: &[TermId], vars: usize, tailrec: bool) -> Term {
        self.tailrec = self.tailrec || tailrec;
        let call = tailrec.then_some(Tailrec::Throw);
        let args = self.args.iter().zip(args).map(|(x, id)| bind(x, *id));
        Term::CallDef(self.id, args.collect(), vars, call)
    }
}

#[derive(Clone, Debug)]
enum Local<S> {
    Var(S),
    Label(S),
    Parent(Sig<S, S>),
    Sibling(Sig<S, Bind<()>>),
    TailrecObstacle,
}

impl<'s> Compiler<&'s str> {
    /// Assume the existence of functions with given signatures.
    ///
    /// For execution, the corresponding functions have to be provided to the filter
    /// via [`Filter::with_funs`].
    pub fn with_funs(self, funs: impl IntoIterator<Item = FunSig<&'s str>>) -> Self {
        let lut = self.lut.with_funs(funs);
        Self { lut, ..self }
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
    pub fn compile(mut self, mods: load::Modules<&'s str>) -> Result<Filter, Errors<&'s str>> {
        self.imported_vars = mods
            .iter()
            .enumerate()
            .flat_map(|(mid, (_file, m))| m.vars.iter().map(move |(_path, x)| (*x, mid)))
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
            let main = self.mod_map.last().unwrap().last().unwrap();
            assert_eq!(main.name, "main");
            assert!(main.args.is_empty());
            assert!(!main.tailrec);
            Ok(Filter(main.id, self.lut.with_funs([])))
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

    fn module(&mut self, m: jaq_syn::load::Module<&'s str>) {
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
            Local::Sibling(sig) => sig,
            _ => panic!(),
        });
        self.mod_map.push(defs.collect());
    }

    fn def(&mut self, d: parse::Def<&'s str, parse::Term<&'s str>>) {
        let tid = self.lut.insert_term(Term::Id);

        let sig = Sig {
            name: d.name,
            args: d.args,
            id: tid,
            tailrec: false,
        };
        self.local.push(Local::Parent(sig));

        self.lut.terms[tid.0] = self.term(d.body);

        let sig = match self.local.pop() {
            Some(Local::Parent(sig)) => sig,
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
        if sig.tailrec {
            for term in &mut self.lut.terms[tid.0..] {
                if let Term::CallDef(id, .., tailrec @ None) = term {
                    *tailrec = (*id == tid).then_some(Tailrec::Catch);
                }
            }
        }

        // turn the parent into a sibling
        self.local
            .push(Local::Sibling(sig.map_args(|a| bind(a, ()))));
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
                (0..defs_len)
                    .for_each(|_| assert!(matches!(self.local.pop(), Some(Local::Sibling(_)))));
                t
            }
            Num(n) => n.parse().map_or_else(|_| Term::Num(n.into()), Term::Int),
            TryCatch(t, c) => Term::TryCatch(
                self.iterm(*t),
                self.iterm_tr(c.map_or_else(|| Call("!empty", Vec::new()), |c| *c)),
            ),
            Fold(name, xs, x, args) => {
                let arity = args.len();
                let fold = match name {
                    "reduce" => FoldType::Reduce,
                    "foreach" => FoldType::Foreach,
                    "for" => FoldType::For,
                    name => return self.fail(name, Undefined::Filter(arity)),
                };
                let mut args = args.into_iter();
                let (init, update) = match (args.next(), args.next(), args.next()) {
                    (Some(init), Some(update), None) => (init, update),
                    _ => return self.fail(name, Undefined::Filter(arity)),
                };
                let xs = self.iterm(*xs);
                let init = self.iterm(init);
                let update = self.with(Local::Var(x), |c| c.iterm(update));

                Term::Fold(fold, xs, init, update)
            }
            BinOp(l, op, r) => {
                use jaq_syn::parse::BinaryOp::*;
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
                    Ord(op) => Term::Ord(l, op, r),
                    Or => Term::Logic(l, true, r),
                    And => Term::Logic(l, false, r),
                    Alt => Term::Alt(l, r),
                    UpdateAlt => Term::UpdateAlt(l, r),
                }
            }
            Path(t, path) => {
                use crate::path::Part;
                use jaq_syn::path::Part::{Index, Range};
                let t = self.iterm(*t);
                let path = path.into_iter().map(|(p, opt)| match p {
                    Index(i) => (Part::Index(self.iterm(i)), opt),
                    Range(lower, upper) => {
                        let lower = lower.map(|f| self.iterm(f));
                        let upper = upper.map(|f| self.iterm(f));
                        (Part::Range(lower, upper), opt)
                    }
                });
                Term::Path(t, crate::path::Path(path.collect()))
            }
            Str(fmt, parts) => {
                use jaq_syn::lex::StrPart;
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
            Local::Parent(sig) => sig.args.len(),
        });
        let vars = vars.sum();
        let mut imported_mods = self.imported_mods.iter().rev();
        let mid = match imported_mods.find(|(_mid, module_)| module == *module_) {
            Some((mid, _module)) => mid,
            None => return self.fail(module, Undefined::Mod),
        };
        let mut defs = self.mod_map[*mid].iter().rev();
        let call = defs.find_map(|sig| sig.matches(name, args).then(|| sig.call(args, vars)));
        call.unwrap_or_else(|| self.fail(name, Undefined::Filter(args.len())))
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
                Local::Sibling(sig) => {
                    if sig.matches(name, &args) {
                        return sig.call(&args, i);
                    }
                }
                Local::Parent(sig) => {
                    for arg in sig.args.iter().rev() {
                        if *arg == name && args.is_empty() {
                            return Term::Var(i, labels);
                        } else {
                            i += 1;
                        }
                    }
                    if sig.matches(name, &args) {
                        return sig.call_parent(&args, i, tailrec);
                    }
                }
                Local::TailrecObstacle => tailrec = false,
            }
        }
        for mid in self.included_mods.iter().rev() {
            for sig in self.mod_map[*mid].iter().rev() {
                if sig.matches(name, &args) {
                    return sig.call(&args, i);
                }
            }
        }

        let mut funs = self.lut.funs.iter();
        if let Some(nid) = funs.position(|(name_, arity)| name == *name_ && args.len() == *arity) {
            return Term::Native(nid, args);
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
                Local::Parent(sig) => {
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
        use MathOp::Add;
        let mut iter = terms.into_iter().rev();
        let last = iter.next().unwrap_or_else(f);
        iter.fold(last, |acc, x| {
            Term::Math(self.lut.insert_term(x), Add, self.lut.insert_term(acc))
        })
    }
}
