use alloc::{boxed::Box, string::String, vec::Vec};
use jaq_syn::{load, parse, MathOp, OrdOp};

type NativeId = usize;
type TermId = usize;
type ModId = usize;
type VarId = usize;
type VarSkip = usize;
type LabelSkip = usize;

/// Function from a value to a stream of value results.
#[derive(Debug, Clone)]
pub struct Filter(TermId, Box<[Term]>);

#[derive(Clone, Debug)]
enum Tailrec {
    Throw,
    Catch,
}

#[derive(Copy, Clone, Debug)]
enum Bind {
    Var,
    Filter,
}

impl From<&str> for Bind {
    fn from(s: &str) -> Self {
        if s.starts_with('$') {
            Self::Var
        } else {
            Self::Filter
        }
    }
}

#[derive(Clone, Debug, Default)]
enum Term<T = TermId> {
    #[default]
    Id,
    ToString,

    Int(isize),
    Num(String),
    Str(String),
    Arr(T),
    ObjEmpty,
    ObjSingle(T, T),

    Var(VarId),
    CallArg(VarId, LabelSkip),
    CallDef(TermId, Vec<(Bind, T)>, VarSkip, Option<Tailrec>),
    Native(usize, Box<[T]>),

    TryCatch(T, T),
    Neg(T),
    Label(T),
    Ite(T, T, T),
    Fold(FoldType, T, T, T),
    Break(usize),

    Pipe(T, bool, T),
    Comma(T, T),
    Assign(T, T),
    Update(T, T),
    UpdateMath(T, MathOp, T),
    Logic(T, bool, T),
    Math(T, MathOp, T),
    Ord(T, OrdOp, T),
    Alt(T, T),
    UpdateAlt(T, T),
    Path(T, crate::path::Path<T>),
}

#[derive(Clone, Debug)]
enum FoldType {
    Reduce,
    Foreach,
    For,
}

struct Error<S>(S, Undefined);

type Errors<S> = Vec<(load::File<S>, Vec<Error<S>>)>;

enum Undefined {
    Mod,
    Var,
    Label,
    Filter(usize),
}

#[derive(Default)]
struct Compiler<S> {
    /// `term_map[tid]` yields the term corresponding to the term ID `tid`
    term_map: Vec<Term>,

    natives_map: Vec<(S, usize)>,

    /// `mod_map[mid]` yields all top-level definitions contained inside a module with ID `mid`
    mod_map: Vec<Vec<Sig<S, Bind>>>,

    imported_mods: Vec<(ModId, S)>,
    included_mods: Vec<ModId>,

    global_vars: Vec<S>,
    imported_vars: Vec<(S, ModId)>,
    local: Vec<Local<S>>,

    errs: Vec<Error<S>>,
}

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

impl<S> Sig<S, Bind> {
    fn call(&self, args: &[TermId], vars: usize) -> Term {
        let call = self.tailrec.then_some(Tailrec::Catch);
        let args = args.iter().copied();
        let args = self.args.iter().copied().zip(args);
        Term::CallDef(self.id, args.collect(), vars, call)
    }
}

impl<'s> Sig<&'s str, &'s str> {
    fn call_parent(&mut self, args: &[TermId], vars: usize, tailrec: bool) -> Term {
        self.tailrec = self.tailrec || tailrec;
        let call = tailrec.then_some(Tailrec::Throw);
        let args = args.iter().copied();
        let args = self.args.iter().copied().map(Bind::from).zip(args);
        Term::CallDef(self.id, args.collect(), vars, call)
    }
}

#[derive(Clone, Debug)]
enum Local<S> {
    Var(S),
    Label(S),
    Parent(Sig<S, S>),
    Sibling(Sig<S, Bind>),
    TailrecObstacle,
}

impl<'s> Compiler<&'s str> {
    pub fn with_natives(self, natives: impl IntoIterator<Item = (&'s str, usize)>) -> Self {
        Self {
            natives_map: natives.into_iter().collect(),
            ..self
        }
    }

    pub fn with_global_vars(self, global_vars: impl IntoIterator<Item = &'s str>) -> Self {
        Self {
            global_vars: global_vars.into_iter().collect(),
            ..self
        }
    }

    pub fn compile<V>(mut self, mods: load::Modules<&'s str>) -> Result<Filter, Errors<&'s str>> {
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
            Ok(Filter(main.id, self.term_map.into()))
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

    fn module(&mut self, m: jaq_syn::load::Module<&'s str, parse::Defs<&'s str>>) {
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
        let tid = self.insert_term(Term::Id);

        let arity = d.args.len();
        let sig = Sig {
            name: d.name,
            args: d.args,
            id: tid,
            tailrec: false,
        };
        self.local.push(Local::Parent(sig));

        self.term_map[tid] = self.term(d.body);

        // turn the parent into a sibling
        match self.local.pop() {
            Some(Local::Parent(sig)) => {
                self.local.push(Local::Sibling(sig.map_args(Bind::from)));
            }
            _ => panic!(),
        }
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
            Label(x, t) => Term::Label(self.with(Local::Label(x), |c| c.iterm_tr(*t))),
            Break(x) => self.break_(x),
            IfThenElse(if_thens, else_) => {
                let else_ = else_.map_or(Term::Id, |else_| self.term(*else_));
                if_thens.into_iter().rev().fold(else_, |acc, (if_, then_)| {
                    Term::Ite(self.iterm(if_), self.iterm_tr(then_), self.insert_term(acc))
                })
            }
            Var(x) => self.var(x),
            Call(name, args) => {
                let args: Vec<_> = args.into_iter().map(|t| self.iterm(t)).collect();
                if let Some((module, name)) = name.split_once("::") {
                    self.call_mod(module, name, args)
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
                let l = self.iterm(*l);
                let r = self.iterm(*r);
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
                use crate::path::{Part, Path};
                use jaq_syn::path::Part::*;
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
                    None => self.insert_term(Term::ToString),
                };
                let parts = parts.into_iter().map(|part| match part {
                    StrPart::Str(s) => Term::Str(s.into()),
                    StrPart::Char(c) => Term::Str(c.into()),
                    StrPart::Term(f) => Term::Pipe(self.iterm(f), false, fmt),
                });
                let parts = parts.collect();
                self.sum_or(|| Term::Str("".into()), parts)
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
        self.insert_term(t)
    }

    fn insert_term(&mut self, t: Term) -> TermId {
        let tid = self.term_map.len();
        self.term_map.push(t);
        tid
    }

    fn fail(&mut self, name: &'s str, undef: Undefined) -> Term {
        self.errs.push(Error(name, undef));
        Term::default()
    }

    /// Resolve call to `mod::filter(a1, ..., an)`.
    fn call_mod(&mut self, module: &'s str, name: &'s str, args: Vec<TermId>) -> Term {
        let vars = self.local.iter().map(|l| match l {
            Local::Var(x) => 1,
            Local::Label(_) | Local::Sibling(..) | Local::TailrecObstacle => 0,
            Local::Parent(sig) => sig.args.len(),
        });
        let vars = vars.sum();
        let mut imported_mods = self.imported_mods.iter().rev();
        let mid = match imported_mods.find(|(mid, module_)| module == *module_) {
            Some((mid, _module)) => mid,
            None => return self.fail(module, Undefined::Mod),
        };
        let mut defs = self.mod_map[*mid].iter().rev();
        let call = defs.find_map(|sig| sig.matches(name, &args).then(|| sig.call(&args, vars)));
        call.unwrap_or_else(|| self.fail(name, Undefined::Filter(args.len())))
    }

    /// Resolve call to `filter(a1, ..., an)`.
    fn call(&mut self, name: &'s str, args: Vec<TermId>) -> Term {
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
                            return Term::CallArg(i, labels);
                        } else {
                            i += 1
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

        let mut natives = self.natives_map.iter();
        if let Some(nid) = natives.position(|(name_, arity)| name == *name_ && args.len() == *arity)
        {
            return Term::Native(nid, args.into());
        }

        self.fail(name, Undefined::Filter(args.len()))
    }

    fn var(&mut self, x: &'s str) -> Term {
        let mut i = 0;
        for l in self.local.iter().rev() {
            match l {
                Local::Sibling(..) | Local::Label(_) | Local::TailrecObstacle => (),
                Local::Var(x_) if x == *x_ => return Term::Var(i),
                Local::Var(_) => i += 1,
                Local::Parent(sig) => {
                    for arg in sig.args.iter().rev() {
                        if *arg == x {
                            return Term::Var(i);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
        }
        for (x_, mid) in self.imported_vars.iter().rev() {
            if x == *x_ && *mid == self.mod_map.len() {
                return Term::Var(i);
            } else {
                i += 1
            }
        }
        for x_ in self.global_vars.iter().rev() {
            if x == *x_ {
                return Term::Var(i);
            } else {
                i += 1
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
            (k @ parse::Term::Var(x), None) => {
                (self.insert_term(Term::Str(x[1..].into())), self.iterm(k))
            }
            (k, None) => {
                use crate::path::{Part, Path};
                let k = self.iterm(k);
                let path = Path::from(Part::Index(k));
                let path = Term::Path(self.insert_term(Term::Id), path);
                (k, self.insert_term(path))
            }
            (k, Some(v)) => (self.iterm(k), self.iterm(v)),
        };
        Term::ObjSingle(k, v)
    }

    fn sum_or(&mut self, f: impl FnOnce() -> Term, terms: Vec<Term>) -> Term {
        let mut iter = terms.into_iter().rev();
        let last = iter.next().unwrap_or_else(f);
        iter.fold(last, |acc, x| {
            Term::Math(self.insert_term(x), MathOp::Add, self.insert_term(acc))
        })
    }
}
