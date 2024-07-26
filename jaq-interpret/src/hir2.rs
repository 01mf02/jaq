use alloc::{boxed::Box, vec::Vec};
use jaq_syn::parse;

type NativeId = usize;
// TODO: change to TermId
type DefId = usize;
type TermId = usize;
type ModId = usize;
type VarId = usize;

enum Tailrec {
    Throw,
    Catch,
}

enum Term {
    Id,
    Var(VarId),
    Pipe(TermId, bool, TermId),
    Comma(TermId, TermId),
    CallDef(DefId, Vec<TermId>, usize, Option<Tailrec>),
    CallArg(VarId, usize),
    Break(usize),
}

struct Ctx<S> {
    term_map: Vec<Term>,
    /// `def_map[did]` yields the body of a definition with ID `did`
    def_map: Vec<Term>,
    /// `mod_map[mid]` yields all top-level definitions contained inside a module with ID `mid`
    mod_map: Vec<Vec<Sig<S, usize>>>,

    imported_mods: Vec<(ModId, S)>,
    included_mods: Vec<ModId>,

    global_vars: Vec<S>,
    imported_vars: Vec<(S, ModId)>,
    local: Vec<Local<S>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Sig<S, A> {
    name: S,
    // TODO: we could analyse for each argument whether it is TR, and
    // use this when converting args at callsite
    args: A,
    id: DefId,
    /// true if there is at least one tail-recursive call to this filter from within itself
    tailrec: bool,
}

impl<S, A> Sig<S, A> {
    fn map_args<A2>(self, f: impl FnOnce(A) -> A2) -> Sig<S, A2> {
        Sig {
            name: self.name,
            args: f(self.args),
            id: self.id,
            tailrec: self.tailrec,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Local<S> {
    Var(S),
    Label(S),
    Parent(Sig<S, Vec<S>>),
    Sibling(Sig<S, usize>),
    TailrecObstacle,
}

impl<'s> Ctx<&'s str> {
    fn with<T>(&mut self, local: Local<&'s str>, f: impl FnOnce(&mut Self) -> T) -> T {
        self.local.push(local.clone());
        let y = f(self);
        assert_eq!(self.local.pop(), Some(local));
        y
    }

    fn module(&mut self, m: jaq_syn::graph::Module<&'s str, parse::Defs<&'s str>>) {
        m.body.into_iter().for_each(|def| self.def(def));
        let defs = self.local.drain(..).map(|l| match l {
            Local::Sibling(sig) => sig,
            _ => panic!(),
        });
        self.mod_map.push(defs.collect());
    }

    fn def(&mut self, d: parse::Def<&'s str, parse::Term<&'s str>>) {
        let did = self.def_map.len();
        self.def_map.push(Term::Id);

        let arity = d.args.len();
        let sig = Sig {
            name: d.name,
            args: d.args,
            id: did,
            tailrec: false,
        };
        self.local.push(Local::Parent(sig));

        let t = self.term(d.body);

        // turn the parent into a sibling
        match self.local.pop() {
            Some(Local::Parent(sig)) => {
                self.local.push(Local::Sibling(sig.map_args(|a| a.len())));
            }
            _ => panic!(),
        }
    }

    fn term(&mut self, t: parse::Term<&'s str>) -> Term {
        use parse::Term::*;
        match t {
            Id => Term::Id,
            Pipe(l, Some(x), r) => Term::Pipe(
                self.iterm(*l),
                true,
                self.with(Local::Var(x), |c| c.iterm_tr(*r)),
            ),
            Pipe(l, None, r) => Term::Pipe(self.iterm(*l), false, self.iterm_tr(*r)),
            Break(x) => self.break_(x),
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
            _ => todo!(),
        }
    }

    fn iterm(&mut self, t: parse::Term<&'s str>) -> TermId {
        self.with(Local::TailrecObstacle, |c| c.iterm(t))
    }

    fn iterm_tr(&mut self, t: parse::Term<&'s str>) -> TermId {
        let t = self.term(t);
        let tid = self.term_map.len();
        self.term_map.push(t);
        tid
    }

    fn call_mod(&mut self, module: &'s str, name: &'s str, args: Vec<TermId>) -> Term {
        let local = self.local.iter();
        let vars = local.map(|l| match l {
            Local::Var(x) => 1,
            Local::Label(_) | Local::Sibling(..) | Local::TailrecObstacle => 0,
            Local::Parent(sig) => sig.args.len(),
        });
        let vars = vars.sum();
        let mut imported_mods = self.imported_mods.iter().rev();
        let mid = imported_mods.find(|(mid, module_)| module == *module_);
        let mid = mid.expect("module not included").0;
        let mut defs = self.mod_map[mid].iter().rev();
        let sig = defs.find(|sig| name == sig.name && args.len() == sig.args);
        let sig = sig.unwrap();
        let call = sig.tailrec.then_some(Tailrec::Catch);
        return Term::CallDef(sig.id, args, vars, call);
    }

    fn call(&mut self, name: &'s str, args: Vec<TermId>) -> Term {
        let mut tailrec = true;
        let mut i = 0;
        let mut labels = 0;
        for l in self.local.iter_mut().rev() {
            match l {
                Local::Var(_) => i += 1,
                Local::Label(_) => labels += 1,
                Local::Sibling(sig) => {
                    if name == sig.name && args.len() == sig.args {
                        let call = sig.tailrec.then_some(Tailrec::Catch);
                        return Term::CallDef(sig.id, args, i, call);
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
                    if name == sig.name && args.len() == sig.args.len() {
                        sig.tailrec = sig.tailrec || tailrec;
                        let call = tailrec.then_some(Tailrec::Throw);
                        return Term::CallDef(sig.id, args, i, call);
                    }
                }
                Local::TailrecObstacle => tailrec = false,
            }
        }
        // TODO: consider included mods
        panic!("undefined filter")
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
        panic!("unbound variable")
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
        panic!("undefined label")
    }
}
