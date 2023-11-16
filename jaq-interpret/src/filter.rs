use crate::box_iter::{box_once, flat_map_with, map_with, BoxIter};
use crate::results::{fold, recurse, then, Fold, Results};
use crate::val::{Val, ValR, ValRs};
use crate::{rc_lazy_list, Bind, Ctx, Error};
use alloc::{boxed::Box, string::String, vec::Vec};
use dyn_clone::DynClone;
use jaq_syn::filter::FoldType;
use jaq_syn::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Debug, Default, Clone)]
pub struct Owned(Id, Vec<Ast>);

#[derive(Debug, Copy, Clone)]
pub struct Ref<'a>(Id, &'a [Ast]);

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Id(pub usize);

#[derive(Copy, Clone, Debug)]
pub struct Tailrec(pub bool);

#[derive(Clone, Debug)]
pub enum CallTyp {
    /// is the filter called from inside itself?
    /// if None, then the called filter is not tail-recursive
    /// if Some(tr), then the called filter is tail-recursive,
    /// and tr indicates if the call is tail-recursive
    Inside(Option<Tailrec>),
    /// is the filter called from outside?
    /// Tailrec indicates whether the filter is tail-recursive
    Outside(Tailrec),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TailCall(Id, crate::Vars, Val);

#[derive(Clone, Debug)]
pub(crate) struct Call {
    pub id: Id,
    pub typ: CallTyp,
    pub skip: usize,
    pub args: Vec<Bind<Id, Id>>,
}

impl Owned {
    pub(crate) fn new(main: Id, recs: Vec<Ast>) -> Self {
        Self(main, recs)
    }
}

/// Function from a value to a stream of value results.
#[derive(Clone, Debug, Default)]
pub(crate) enum Ast {
    #[default]
    Id,
    ToString,

    Int(isize),
    Float(f64),
    Str(String),
    Array(Id),
    Object(Vec<(Id, Id)>),

    Try(Id, Id),
    Neg(Id),
    Pipe(Id, bool, Id),
    Comma(Id, Id),
    Alt(Id, Id),
    Ite(Id, Id, Id),
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
    Fold(FoldType, Id, Id, Id),

    Path(Id, crate::path::Path<Id>),

    Update(Id, Id),
    UpdateMath(Id, MathOp, Id),
    Assign(Id, Id),

    Logic(Id, bool, Id),
    Math(Id, MathOp, Id),
    Ord(Id, OrdOp, Id),

    Var(usize),
    Call(Call),

    Native(Native, Vec<Id>),
}

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
pub trait Update<'a>: Fn(Val) -> ValRs<'a> + DynClone {}

impl<'a, T: Fn(Val) -> ValRs<'a> + Clone> Update<'a> for T {}

dyn_clone::clone_trait_object!(<'a> Update<'a>);

/// Enhance the context `ctx` with variables bound to the outputs of `args` executed on `cv`,
/// and return the enhanced contexts together with the original value of `cv`.
///
/// This is used when we call filters with variable arguments.
fn bind_vars<'a, I>(mut args: I, ctx: Ctx<'a>, cv: Cv<'a>) -> Results<'a, Cv<'a>, Error>
where
    I: Iterator<Item = Bind<Ref<'a>, Ref<'a>>> + Clone + 'a,
{
    match args.next() {
        Some(Bind::Var(arg)) => flat_map_with(
            arg.run(cv.clone()),
            (ctx, cv, args),
            |y, (ctx, cv, args)| then(y, |y| bind_vars(args, ctx.cons_var(y), cv)),
        ),
        Some(Bind::Fun(Ref(arg, _defs))) => bind_vars(args, ctx.cons_fun((arg, cv.0.clone())), cv),
        None => box_once(Ok((ctx, cv.1))),
    }
}

fn run_cvs<'a>(f: Ref<'a>, cvs: Results<'a, Cv<'a>, Error>) -> impl Iterator<Item = ValR> + 'a {
    cvs.flat_map(move |cv| then(cv, |cv| f.run(cv)))
}

type ObjVec = Vec<(ValR, ValR)>;
fn obj_cart<'a, I>(mut args: I, cv: Cv<'a>, prev: ObjVec) -> BoxIter<'a, ObjVec>
where
    I: Iterator<Item = (Ref<'a>, Ref<'a>)> + Clone + 'a,
{
    if let Some((l, r)) = args.next() {
        let iter = l.run(cv.clone());
        flat_map_with(iter, (args, cv, prev), move |l, (args, cv, prev)| {
            let iter = r.run(cv.clone());
            flat_map_with(iter, (l, args, cv, prev), |r, (l, args, cv, mut prev)| {
                prev.push((l, r));
                obj_cart(args, cv, prev)
            })
        })
    } else {
        box_once(prev)
    }
}

fn reduce<'a, T: Clone + 'a, F>(xs: Results<'a, T, Error>, init: Val, f: F) -> ValRs
where
    F: Fn(T, Val) -> ValRs<'a> + 'a,
{
    let xs = rc_lazy_list::List::from_iter(xs);
    Box::new(fold(false, xs, Fold::Input(init), f))
}

type Cv<'c> = (Ctx<'c>, Val);

/// A filter which is implemented using function pointers.
#[derive(Clone)]
pub struct Native {
    run: RunPtr,
    update: UpdatePtr,
}

/// Run function pointer.
pub type RunPtr = for<'a> fn(Args<'a>, Cv<'a>) -> ValRs<'a>;
/// Update function pointer.
pub type UpdatePtr = for<'a> fn(Args<'a>, Cv<'a>, Box<dyn Update<'a> + 'a>) -> ValRs<'a>;

impl Native {
    /// Create a native filter from a run function, without support for updates.
    pub const fn new(run: RunPtr) -> Self {
        Self::with_update(run, |_, _, _| box_once(Err(Error::PathExp)))
    }

    /// Create a native filter from a run function and an update function (used for `filter |= ...`).
    pub const fn with_update(run: RunPtr, update: UpdatePtr) -> Self {
        Self { run, update }
    }
}

impl core::fmt::Debug for Native {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Native").finish()
    }
}

/// Arguments passed to a native filter.
#[derive(Copy, Clone)]
pub struct Args<'a>(&'a [Id], &'a [Ast]);

impl<'a> Args<'a> {
    /// Obtain the n-th argument passed to the filter, crash if it is not given.
    // This function returns an `impl` in order not to expose the `Filter` type publicly.
    // It would be more elegant to implement `Index<usize>` here instead,
    // but because of returning `impl`, we cannot do this right now, see:
    // <https://github.com/rust-lang/rust/issues/63063>.
    pub fn get(self, i: usize) -> impl FilterT<'a> {
        Ref(self.0[i], self.1)
    }
}

impl<'a> FilterT<'a> for &'a Owned {
    fn run(self, cv: Cv<'a>) -> ValRs<'a> {
        Ref(self.0, &self.1).run(cv)
    }

    fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        Ref(self.0, &self.1).update(cv, f)
    }
}

impl<'a> FilterT<'a> for Ref<'a> {
    fn run(self, cv: Cv<'a>) -> ValRs<'a> {
        use core::iter::{once, once_with};
        // wrap a filter AST with the filter definitions
        let w = move |id: &Id| Ref(*id, self.1);
        match &self.1[self.0 .0] {
            Ast::Id => box_once(Ok(cv.1)),
            Ast::ToString => Box::new(once_with(move || Ok(Val::str(cv.1.to_string_or_clone())))),
            Ast::Int(n) => box_once(Ok(Val::Int(*n))),
            Ast::Float(x) => box_once(Ok(Val::Float(*x))),
            Ast::Str(s) => Box::new(once_with(move || Ok(Val::str(s.clone())))),
            Ast::Array(f) => Box::new(once_with(move || {
                w(f).run(cv).collect::<Result<_, _>>().map(Val::arr)
            })),
            Ast::Object(o) if o.is_empty() => box_once(Ok(Val::Obj(Default::default()))),
            Ast::Object(o) => Box::new(
                obj_cart(o.iter().map(move |(k, v)| (w(k), w(v))), cv, Vec::new()).map(|kvs| {
                    kvs.into_iter()
                        .map(|(k, v)| Ok((k?.to_str()?, v?)))
                        .collect::<Result<_, _>>()
                        .map(Val::obj)
                }),
            ),
            Ast::Try(f, c) => Box::new(w(f).run((cv.0.clone(), cv.1)).flat_map(move |y| {
                y.map_or_else(
                    |e| w(c).run((cv.0.clone(), e.as_val())),
                    |v| box_once(Ok(v)),
                )
            })),
            Ast::Neg(f) => Box::new(w(f).run(cv).map(|v| -v?)),

            // `l | r`
            Ast::Pipe(l, false, r) => {
                let l = w(l).run((cv.0.clone(), cv.1));
                flat_map_with(l, cv.0, move |y, ctx| then(y, |y| w(r).run((ctx, y))))
            }
            // `l as $x | r`
            Ast::Pipe(l, true, r) => w(l).pipe(cv, move |cv, y| w(r).run((cv.0.cons_var(y), cv.1))),

            Ast::Comma(l, r) => Box::new(w(l).run(cv.clone()).chain(w(r).run(cv))),
            Ast::Alt(l, r) => {
                let mut l = w(l)
                    .run(cv.clone())
                    .filter(|v| v.as_ref().map_or(true, |v| v.as_bool()));
                match l.next() {
                    Some(head) => Box::new(once(head).chain(l)),
                    None => w(r).run(cv),
                }
            }
            Ast::Ite(if_, then_, else_) => w(if_).pipe(cv, move |cv, v| {
                w(if v.as_bool() { then_ } else { else_ }).run(cv)
            }),
            Ast::Path(f, path) => then(path.eval(|p| w(p).run(cv.clone())), |path| {
                let outs = w(f).run(cv).map(move |i| path.collect(i?));
                Box::new(
                    outs.flat_map(|vals| then(vals, |vals| Box::new(vals.into_iter().map(Ok)))),
                )
            }),
            Ast::Update(path, f) => w(path).update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| w(f).run((cv.0.clone(), v))),
            ),
            Ast::UpdateMath(path, op, f) => w(f).pipe(cv, move |cv, y| {
                w(path).update(cv, Box::new(move |x| box_once(op.run(x, y.clone()))))
            }),
            Ast::Assign(path, f) => w(f).pipe(cv, move |cv, y| {
                w(path).update(cv, Box::new(move |_| box_once(Ok(y.clone()))))
            }),
            Ast::Logic(l, stop, r) => w(l).pipe(cv, move |cv, l| {
                if l.as_bool() == *stop {
                    box_once(Ok(Val::Bool(*stop)))
                } else {
                    Box::new(w(r).run(cv).map(|r| Ok(Val::Bool(r?.as_bool()))))
                }
            }),
            Ast::Math(l, op, r) => {
                Box::new(Self::cartesian(w(l), w(r), cv).map(|(x, y)| op.run(x?, y?)))
            }
            Ast::Ord(l, op, r) => Box::new(
                Self::cartesian(w(l), w(r), cv).map(|(x, y)| Ok(Val::Bool(op.run(&x?, &y?)))),
            ),

            Ast::Fold(typ, xs, init, f) => {
                let xs = rc_lazy_list::List::from_iter(w(xs).run(cv.clone()));
                let init = w(init).run(cv.clone());
                let f = move |x, v| w(f).run((cv.0.clone().cons_var(x), v));
                use Fold::{Input, Output};
                match typ {
                    FoldType::Reduce => Box::new(fold(false, xs, Output(init), f)),
                    FoldType::For => Box::new(fold(true, xs, Output(init), f)),
                    FoldType::Foreach => Box::new(init.flat_map(move |i| {
                        then(i, |i| Box::new(fold(true, xs.clone(), Input(i), f.clone())))
                    })),
                }
            }

            Ast::Var(v) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(v) => box_once(Ok(v.clone())),
                Bind::Fun(f) => w(&f.0).run((cv.0.with_vars(f.1.clone()), cv.1)),
            },
            Ast::Call(call) => {
                let def = w(&call.id);
                let ctx = cv.0.clone().skip_vars(call.skip);
                let inputs = cv.0.inputs;
                let cvs = bind_vars(call.args.iter().map(move |a| a.as_ref().map(w)), ctx, cv);
                let catch = move |r| match r {
                    Err(Error::TailCall(TailCall(id, vars, v))) if id == call.id => {
                        Err(def.run((Ctx { inputs, vars }, v)))
                    }
                    Ok(_) | Err(_) => Ok(r),
                };
                let tailrec = |init| Box::new(crate::Stack::new(Vec::from([init]), catch));
                match call.typ {
                    CallTyp::Outside(Tailrec(false)) => Box::new(run_cvs(def, cvs)),
                    CallTyp::Outside(Tailrec(true)) => {
                        tailrec(Box::new(run_cvs(def, cvs)) as Results<_, _>)
                    }
                    // non-TR call in a TR filter
                    CallTyp::Inside(Some(Tailrec(false))) => {
                        tailrec(Box::new(once_with(move || run_cvs(def, cvs)).flatten()))
                    }
                    CallTyp::Inside(Some(Tailrec(true))) => Box::new(cvs.map(move |cv| {
                        cv.and_then(|cv| Err(Error::TailCall(TailCall(call.id, cv.0.vars, cv.1))))
                    })),
                    // non-TR call in non-TR filter
                    CallTyp::Inside(None) => {
                        Box::new(once_with(move || run_cvs(def, cvs)).flatten())
                    }
                }
            }

            Ast::Native(Native { run, .. }, args) => (run)(Args(args, self.1), cv),
        }
    }

    fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        let err = box_once(Err(Error::PathExp));
        let w = move |id: &Id| Ref(*id, self.1);
        match &self.1[self.0 .0] {
            Ast::ToString => err,
            Ast::Int(_) | Ast::Float(_) | Ast::Str(_) => err,
            Ast::Array(_) | Ast::Object(_) => err,
            Ast::Neg(_) | Ast::Logic(..) | Ast::Math(..) | Ast::Ord(..) => err,
            Ast::Update(..) | Ast::UpdateMath(..) | Ast::Assign(..) => err,

            // these are up for grabs to implement :)
            Ast::Try(..) | Ast::Alt(..) => todo!(),
            Ast::Fold(..) => todo!(),

            Ast::Id => f(cv.1),
            Ast::Path(l, path) => w(l).update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| {
                    then(path.eval(|i| w(i).run((cv.0.clone(), v.clone()))), |path| {
                        path.update(v, &f)
                    })
                }),
            ),
            Ast::Pipe(l, false, r) => w(l).update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| w(r).update((cv.0.clone(), v), f.clone())),
            ),
            Ast::Pipe(l, true, r) => reduce(w(l).run(cv.clone()), cv.1, move |x, v| {
                w(r).update((cv.0.clone().cons_var(x), v), f.clone())
            }),
            Ast::Comma(l, r) => {
                let l = w(l).update((cv.0.clone(), cv.1), f.clone());
                Box::new(
                    l.flat_map(move |v| then(v, |v| w(r).update((cv.0.clone(), v), f.clone()))),
                )
            }
            Ast::Ite(if_, then_, else_) => reduce(w(if_).run(cv.clone()), cv.1, move |x, v| {
                w(if x.as_bool() { then_ } else { else_ }).update((cv.0.clone(), v), f.clone())
            }),

            Ast::Var(v) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(_) => err,
                Bind::Fun(l) => w(&l.0).update((cv.0.with_vars(l.1.clone()), cv.1), f),
            },
            Ast::Call(call) => {
                let def = w(&call.id);
                let init = cv.1.clone();
                let ctx = cv.0.clone().skip_vars(call.skip);
                let cvs = bind_vars(call.args.iter().map(move |a| a.as_ref().map(w)), ctx, cv);
                reduce(cvs, init, move |cv, v| def.update((cv.0, v), f.clone()))
            }

            Ast::Native(Native { update, .. }, args) => (update)(Args(args, self.1), cv, f),
        }
    }
}

/// Function from a value to a stream of value results.
pub trait FilterT<'a>: Clone + 'a {
    /// `f.run((c, v))` returns the output of `v | f` in the context `c`.
    fn run(self, cv: Cv<'a>) -> ValRs<'a>;

    /// `p.update((c, v), f)` returns the output of `v | p |= f` in the context `c`.
    fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a>;

    /// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
    ///
    /// This has a special optimisation for the case where only a single `v` is returned.
    /// In that case, we can consume `cv` instead of cloning it.
    fn pipe<T: 'a, F>(self, cv: Cv<'a>, f: F) -> Results<'a, T, Error>
    where
        F: Fn(Cv<'a>, Val) -> Results<'a, T, Error> + 'a,
    {
        flat_map_with(self.run(cv.clone()), cv, move |y, cv| then(y, |y| f(cv, y)))
    }

    /// Run `self` and `r` and return the cartesian product of their outputs.
    fn cartesian(self, r: Self, cv: Cv<'a>) -> Box<dyn Iterator<Item = (ValR, ValR)> + 'a> {
        flat_map_with(self.run(cv.clone()), cv, move |l, cv| {
            map_with(r.clone().run(cv), l, |r, l| (l, r))
        })
    }

    /// Return the output of `recurse(f)` if `inner` and `outer` are true.
    ///
    /// This function implements a generalisation of `recurse(f)`:
    /// if `inner` is true, it returns values for which `f` yields at least one output, and
    /// if `outer` is true, it returns values for which `f` yields no output.
    /// This is useful to implement `while` and `until`.
    #[deprecated(since = "1.2.0")]
    fn recurse(self, inner: bool, outer: bool, cv: Cv<'a>) -> ValRs {
        let f = move |v| self.clone().run((cv.0.clone(), v));
        Box::new(recurse(inner, outer, box_once(Ok(cv.1)), f))
    }

    /// Return the output of `recurse(l) |= f`.
    #[deprecated(since = "1.2.0")]
    fn recurse_update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        // implemented by the expansion of `def recurse(l): ., (l | recurse(l))`
        Box::new(f(cv.1).flat_map(move |v| {
            then(v, |v| {
                let (c, f) = (cv.0.clone(), f.clone());
                let slf = self.clone();
                let rec = move |v| slf.clone().recurse_update((c.clone(), v), f.clone());
                (self.clone()).update((cv.0.clone(), v), Box::new(rec))
            })
        }))
    }
}
