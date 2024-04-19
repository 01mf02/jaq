use crate::box_iter::{box_once, flat_map_with, map_with, BoxIter};
use crate::results::{fold, recurse, then, Fold, Results};
use crate::val::{Val, ValR2, ValR2s, ValT};
use crate::{rc_lazy_list, Bind, Ctx, Error};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::ops::ControlFlow;
use dyn_clone::DynClone;
use jaq_syn::filter::FoldType;
use jaq_syn::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Debug, Clone)]
pub struct Owned<V = Val>(Id, Lut<V>);

/// Look-up table for indices stored in ASTs.
#[derive(Clone, Debug)]
struct Lut<V = Val> {
    defs: Box<[Ast]>,
    natives: Box<[Native<V>]>,
}

impl<V> Default for Owned<V> {
    fn default() -> Self {
        Self::new(Id(0), [Ast::Id].into(), [].into())
    }
}

#[derive(Debug)]
pub struct Ref<'a, V = Val>(Id, &'a Lut<V>);

impl<'a, V> Clone for Ref<'a, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, V> Copy for Ref<'a, V> {}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Id(pub usize);

#[derive(Clone, Debug)]
pub enum CallTyp {
    /// everything that is not tail-recursive
    Normal,
    /// set up a tail-recursion handler
    Catch,
    /// throw a tail-recursion exception to be caught by the handler
    Throw,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TailCall<V>(Id, crate::Vars<V>, V);

#[derive(Clone, Debug)]
pub(crate) struct Call {
    pub id: Id,
    pub typ: CallTyp,
    pub skip: usize,
    pub args: Box<[Bind<Id, Id>]>,
}

impl<V> Owned<V> {
    pub(crate) fn new(main: Id, defs: Box<[Ast]>, natives: Box<[Native<V>]>) -> Self {
        Self(main, Lut { defs, natives })
    }
}

/// Abstract syntax tree for filters.
#[derive(Clone, Debug)]
pub(crate) enum Ast {
    Id,
    ToString,

    Int(isize),
    Num(String),
    Str(String),
    Array(Id),
    ObjEmpty,
    ObjSingle(Id, Id),

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

    Native(usize, Box<[Id]>),
}

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
pub trait Update<'a, V = Val>: Fn(V) -> ValR2s<'a, V> + DynClone {}

impl<'a, V, T: Fn(V) -> ValR2s<'a, V> + Clone> Update<'a, V> for T {}

dyn_clone::clone_trait_object!(<'a, V> Update<'a, V>);

/// Enhance the context `ctx` with variables bound to the outputs of `args` executed on `cv`,
/// and return the enhanced contexts together with the original value of `cv`.
///
/// This is used when we call filters with variable arguments.
fn bind_vars<'a, V: ValT>(
    mut args: impl Iterator<Item = Bind<Ref<'a, V>, Ref<'a, V>>> + Clone + 'a,
    ctx: Ctx<'a, V>,
    cv: Cv<'a, V>,
) -> Results<'a, Cv<'a, V>, Error<V>> {
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

fn run_cvs<'a, V: ValT>(f: Ref<'a, V>, cvs: Results<'a, Cv<'a, V>, Error<V>>) -> ValR2s<'a, V> {
    Box::new(cvs.flat_map(move |cv| then(cv, |cv| f.run(cv))))
}

fn reduce<'a, T, V, F>(xs: Results<'a, T, Error<V>>, init: V, f: F) -> ValR2s<V>
where
    T: Clone + 'a,
    V: Clone + 'a,
    F: Fn(T, V) -> ValR2s<'a, V> + 'a,
{
    let xs = rc_lazy_list::List::from_iter(xs);
    Box::new(fold(false, xs, Fold::Input(init), f))
}

type Cv<'c, V = Val> = (Ctx<'c, V>, V);

/// A filter which is implemented using function pointers.
#[derive(Clone)]
pub struct Native<V = Val> {
    run: RunPtr<V>,
    update: UpdatePtr<V>,
}

/// Run function pointer.
pub type RunPtr<V = Val> = for<'a> fn(Args<'a, V>, Cv<'a, V>) -> ValR2s<'a, V>;
/// Update function pointer.
pub type UpdatePtr<V = Val> =
    for<'a> fn(Args<'a, V>, Cv<'a, V>, Box<dyn Update<'a, V> + 'a>) -> ValR2s<'a, V>;

impl<V> Native<V> {
    /// Create a native filter from a run function, without support for updates.
    pub const fn new(run: RunPtr<V>) -> Self {
        Self::with_update(run, |_, _, _| box_once(Err(Error::PathExp)))
    }

    /// Create a native filter from a run function and an update function (used for `filter |= ...`).
    // TODO for v2.0: remove this
    pub const fn with_update(run: RunPtr<V>, update: UpdatePtr<V>) -> Self {
        Self { run, update }
    }
}

// TODO for v2.0: remove this
impl<V> core::fmt::Debug for Native<V> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Native").finish()
    }
}

/// Arguments passed to a native filter.
pub struct Args<'a, V = Val>(&'a [Id], &'a Lut<V>);

impl<'a, V> Clone for Args<'a, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, V> Copy for Args<'a, V> {}

impl<'a, V: ValT> Args<'a, V> {
    /// Obtain the n-th argument passed to the filter, crash if it is not given.
    // This function returns an `impl` in order not to expose the `Filter` type publicly.
    // It would be more elegant to implement `Index<usize>` here instead,
    // but because of returning `impl`, we cannot do this right now, see:
    // <https://github.com/rust-lang/rust/issues/63063>.
    // TODO for v2.0: make this take `&self`?
    pub fn get(self, i: usize) -> impl FilterT<'a, V> {
        Ref(self.0[i], self.1)
    }
}

impl<'a, V: ValT> FilterT<'a, V> for &'a Owned<V> {
    fn run(self, cv: Cv<'a, V>) -> ValR2s<'a, V> {
        Ref(self.0, &self.1).run(cv)
    }

    fn update(self, cv: Cv<'a, V>, f: Box<dyn Update<'a, V> + 'a>) -> ValR2s<'a, V> {
        Ref(self.0, &self.1).update(cv, f)
    }
}

impl<'a, V: ValT> FilterT<'a, V> for Ref<'a, V> {
    fn run(self, cv: Cv<'a, V>) -> ValR2s<'a, V> {
        use alloc::string::ToString;
        use core::iter::{once, once_with};
        // wrap a filter AST with the filter definitions
        let w = move |id: &Id| Ref(*id, self.1);
        match &self.1.defs[self.0 .0] {
            Ast::Id => box_once(Ok(cv.1)),
            Ast::ToString => Box::new(once_with(move || match cv.1.as_str() {
                Some(_) => Ok(cv.1),
                None => Ok(V::from(cv.1.to_string())),
            })),
            Ast::Int(n) => box_once(Ok(V::from(*n))),
            Ast::Num(x) => box_once(V::from_num(x.clone())),
            Ast::Str(s) => Box::new(once_with(move || Ok(V::from(s.clone())))),
            Ast::Array(f) => Box::new(once_with(move || w(f).run(cv).collect::<Result<_, _>>())),
            Ast::ObjEmpty => box_once(V::from_map([])),
            Ast::ObjSingle(k, v) => {
                Box::new(Self::cartesian(w(k), w(v), cv).map(|(k, v)| V::from_map([(k?, v?)])))
            }
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
            Ast::Path(f, path) => {
                let path = path.map_ref(|i| {
                    let cv = cv.clone();
                    crate::into_iter::collect_if_once(move || w(i).run(cv))
                });
                flat_map_with(w(f).run(cv), path, |y, path| {
                    then(y, |y| {
                        flat_map_with(path.explode(), y, |path, y| then(path, |path| path.run(y)))
                    })
                })
            }

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
                    box_once(Ok(V::from(*stop)))
                } else {
                    Box::new(w(r).run(cv).map(|r| Ok(V::from(r?.as_bool()))))
                }
            }),
            Ast::Math(l, op, r) => {
                Box::new(Self::cartesian(w(l), w(r), cv).map(|(x, y)| op.run(x?, y?)))
            }
            Ast::Ord(l, op, r) => Box::new(
                Self::cartesian(w(l), w(r), cv).map(|(x, y)| Ok(V::from(op.run(&x?, &y?)))),
            ),

            Ast::Fold(typ, xs, init, f) => {
                let xs = rc_lazy_list::List::from_iter(w(xs).run(cv.clone()));
                let init = w(init).run(cv.clone());
                let f = move |x, v| w(f).run((cv.0.clone().cons_var(x), v));
                use Fold::{Input, Output};
                match typ {
                    FoldType::Reduce => Box::new(fold(false, xs, Output(init), f)),
                    FoldType::For => Box::new(fold(true, xs, Output(init), f)),
                    FoldType::Foreach => flat_map_with(init, xs, move |i, xs| {
                        then(i, |i| Box::new(fold(true, xs, Input(i), f.clone())))
                    }),
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
                match call.typ {
                    CallTyp::Normal => run_cvs(def, cvs),
                    CallTyp::Catch => Box::new(crate::Stack::new(
                        Vec::from([run_cvs(def, cvs)]),
                        move |r| match r {
                            Err(Error::TailCall(TailCall(id, vars, v))) if id == call.id => {
                                ControlFlow::Continue(def.run((Ctx { inputs, vars }, v)))
                            }
                            Ok(_) | Err(_) => ControlFlow::Break(r),
                        },
                    )),
                    CallTyp::Throw => Box::new(cvs.map(move |cv| {
                        cv.and_then(|cv| Err(Error::TailCall(TailCall(call.id, cv.0.vars, cv.1))))
                    })),
                }
            }

            Ast::Native(id, args) => (self.1.natives[*id].run)(Args(args, self.1), cv),
        }
    }

    fn update(self, cv: Cv<'a, V>, f: Box<dyn Update<'a, V> + 'a>) -> ValR2s<'a, V> {
        let err = box_once(Err(Error::PathExp));
        let w = move |id: &Id| Ref(*id, self.1);
        match &self.1.defs[self.0 .0] {
            Ast::ToString => err,
            Ast::Int(_) | Ast::Num(_) | Ast::Str(_) => err,
            Ast::Array(_) | Ast::ObjEmpty | Ast::ObjSingle(..) => err,
            Ast::Neg(_) | Ast::Logic(..) | Ast::Math(..) | Ast::Ord(..) => err,
            Ast::Update(..) | Ast::UpdateMath(..) | Ast::Assign(..) => err,

            // these are up for grabs to implement :)
            Ast::Try(..) | Ast::Alt(..) | Ast::Fold(..) => {
                unimplemented!("updating with this operator is not supported yet")
            }

            Ast::Id => f(cv.1),
            Ast::Path(l, path) => {
                let path = path.map_ref(|i| {
                    let cv = cv.clone();
                    crate::into_iter::collect_if_once(move || w(i).run(cv))
                });
                let f = move |v| {
                    let mut paths = path.clone().explode();
                    box_once(paths.try_fold(v, |acc, path| path?.update(acc, &f)))
                };
                w(l).update(cv, Box::new(f))
            }
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

            Ast::Native(id, args) => (self.1.natives[*id].update)(Args(args, self.1), cv, f),
        }
    }
}

type Triple<T> = (T, T, T);

/// Function from a value to a stream of value results.
// TODO for v2.0: Remove `Clone` bound and make sub-trait requiring `Copy` for derived functions
// try to implement functions on &self?
pub trait FilterT<'a, V: ValT = Val>: Clone + 'a {
    /// `f.run((c, v))` returns the output of `v | f` in the context `c`.
    fn run(self, cv: Cv<'a, V>) -> ValR2s<'a, V>;

    /// `p.update((c, v), f)` returns the output of `v | p |= f` in the context `c`.
    fn update(self, cv: Cv<'a, V>, f: Box<dyn Update<'a, V> + 'a>) -> ValR2s<'a, V>;

    /// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
    ///
    /// This has a special optimisation for the case where only a single `v` is returned.
    /// In that case, we can consume `cv` instead of cloning it.
    fn pipe<T: 'a, F>(self, cv: Cv<'a, V>, f: F) -> Results<'a, T, Error<V>>
    where
        F: Fn(Cv<'a, V>, V) -> Results<'a, T, Error<V>> + 'a,
    {
        flat_map_with(self.run(cv.clone()), cv, move |y, cv| then(y, |y| f(cv, y)))
    }

    /// Run `self` and `r` and return the cartesian product of their outputs.
    fn cartesian(self, r: Self, cv: Cv<'a, V>) -> BoxIter<'a, (ValR2<V>, ValR2<V>)> {
        flat_map_with(self.run(cv.clone()), cv, move |l, cv| {
            map_with(r.clone().run(cv), l, |r, l| (l, r))
        })
    }

    /// Run `self`, `m`, and `r`, and return the cartesian product of their outputs.
    fn cartesian3(self, m: Self, r: Self, cv: Cv<'a, V>) -> BoxIter<'a, Triple<ValR2<V>>> {
        flat_map_with(self.run(cv.clone()), cv, move |l, cv| {
            let r = r.clone();
            flat_map_with(m.clone().run(cv.clone()), (l, cv), move |m, (l, cv)| {
                map_with(r.clone().run(cv), (l, m), |r, (l, m)| (l, m, r))
            })
        })
    }

    /// Return the output of `recurse(f)` if `inner` and `outer` are true.
    ///
    /// This function implements a generalisation of `recurse(f)`:
    /// if `inner` is true, it returns values for which `f` yields at least one output, and
    /// if `outer` is true, it returns values for which `f` yields no output.
    /// This is useful to implement `while` and `until`.
    #[deprecated(since = "1.2.0")]
    fn recurse(self, inner: bool, outer: bool, cv: Cv<'a, V>) -> ValR2s<V> {
        let f = move |v| self.clone().run((cv.0.clone(), v));
        Box::new(recurse(inner, outer, box_once(Ok(cv.1)), f))
    }

    /// Return the output of `recurse(l) |= f`.
    #[deprecated(since = "1.2.0")]
    fn recurse_update(self, cv: Cv<'a, V>, f: Box<dyn Update<'a, V> + 'a>) -> ValR2s<'a, V> {
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
