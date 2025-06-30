//! Filter execution.

use crate::box_iter::{self, box_once, flat_map_then, flat_map_then_with, flat_map_with, map_with};
use crate::compile::{Bind, Fold, Pattern, Tailrec, Term as Ast, TermId as Id};
use crate::fold::fold;
use crate::val::{ValR, ValT, ValX, ValXs};
use crate::{exn, rc_lazy_list, Bind as Arg, Error, Exn, Inputs, RcList};
use alloc::boxed::Box;
use dyn_clone::DynClone;

/// Combination of context and input value.
pub type Cv<'c, V, T = V> = (Ctx<'c, V>, T);
/// Combination of context and input value with a path.
type Cvp<'a, V> = Cv<'a, V, (V, RcList<V>)>;
type ValPathXs<'a, V> = ValXs<'a, (V, RcList<V>), V>;

type Lut<V> = crate::compile::Lut<Native<V>>;

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
pub trait Update<'a, V>: Fn(V) -> ValXs<'a, V> + DynClone {}

impl<'a, V, T: Fn(V) -> ValXs<'a, V> + Clone> Update<'a, V> for T {}

dyn_clone::clone_trait_object!(<'a, V> Update<'a, V>);

type BoxUpdate<'a, V> = Box<dyn Update<'a, V> + 'a>;

/// List of bindings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Vars<V>(RcList<Bind<V, usize, (Id, Self)>>);

impl<V> Vars<V> {
    fn get(&self, i: usize) -> Option<&Bind<V, usize, (Id, Self)>> {
        self.0.get(i)
    }
}

/// Filter execution context.
#[derive(Clone)]
pub struct Ctx<'a, V> {
    vars: Vars<V>,
    /// Number of bound labels at the current path
    ///
    /// This is used to create fresh break IDs.
    labels: usize,
    inputs: &'a Inputs<'a, V>,
}

impl<'a, V> Ctx<'a, V> {
    /// Construct a context.
    pub fn new(vars: impl IntoIterator<Item = V>, inputs: &'a Inputs<'a, V>) -> Self {
        Self {
            vars: Vars(RcList::new().extend(vars.into_iter().map(Bind::Var))),
            labels: 0,
            inputs,
        }
    }

    /// Add a new variable binding.
    fn cons_var(mut self, x: V) -> Self {
        self.vars.0 = self.vars.0.cons(Bind::Var(x));
        self
    }

    /// Add a new filter binding.
    fn cons_fun(mut self, (f, ctx): (Id, Self)) -> Self {
        self.vars.0 = self.vars.0.cons(Bind::Fun((f, ctx.vars)));
        self
    }

    fn cons_label(mut self) -> Self {
        self.labels += 1;
        self.vars.0 = self.vars.0.cons(Bind::Label(self.labels));
        self
    }

    /// Remove the `skip` most recent variable bindings.
    fn skip_vars(mut self, skip: usize) -> Self {
        if skip > 0 {
            self.vars.0 = self.vars.0.skip(skip).clone();
        }
        self
    }

    /// Replace variables in context with given ones.
    fn with_vars(&self, vars: Vars<V>) -> Self {
        Self {
            vars,
            labels: self.labels,
            inputs: self.inputs,
        }
    }

    /// Return remaining input values.
    pub fn inputs(&self) -> &'a Inputs<'a, V> {
        self.inputs
    }
}

impl<'a, V: Clone> Ctx<'a, V> {
    /// Remove the latest bound variable from the context.
    ///
    /// This is useful for writing [`Native`] filters.
    pub fn pop_var(&mut self) -> V {
        let (head, tail) = match core::mem::take(&mut self.vars.0).pop() {
            Some((Bind::Var(head), tail)) => (head, tail),
            _ => panic!(),
        };
        self.vars.0 = tail;
        head
    }

    /// Remove the latest bound function from the context.
    ///
    /// This is useful for writing [`Native`] filters.
    pub fn pop_fun(&mut self) -> (Id, Self) {
        let ((id, vars), tail) = match core::mem::take(&mut self.vars.0).pop() {
            Some((Bind::Fun(head), tail)) => (head, tail),
            _ => panic!(),
        };
        self.vars.0 = tail;
        (id, self.with_vars(vars))
    }
}

/// Enhance the context `ctx` with variables bound to the outputs of `args` executed on `cv`,
/// and return the enhanced contexts together with the original value of `cv`.
///
/// This is used when we call filters with variable arguments.
fn bind_vars<'a, V: ValT, T: 'a + Clone>(
    args: &'a [Arg<Id>],
    lut: &'a Lut<V>,
    ctx: Ctx<'a, V>,
    cv: Cv<'a, V, T>,
    proj: fn(&T) -> V,
) -> ValXs<'a, Cv<'a, V, T>, V> {
    match args.split_first() {
        Some((Arg::Var(arg), [])) => map_with(
            arg.run(lut, (cv.0.clone(), proj(&cv.1))),
            (ctx, cv.1),
            |y, (ctx, v)| Ok((ctx.cons_var(y?), v)),
        ),
        Some((Arg::Fun(arg), [])) => box_once(Ok((ctx.cons_fun((*arg, cv.0)), cv.1))),
        Some((Arg::Var(arg), rest)) => flat_map_then_with(
            arg.run(lut, (cv.0.clone(), proj(&cv.1))),
            (ctx, cv),
            move |y, (ctx, cv)| bind_vars(rest, lut, ctx.cons_var(y), cv, proj),
        ),
        Some((Arg::Fun(arg), rest)) => {
            bind_vars(rest, lut, ctx.cons_fun((*arg, cv.0.clone())), cv, proj)
        }
        None => box_once(Ok((ctx, cv.1))),
    }
}

fn bind_pat<'a, V: ValT>(
    (idxs, pat): &'a (Id, Pattern<Id>),
    lut: &'a Lut<V>,
    ctx: Ctx<'a, V>,
    cv: Cv<'a, V>,
) -> ValXs<'a, Ctx<'a, V>, V> {
    let (ctx0, v0) = cv.clone();
    let v1 = map_with(idxs.run(lut, cv), v0, move |i, v0| Ok(v0.index(&i?)?));
    match pat {
        Pattern::Var => Box::new(v1.map(move |v| Ok(ctx.clone().cons_var(v?)))),
        Pattern::Idx(pats) => flat_map_then_with(v1, (ctx, ctx0), move |v, (ctx, ctx0)| {
            bind_pats(pats, lut, ctx, (ctx0, v))
        }),
    }
}

fn bind_pats<'a, V: ValT>(
    pats: &'a [(Id, Pattern<Id>)],
    lut: &'a Lut<V>,
    ctx: Ctx<'a, V>,
    cv: Cv<'a, V>,
) -> ValXs<'a, Ctx<'a, V>, V> {
    match pats.split_first() {
        None => box_once(Ok(ctx)),
        Some((pat, [])) => bind_pat(pat, lut, ctx, cv),
        Some((pat, rest)) => {
            flat_map_then_with(bind_pat(pat, lut, ctx, cv.clone()), cv, |ctx, cv| {
                bind_pats(rest, lut, ctx, cv)
            })
        }
    }
}

fn run_and_bind<'a, V: ValT>(
    xs: &'a Id,
    lut: &'a Lut<V>,
    cv: Cv<'a, V>,
    pat: &'a Pattern<Id>,
) -> ValXs<'a, Ctx<'a, V>, V> {
    let xs = xs.run(lut, (cv.0.clone(), cv.1));
    match pat {
        Pattern::Var => map_with(xs, cv.0, move |y, ctx| Ok(ctx.cons_var(y?))),
        Pattern::Idx(pats) => flat_map_then_with(xs, cv.0, |y, ctx| {
            bind_pats(pats, lut, ctx.clone(), (ctx, y))
        }),
    }
}

fn bind_run<'a, V: ValT, T: Clone + 'a>(
    pat: &'a Pattern<Id>,
    r: &'a Id,
    lut: &'a Lut<V>,
    cv: Cv<'a, V, T>,
    y: V,
    run: impl Fn(&'a Id, &'a Lut<V>, Cv<'a, V, T>) -> ValXs<'a, T, V> + 'a,
) -> ValXs<'a, T, V> {
    match pat {
        Pattern::Var => run(r, lut, (cv.0.cons_var(y), cv.1)),
        Pattern::Idx(pats) => {
            let r = move |ctx, vp| run(r, lut, (ctx, vp));
            flat_map_then_with(bind_pats(pats, lut, cv.0.clone(), (cv.0, y)), cv.1, r)
        }
    }
}

fn label_run<'a, V, T: 'a>(
    cv: Cv<'a, V, T>,
    run: impl Fn(Cv<'a, V, T>) -> ValXs<'a, T, V>,
) -> ValXs<'a, T, V> {
    let ctx = cv.0.cons_label();
    let labels = ctx.labels;
    Box::new(run((ctx, cv.1)).map_while(move |y| match y {
        Err(Exn(exn::Inner::Break(b))) if b == labels => None,
        y => Some(y),
    }))
}

fn try_catch_run<'a, T: 'a, V: 'a, I: Iterator<Item = ValX<T, V>> + 'a>(
    mut ys: ValXs<'a, T, V>,
    f: impl Fn(Error<V>) -> I + 'a,
) -> ValXs<'a, T, V> {
    let mut end: Option<I> = None;
    Box::new(core::iter::from_fn(move || match &mut end {
        Some(end) => end.next(),
        None => match ys.next()? {
            Err(Exn(exn::Inner::Err(e))) => {
                end = Some(f(*e));
                end.as_mut().and_then(|end| end.next())
            }
            y => Some(y),
        },
    }))
}

fn fold_run<'a, V: Clone, T: Clone + 'a>(
    xs: impl Iterator<Item = ValX<Ctx<'a, V>, V>> + Clone + 'a,
    cv: Cv<'a, V, T>,
    init: &'a Id,
    update: &'a Id,
    fold_type: &'a Fold<Id>,
    run: impl Fn(&'a Id, Cv<'a, V, T>) -> ValXs<'a, T, V> + 'a + Copy,
) -> ValXs<'a, T, V> {
    let init = run(init, cv.clone());
    let update = move |ctx, v| run(update, (ctx, v));
    let inner = |_, y: &T| Some(y.clone());
    let inner_proj = |ctx, y: &T| Some((ctx, y.clone()));
    flat_map_then_with(init, xs, move |i, xs| match fold_type {
        Fold::Reduce => Box::new(fold(xs, i, update, |_| (), |_, _| None, Some)),
        Fold::Foreach(None) => Box::new(fold(xs, i, update, |_| (), inner, |_| None)),
        Fold::Foreach(Some(proj)) => flat_map_then(
            fold(xs, i, update, |ctx| ctx.clone(), inner_proj, |_| None),
            move |(ctx, y)| run(proj, (ctx, y)),
        ),
    })
}

/// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
///
/// This has a special optimisation for the case where only a single `v` is returned.
/// In that case, we can consume `cv` instead of cloning it.
fn pipe<'a, V: ValT, T: 'a>(
    l: &'a Id,
    lut: &'a Lut<V>,
    cv: Cv<'a, V>,
    r: impl Fn(Cv<'a, V>, V) -> ValXs<'a, T, V> + 'a,
) -> ValXs<'a, T, V> {
    flat_map_then_with(l.run(lut, cv.clone()), cv, move |y, cv| r(cv, y))
}

/// Run `self` and `r` and return the cartesian product of their outputs.
fn cartesian<'a, V: ValT>(
    l: &'a Id,
    r: &'a Id,
    lut: &'a Lut<V>,
    cv: Cv<'a, V>,
) -> box_iter::BoxIter<'a, (ValX<V>, ValX<V>)> {
    flat_map_with(l.run(lut, cv.clone()), cv, move |l, cv| {
        map_with(r.run(lut, cv), l, |r, l| (l, r))
    })
}

fn fold_update<'a, V: ValT>(
    lut: &'a Lut<V>,
    fold_type: &'a Fold<Id>,
    path: &'a Id,
    v: V,
    mut xs: impl Iterator<Item = Result<Ctx<'a, V>, Exn<V>>> + Clone + 'a,
    f: BoxUpdate<'a, V>,
) -> ValXs<'a, V> {
    let ctx = match xs.next() {
        Some(Ok(ctx)) => ctx,
        Some(Err(e)) => return box_once(Err(e)),
        None => match fold_type {
            Fold::Reduce => return f(v),
            Fold::Foreach(_) => return box_once(Ok(v)),
        },
    };

    let rec = |v, (xs, f)| fold_update(lut, fold_type, path, v, xs, f);
    let update: BoxUpdate<_> = match fold_type {
        Fold::Reduce => Box::new(move |v| rec(v, (xs.clone(), f.clone()))),
        Fold::Foreach(None) => {
            Box::new(move |v| flat_map_then_with(f(v), (xs.clone(), f.clone()), rec))
        }
        Fold::Foreach(Some(proj)) => {
            let ctx_ = ctx.clone();
            Box::new(move |v| {
                let proj = proj.update(lut, (ctx_.clone(), v), f.clone());
                flat_map_then_with(proj, (xs.clone(), f.clone()), rec)
            })
        }
    };
    path.update(lut, (ctx, v), update)
}

fn reduce<'a, T, V, F>(xs: ValXs<'a, T, V>, init: V, f: F) -> ValXs<'a, V>
where
    T: Clone + 'a,
    V: Clone + 'a,
    F: Fn(T, V) -> ValXs<'a, V> + 'a,
{
    let xs = rc_lazy_list::List::from_iter(xs);
    Box::new(fold(xs, init, f, |_| (), |_, _| None, Some))
}

fn lazy<I: Iterator, F: FnOnce() -> I>(f: F) -> impl Iterator<Item = I::Item> {
    core::iter::once_with(f).flatten()
}

#[test]
fn lazy_is_lazy() {
    let f = || panic!();
    let mut iter = core::iter::once(0).chain(lazy(|| box_once(f())));
    assert_eq!(iter.size_hint(), (1, None));
    assert_eq!(iter.next(), Some(0));
}

/// Runs `def recurse(f): ., (f? | recurse(f)); v | recurse(f)`.
fn recurse_run<'a, T: Clone + 'a, V: 'a, I: Iterator<Item = ValR<T, V>> + 'a>(
    x: T,
    f: &'a impl Fn(T) -> I,
) -> ValXs<'a, T, V> {
    let id = core::iter::once(Ok(x.clone()));
    Box::new(id.chain(f(x).flatten().flat_map(|y| recurse_run(y, f))))
}

/// Runs `def recurse: (.[]? | recurse), .; v | recurse |= f`.
///
/// This uses a `recurse` different from that of `recurse_run`; in particular,
/// `recurse_run` yields values from root to leafs, whereas
/// this function performs updates from leafs to root.
/// Performing updates from root to leafs can easily lead to
/// nontermination or other weird effects, such as
/// trying to update values that have been deleted.
fn recurse_update<'a, V: ValT + 'a>(v: V, f: &dyn Update<'a, V>) -> ValXs<'a, V> {
    use crate::path::Opt::Optional;
    box_iter::then(v.map_values(Optional, |v| recurse_update(v, f)), f)
}

/// A filter which is implemented using function pointers.
pub struct Native<V> {
    run: RunPtr<V>,
    paths: PathsPtr<V>,
    update: UpdatePtr<V>,
}

/// Run function pointer (see [`Id::run`]).
pub type RunPtr<V> = for<'a> fn(&'a Lut<V>, Cv<'a, V>) -> ValXs<'a, V>;
/// Paths function pointer (see [`Id::paths`]).
pub type PathsPtr<V> = for<'a> fn(&'a Lut<V>, Cvp<'a, V>) -> ValPathXs<'a, V>;
/// Update function pointer (see [`Id::update`]).
pub type UpdatePtr<V> = for<'a> fn(&'a Lut<V>, Cv<'a, V>, BoxUpdate<'a, V>) -> ValXs<'a, V>;

impl<V> Native<V> {
    /// Create a native filter from a run function.
    ///
    /// A filter created this way initially supports neither paths nor updates.
    /// For that, use [`Self::with_paths`] and [`Self::with_update`].
    pub const fn new(run: RunPtr<V>) -> Self {
        Self {
            run,
            paths: |_, cv| box_once(Err(Exn::from(Error::path_expr(cv.1 .0)))),
            update: |_, cv, _| box_once(Err(Exn::from(Error::path_expr(cv.1)))),
        }
    }

    /// Specify a paths function (used for `path(...)`).
    pub const fn with_paths(self, paths: PathsPtr<V>) -> Self {
        Self { paths, ..self }
    }

    /// Specify an update function (used for `filter |= ...`).
    ///
    /// If an update function is given, then a paths function should be implemented too.
    pub const fn with_update(self, update: UpdatePtr<V>) -> Self {
        Self { update, ..self }
    }
}

impl Id {
    /// `f.run(lut, (c, v))` returns the output of `v | f` in the context `c`.
    pub fn run<'a, V: ValT>(&self, lut: &'a Lut<V>, cv: Cv<'a, V>) -> ValXs<'a, V> {
        use alloc::string::ToString;
        use core::iter::once;
        match &lut.terms[self.0] {
            Ast::Id => box_once(Ok(cv.1)),
            Ast::Recurse => recurse_run(cv.1, &|v| v.values()),
            Ast::ToString => box_once(match cv.1.as_str() {
                Some(_) => Ok(cv.1),
                None => Ok(V::from(cv.1.to_string())),
            }),
            Ast::Int(n) => box_once(Ok(V::from(*n))),
            Ast::Num(x) => box_once(V::from_num(x).map_err(Exn::from)),
            Ast::Str(s) => box_once(Ok(V::from(s.clone()))),
            Ast::Arr(f) => box_once(f.run(lut, cv).collect()),
            Ast::ObjEmpty => box_once(V::from_map([]).map_err(Exn::from)),
            Ast::ObjSingle(k, v) => {
                Box::new(cartesian(k, v, lut, cv).map(|(k, v)| Ok(V::from_map([(k?, v?)])?)))
            }
            Ast::TryCatch(f, c) => try_catch_run(f.run(lut, (cv.0.clone(), cv.1)), move |e| {
                c.run(lut, (cv.0.clone(), e.into_val()))
            }),
            Ast::Neg(f) => Box::new(f.run(lut, cv).map(|v| Ok((-v?)?))),

            // `l | r`
            Ast::Pipe(l, None, r) => {
                flat_map_then_with(l.run(lut, (cv.0.clone(), cv.1)), cv.0, move |y, ctx| {
                    r.run(lut, (ctx, y))
                })
            }
            // `l as $x | r`, `l as [...] | r`, or `l as {...} | r`
            Ast::Pipe(l, Some(pat), r) => pipe(l, lut, cv, move |cv, y| {
                bind_run(pat, r, lut, cv, y, |f, lut, cv| f.run(lut, cv))
            }),

            Ast::Comma(l, r) => Box::new(l.run(lut, cv.clone()).chain(lazy(|| r.run(lut, cv)))),
            Ast::Alt(l, r) => {
                let mut l = l
                    .run(lut, cv.clone())
                    .filter(|v| v.as_ref().map_or(true, ValT::as_bool));
                match l.next() {
                    Some(head) => Box::new(once(head).chain(l)),
                    None => r.run(lut, cv),
                }
            }
            Ast::Ite(if_, then_, else_) => pipe(if_, lut, cv, move |cv, v| {
                if v.as_bool() { then_ } else { else_ }.run(lut, cv)
            }),
            Ast::Path(f, path) => {
                let path = path.map_ref(|i| {
                    let cv = cv.clone();
                    crate::into_iter::collect_if_once(move || i.run(lut, cv))
                });
                flat_map_then_with(f.run(lut, cv), path, |y, path| {
                    flat_map_then_with(path.explode(), y, |path, y| {
                        Box::new(path.run(y).map(|r| r.map_err(Exn::from)))
                    })
                })
            }

            Ast::Update(path, f) => path.update(
                lut,
                (cv.0.clone(), cv.1),
                Box::new(move |v| f.run(lut, (cv.0.clone(), v))),
            ),
            Ast::UpdateMath(path, op, f) => pipe(f, lut, cv, move |cv, y| {
                let u = move |x: V| box_once(op.run(x, y.clone()).map_err(Exn::from));
                path.update(lut, cv, Box::new(u))
            }),
            Ast::UpdateAlt(path, f) => pipe(f, lut, cv, move |cv, y| {
                let u = move |x: V| box_once(Ok(if x.as_bool() { x } else { y.clone() }));
                path.update(lut, cv, Box::new(u))
            }),
            Ast::Assign(path, f) => pipe(f, lut, cv, move |cv, y| {
                path.update(lut, cv, Box::new(move |_| box_once(Ok(y.clone()))))
            }),

            Ast::Logic(l, stop, r) => pipe(l, lut, cv, move |cv, l| {
                if l.as_bool() == *stop {
                    box_once(Ok(V::from(*stop)))
                } else {
                    Box::new(r.run(lut, cv).map(|r| Ok(V::from(r?.as_bool()))))
                }
            }),
            Ast::Math(l, op, r) => {
                Box::new(cartesian(l, r, lut, cv).map(|(x, y)| Ok(op.run(x?, y?)?)))
            }
            Ast::Cmp(l, op, r) => {
                Box::new(cartesian(l, r, lut, cv).map(|(x, y)| Ok(V::from(op.run(&x?, &y?)))))
            }

            Ast::Fold(xs, pat, init, update, fold_type) => {
                let xs = rc_lazy_list::List::from_iter(run_and_bind(xs, lut, cv.clone(), pat));
                fold_run(xs, cv, init, update, fold_type, |f, cv| f.run(lut, cv))
            }

            Ast::Var(v) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(v) => box_once(Ok(v.clone())),
                Bind::Fun((id, vars)) => id.run(lut, (cv.0.with_vars(vars.clone()), cv.1)),
                Bind::Label(l) => box_once(Err(Exn(exn::Inner::Break(*l)))),
            },
            Ast::CallDef(id, args, skip, tailrec) => {
                use core::ops::ControlFlow;
                let with_vars = move |vars| Ctx {
                    vars,
                    labels: cv.0.labels,
                    inputs: cv.0.inputs,
                };
                let cvs = bind_vars(args, lut, cv.0.clone().skip_vars(*skip), cv, Clone::clone);
                match tailrec {
                    None => flat_map_then(cvs, |cv| id.run(lut, cv)),
                    Some(Tailrec::Catch) => Box::new(crate::Stack::new(
                        [flat_map_then(cvs, |cv| id.run(lut, cv))].into(),
                        move |r| match r {
                            Err(Exn(exn::Inner::TailCall(tc))) if tc.id == *id => {
                                ControlFlow::Continue(id.run(lut, (with_vars(tc.vars), tc.val)))
                            }
                            Ok(_) | Err(_) => ControlFlow::Break(r),
                        },
                    )),
                    Some(Tailrec::Throw) => Box::new(cvs.map(move |cv| {
                        cv.and_then(|cv| {
                            Err(Exn(exn::Inner::TailCall(Box::new(exn::TailCall {
                                id: *id,
                                vars: cv.0.vars,
                                val: cv.1,
                                path: None,
                            }))))
                        })
                    })),
                }
            }
            Ast::Native(id, args) => {
                let cvs = bind_vars(args, lut, Ctx::new([], cv.0.inputs), cv, Clone::clone);
                flat_map_then(cvs, |cv| (lut.funs[*id].run)(lut, cv))
            }
            Ast::Label(id) => label_run(cv, |cv| id.run(lut, cv)),
        }
    }

    /// `f.paths(lut, (c, (v, p)))` returns the outputs and paths of `v | f` in the context `c`,
    /// where `v` is assumed to be at path `p`.
    ///
    /// In particular, `v | path(f)` in context `c` yields the same paths as
    /// `f.paths(lut, (c, (v, Default::default())))`.
    pub fn paths<'a, V: ValT>(&self, lut: &'a Lut<V>, cv: Cvp<'a, V>) -> ValPathXs<'a, V> {
        let err = |v| box_once(Err(Exn::from(Error::path_expr(v))));
        let proj_cv = |cv: &Cvp<'a, V>| (cv.0.clone(), cv.1 .0.clone());
        let proj_val = |(val, _path): &(V, _)| val.clone();
        match &lut.terms[self.0] {
            Ast::ToString => err(cv.1 .0),
            Ast::Int(_) | Ast::Num(_) | Ast::Str(_) => err(cv.1 .0),
            Ast::Arr(_) | Ast::ObjEmpty | Ast::ObjSingle(..) => err(cv.1 .0),
            Ast::Neg(_) | Ast::Logic(..) | Ast::Math(..) | Ast::Cmp(..) => err(cv.1 .0),
            Ast::Update(..) | Ast::Assign(..) => err(cv.1 .0),
            Ast::UpdateMath(..) | Ast::UpdateAlt(..) => err(cv.1 .0),
            Ast::Id => box_once(Ok(cv.1)),
            Ast::Recurse => recurse_run(cv.1, &|(v, p)| {
                v.key_values()
                    .map(move |r| r.map(|(k, v_)| (v_, p.clone().cons(k))))
            }),
            Ast::Pipe(l, None, r) => {
                flat_map_then_with(l.paths(lut, (cv.0.clone(), cv.1)), cv.0, move |y, ctx| {
                    r.paths(lut, (ctx, y))
                })
            }
            Ast::Pipe(l, Some(pat), r) => {
                flat_map_then_with(l.run(lut, proj_cv(&cv)), cv, move |y, cv| {
                    bind_run(pat, r, lut, cv, y, |f, lut, cv| f.paths(lut, cv))
                })
            }
            Ast::Comma(l, r) => Box::new(l.paths(lut, cv.clone()).chain(lazy(|| r.paths(lut, cv)))),
            Ast::Alt(l, r) => {
                let any_true = l
                    .run(lut, proj_cv(&cv))
                    .any(|v| v.as_ref().map_or(true, ValT::as_bool));
                if any_true { l } else { r }.paths(lut, cv)
            }
            Ast::Ite(if_, then_, else_) => {
                flat_map_then_with(if_.run(lut, proj_cv(&cv)), cv, move |v, cv| {
                    if v.as_bool() { then_ } else { else_ }.paths(lut, cv)
                })
            }
            Ast::TryCatch(f, c) => try_catch_run(f.paths(lut, (cv.0.clone(), cv.1)), move |e| {
                c.run(lut, (cv.0.clone(), e.into_val()))
                    .map(|e| Err(Exn::from(Error::path_expr(e?))))
            }),
            Ast::Path(f, path) => {
                let path = path.map_ref(|i| {
                    let cv = (cv.0.clone(), cv.1 .0.clone());
                    crate::into_iter::collect_if_once(move || i.run(lut, cv))
                });
                flat_map_then_with(f.paths(lut, cv), path, |y, path| {
                    flat_map_then_with(path.explode(), y, |path, y| {
                        Box::new(path.paths(y).map(|r| r.map_err(Exn::from)))
                    })
                })
            }
            Ast::Var(v) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(_) => err(cv.1 .0),
                Bind::Fun(l) => l.0.paths(lut, (cv.0.with_vars(l.1.clone()), cv.1)),
                Bind::Label(l) => box_once(Err(Exn(exn::Inner::Break(*l)))),
            },
            Ast::Fold(xs, pat, init, update, fold_type) => {
                let xs = rc_lazy_list::List::from_iter(run_and_bind(xs, lut, proj_cv(&cv), pat));
                fold_run(xs, cv, init, update, fold_type, |f, cv| f.paths(lut, cv))
            }
            Ast::CallDef(id, args, skip, tailrec) => {
                use core::ops::ControlFlow;
                let with_vars = move |vars| Ctx {
                    vars,
                    labels: cv.0.labels,
                    inputs: cv.0.inputs,
                };
                let cvs = bind_vars(args, lut, cv.0.clone().skip_vars(*skip), cv, proj_val);
                match tailrec {
                    None => flat_map_then(cvs, |cv| id.paths(lut, cv)),
                    Some(Tailrec::Catch) => Box::new(crate::Stack::new(
                        [flat_map_then(cvs, |cv| id.paths(lut, cv))].into(),
                        move |r| match r {
                            Err(Exn(exn::Inner::TailCall(tc))) if tc.id == *id => {
                                ControlFlow::Continue(
                                    id.paths(lut, (with_vars(tc.vars), (tc.val, tc.path.unwrap()))),
                                )
                            }
                            Ok(_) | Err(_) => ControlFlow::Break(r),
                        },
                    )),
                    Some(Tailrec::Throw) => Box::new(cvs.map(move |cv| {
                        cv.and_then(|cv| {
                            Err(Exn(exn::Inner::TailCall(Box::new(exn::TailCall {
                                id: *id,
                                vars: cv.0.vars,
                                val: cv.1 .0,
                                path: Some(cv.1 .1),
                            }))))
                        })
                    })),
                }
            }
            Ast::Label(id) => label_run(cv, |cv| id.paths(lut, cv)),
            Ast::Native(id, args) => {
                let cvs = bind_vars(args, lut, Ctx::new([], cv.0.inputs), cv, proj_val);
                flat_map_then(cvs, |cv| (lut.funs[*id].paths)(lut, cv))
            }
        }
    }

    /// `p.update(lut, (c, v), f)` returns the output of `v | p |= f` in the context `c`.
    pub fn update<'a, V: ValT>(
        &self,
        lut: &'a Lut<V>,
        cv: Cv<'a, V>,
        f: BoxUpdate<'a, V>,
    ) -> ValXs<'a, V> {
        let err = |v| box_once(Err(Exn::from(Error::path_expr(v))));
        match &lut.terms[self.0] {
            Ast::ToString => err(cv.1),
            Ast::Int(_) | Ast::Num(_) | Ast::Str(_) => err(cv.1),
            Ast::Arr(_) | Ast::ObjEmpty | Ast::ObjSingle(..) => err(cv.1),
            Ast::Neg(_) | Ast::Logic(..) | Ast::Math(..) | Ast::Cmp(..) => err(cv.1),
            Ast::Update(..) | Ast::Assign(..) => err(cv.1),
            Ast::UpdateMath(..) | Ast::UpdateAlt(..) => err(cv.1),
            // jq implements updates on `try ... catch` and `label`, but
            // I do not see how to implement this in jaq
            Ast::TryCatch(..) | Ast::Label(..) => err(cv.1),

            Ast::Id => f(cv.1),
            Ast::Recurse => recurse_update(cv.1, &f),
            Ast::Path(l, path) => {
                let path = path.map_ref(|i| {
                    let cv = cv.clone();
                    crate::into_iter::collect_if_once(move || i.run(lut, cv))
                });
                let f = move |v| {
                    let mut paths = path.clone().explode();
                    box_once(paths.try_fold(v, |acc, path| path?.update(acc, &f)))
                };
                l.update(lut, cv, Box::new(f))
            }
            Ast::Pipe(l, None, r) => l.update(
                lut,
                (cv.0.clone(), cv.1),
                Box::new(move |v| r.update(lut, (cv.0.clone(), v), f.clone())),
            ),
            Ast::Pipe(l, Some(pat), r) => reduce(
                run_and_bind(l, lut, (cv.0, cv.1.clone()), pat),
                cv.1,
                move |ctx, v| r.update(lut, (ctx, v), f.clone()),
            ),
            Ast::Comma(l, r) => flat_map_then_with(
                l.update(lut, (cv.0.clone(), cv.1), f.clone()),
                (cv.0, f),
                move |v, (ctx, f)| r.update(lut, (ctx, v), f),
            ),
            Ast::Ite(if_, then_, else_) => reduce(if_.run(lut, cv.clone()), cv.1, move |x, v| {
                if x.as_bool() { then_ } else { else_ }.update(lut, (cv.0.clone(), v), f.clone())
            }),
            Ast::Alt(l, r) => {
                let some_true = l
                    .run(lut, cv.clone())
                    .any(|y| y.map_or(true, |y| y.as_bool()));
                if some_true { l } else { r }.update(lut, cv, f)
            }
            Ast::Fold(xs, pat, init, update, fold_type) => {
                let xs = rc_lazy_list::List::from_iter(run_and_bind(xs, lut, cv.clone(), pat));
                let rec = move |v| fold_update(lut, fold_type, update, v, xs.clone(), f.clone());
                init.update(lut, cv, Box::new(rec))
            }
            Ast::Var(v) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(_) => err(cv.1),
                Bind::Fun(l) => l.0.update(lut, (cv.0.with_vars(l.1.clone()), cv.1), f),
                Bind::Label(l) => box_once(Err(Exn(exn::Inner::Break(*l)))),
            },
            Ast::CallDef(id, args, skip, _tailrec) => {
                let init = cv.1.clone();
                let cvs = bind_vars(args, lut, cv.0.clone().skip_vars(*skip), cv, Clone::clone);
                reduce(cvs, init, move |cv, v| id.update(lut, (cv.0, v), f.clone()))
            }
            Ast::Native(id, args) => {
                let init = cv.1.clone();
                let cvs = bind_vars(args, lut, Ctx::new([], cv.0.inputs), cv, Clone::clone);
                reduce(cvs, init, move |cv, v| {
                    (lut.funs[*id].update)(lut, (cv.0, v), f.clone())
                })
            }
        }
    }
}
