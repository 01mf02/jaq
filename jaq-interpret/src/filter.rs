use crate::box_iter::{box_once, flat_map_with, map_with, BoxIter};
use crate::compile::{FoldType, Lut, Tailrec, Term as Ast};
use crate::results::{fold, then, Fold, Results};
use crate::val::{ValT, ValX, ValXs};
use crate::{exn, rc_lazy_list, Bind, Ctx, Error, Exn};
use alloc::boxed::Box;
use dyn_clone::DynClone;

pub(crate) use crate::compile::TermId as Id;

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
pub trait Update<'a, V>: Fn(V) -> ValXs<'a, V> + DynClone {}

impl<'a, V, T: Fn(V) -> ValXs<'a, V> + Clone> Update<'a, V> for T {}

dyn_clone::clone_trait_object!(<'a, V> Update<'a, V>);

type BoxUpdate<'a, V> = Box<dyn Update<'a, V> + 'a>;

/// Enhance the context `ctx` with variables bound to the outputs of `args` executed on `cv`,
/// and return the enhanced contexts together with the original value of `cv`.
///
/// This is used when we call filters with variable arguments.
fn bind_vars<'a, F: FilterT>(
    args: &'a [Bind<Id>],
    lut: &'a Lut<F>,
    ctx: Ctx<'a, F::V>,
    cv: Cv<'a, F::V>,
) -> Results<'a, Cv<'a, F::V>, Exn<'a, F::V>> {
    match args.split_first() {
        Some((Bind::Var(arg), rest)) => {
            flat_map_with(arg.run(lut, cv.clone()), (ctx, cv), |y, (ctx, cv)| {
                then(y, |y| bind_vars(rest, lut, ctx.cons_var(y), cv))
            })
        }
        Some((Bind::Fun(arg), rest)) => bind_vars(rest, lut, ctx.cons_fun((arg, cv.0.clone())), cv),
        None => box_once(Ok((ctx, cv.1))),
    }
}

fn run_cvs<'a, F: FilterT>(
    f: &'a impl FilterT<F, V = F::V>,
    lut: &'a Lut<F>,
    cvs: Results<'a, Cv<'a, F::V>, Exn<'a, F::V>>,
) -> ValXs<'a, F::V> {
    Box::new(cvs.flat_map(move |cv| then(cv, |cv| f.run(lut, cv))))
}

fn reduce<'a, T, V, F>(xs: Results<'a, T, Exn<'a, V>>, init: V, f: F) -> ValXs<V>
where
    T: Clone + 'a,
    V: Clone + 'a,
    F: Fn(T, V) -> ValXs<'a, V> + 'a,
{
    let xs = rc_lazy_list::List::from_iter(xs);
    Box::new(fold(false, xs, Fold::Input(init), f))
}

fn label_skip<'a, V: 'a>(ys: ValXs<'a, V>, skip: usize) -> ValXs<'a, V> {
    if skip == 0 {
        return ys;
    }
    Box::new(ys.map(move |y| match y {
        Err(Exn(exn::Inner::Break(n))) => Err(Exn(exn::Inner::Break(n + skip))),
        y => y,
    }))
}

/// Combination of context and input value.
pub type Cv<'c, V> = (Ctx<'c, V>, V);

/// A filter which is implemented using function pointers.
#[derive(Clone)]
pub struct Native<V> {
    run: RunPtr<V>,
    update: UpdatePtr<V>,
}

/// Run function pointer.
///
/// Implementation-wise, this would be a perfect spot for `for<'a, F: FilterT<V>>`;
/// unfortunately, this is [not stable yet](https://github.com/rust-lang/rust/issues/108185).
/// That would also allow to eliminate `F` from `FilterT`.
pub type RunPtr<V, F = Native<V>> = for<'a> fn(&'a Lut<F>, Cv<'a, V>) -> ValXs<'a, V>;
/// Update function pointer.
pub type UpdatePtr<V, F = Native<V>> =
    for<'a> fn(&'a Lut<F>, Cv<'a, V>, BoxUpdate<'a, V>) -> ValXs<'a, V>;

impl<V> Native<V> {
    /// Create a native filter from a run function, without support for updates.
    pub const fn new(run: RunPtr<V, Self>) -> Self {
        Self {
            run,
            update: |_, _, _| box_once(Err(Exn::from(Error::PathExp))),
        }
    }

    /// Specify an update function (used for `filter |= ...`).
    pub const fn with_update(self, update: UpdatePtr<V, Self>) -> Self {
        Self { update, ..self }
    }
}

impl<V: ValT> FilterT for Native<V> {
    type V = V;

    fn run<'a>(&'a self, lut: &'a Lut<Self>, cv: Cv<'a, V>) -> ValXs<'a, V> {
        (self.run)(lut, cv)
    }

    fn update<'a>(
        &'a self,
        lut: &'a Lut<Self>,
        cv: Cv<'a, V>,
        f: BoxUpdate<'a, V>,
    ) -> ValXs<'a, V> {
        (self.update)(lut, cv, f)
    }
}

impl<F: FilterT<F>> FilterT<F> for Id {
    type V = F::V;

    fn run<'a>(&'a self, lut: &'a Lut<F>, cv: Cv<'a, Self::V>) -> ValXs<'a, Self::V> {
        use alloc::string::ToString;
        use core::iter::{once, once_with};
        match &lut.terms[self.0] {
            Ast::Id => box_once(Ok(cv.1)),
            Ast::ToString => Box::new(once_with(move || match cv.1.as_str() {
                Some(_) => Ok(cv.1),
                None => Ok(Self::V::from(cv.1.to_string())),
            })),
            Ast::Int(n) => box_once(Ok(Self::V::from(*n))),
            Ast::Num(x) => box_once(Self::V::from_num(x).map_err(Exn::from)),
            Ast::Str(s) => Box::new(once_with(move || Ok(Self::V::from(s.clone())))),
            Ast::Arr(f) => Box::new(once_with(move || f.run(lut, cv).collect())),
            Ast::ObjEmpty => box_once(Self::V::from_map([]).map_err(Exn::from)),
            Ast::ObjSingle(k, v) => Box::new(
                Self::cartesian(k, v, lut, cv).map(|(k, v)| Ok(Self::V::from_map([(k?, v?)])?)),
            ),
            // TODO: write test for `try (break $x)`
            Ast::TryCatch(f, c) => Box::new(f.run(lut, (cv.0.clone(), cv.1)).flat_map(move |y| {
                y.map_or_else(
                    |e| match e.get_err() {
                        Ok(err) => c.run(lut, (cv.0.clone(), err.as_val())),
                        Err(exn) => box_once(Err(exn)),
                    },
                    |v| box_once(Ok(v)),
                )
            })),
            Ast::Neg(f) => Box::new(f.run(lut, cv).map(|v| Ok((-v?)?))),

            // `l | r`
            Ast::Pipe(l, false, r) => {
                let l = l.run(lut, (cv.0.clone(), cv.1));
                flat_map_with(l, cv.0, move |y, ctx| then(y, |y| r.run(lut, (ctx, y))))
            }
            // `l as $x | r`
            Ast::Pipe(l, true, r) => {
                l.pipe(lut, cv, move |cv, y| r.run(lut, (cv.0.cons_var(y), cv.1)))
            }

            Ast::Comma(l, r) => Box::new(l.run(lut, cv.clone()).chain(r.run(lut, cv))),
            Ast::Alt(l, r) => {
                let mut l = l
                    .run(lut, cv.clone())
                    .filter(|v| v.as_ref().map_or(true, ValT::as_bool));
                match l.next() {
                    Some(head) => Box::new(once(head).chain(l)),
                    None => r.run(lut, cv),
                }
            }
            Ast::Ite(if_, then_, else_) => if_.pipe(lut, cv, move |cv, v| {
                if v.as_bool() { then_ } else { else_ }.run(lut, cv)
            }),
            Ast::Path(f, path) => {
                let path = path.map_ref(|i| {
                    let cv = cv.clone();
                    crate::into_iter::collect_if_once(move || i.run(lut, cv))
                });
                flat_map_with(f.run(lut, cv), path, |y, path| {
                    then(y, |y| {
                        flat_map_with(path.explode(), y, |path, y| {
                            then(path, |path| {
                                Box::new(path.run(y).map(|r| r.map_err(Exn::from)))
                            })
                        })
                    })
                })
            }

            Ast::Update(path, f) => path.update(
                lut,
                (cv.0.clone(), cv.1),
                Box::new(move |v| f.run(lut, (cv.0.clone(), v))),
            ),
            Ast::UpdateMath(path, op, f) => f.pipe(lut, cv, move |cv, y| {
                path.update(
                    lut,
                    cv,
                    Box::new(move |x| box_once(op.run(x, y.clone()).map_err(Exn::from))),
                )
            }),
            Ast::UpdateAlt(path, f) => f.pipe(lut, cv, move |cv, y| {
                path.update(
                    lut,
                    cv,
                    Box::new(move |x| box_once(Ok(if x.as_bool() { x } else { y.clone() }))),
                )
            }),
            Ast::Assign(path, f) => f.pipe(lut, cv, move |cv, y| {
                path.update(lut, cv, Box::new(move |_| box_once(Ok(y.clone()))))
            }),

            Ast::Logic(l, stop, r) => l.pipe(lut, cv, move |cv, l| {
                if l.as_bool() == *stop {
                    box_once(Ok(Self::V::from(*stop)))
                } else {
                    Box::new(r.run(lut, cv).map(|r| Ok(Self::V::from(r?.as_bool()))))
                }
            }),
            Ast::Math(l, op, r) => {
                Box::new(Self::cartesian(l, r, lut, cv).map(|(x, y)| Ok(op.run(x?, y?)?)))
            }
            Ast::Ord(l, op, r) => Box::new(
                Self::cartesian(l, r, lut, cv).map(|(x, y)| Ok(Self::V::from(op.run(&x?, &y?)))),
            ),

            Ast::Fold(typ, xs, init, f) => {
                use Fold::{Input, Output};
                let xs = rc_lazy_list::List::from_iter(xs.run(lut, cv.clone()));
                let init = init.run(lut, cv.clone());
                let f = move |x, v| f.run(lut, (cv.0.clone().cons_var(x), v));
                match typ {
                    FoldType::Reduce => Box::new(fold(false, xs, Output(init), f)),
                    FoldType::For => Box::new(fold(true, xs, Output(init), f)),
                    FoldType::Foreach => flat_map_with(init, xs, move |i, xs| {
                        then(i, |i| Box::new(fold(true, xs, Input(i), f.clone())))
                    }),
                }
            }

            Ast::Var(v, skip) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(v) => box_once(Ok(v.clone())),
                Bind::Fun((id, vars)) => {
                    label_skip(id.run(lut, (cv.0.with_vars(vars.clone()), cv.1)), *skip)
                }
            },
            Ast::CallDef(id, args, skip, tailrec) => {
                use core::ops::ControlFlow;
                let inputs = cv.0.inputs;
                let cvs = bind_vars(args, lut, cv.0.clone().skip_vars(*skip), cv);
                match tailrec {
                    None => run_cvs(id, lut, cvs),
                    Some(Tailrec::Catch) => Box::new(crate::Stack::new(
                        [run_cvs(id, lut, cvs)].into(),
                        move |r| match r {
                            Err(Exn(exn::Inner::TailCall(id_, vars, v))) if id == id_ => {
                                ControlFlow::Continue(id.run(lut, (Ctx { vars, inputs }, v)))
                            }
                            Ok(_) | Err(_) => ControlFlow::Break(r),
                        },
                    )),
                    Some(Tailrec::Throw) => Box::new(cvs.map(move |cv| {
                        cv.and_then(|cv| Err(Exn(exn::Inner::TailCall(id, cv.0.vars, cv.1))))
                    })),
                }
            }
            Ast::Native(id, args) => {
                let cvs = bind_vars(args, lut, Ctx::new([], cv.0.inputs), cv);
                run_cvs(&lut.funs[*id], lut, cvs)
            }
            Ast::Label(id) => Box::new(id.run(lut, cv).map_while(|y| match y {
                Err(Exn(exn::Inner::Break(n))) => {
                    n.checked_sub(1).map(|m| Err(Exn(exn::Inner::Break(m))))
                }
                y => Some(y),
            })),
            Ast::Break(skip) => box_once(Err(Exn(exn::Inner::Break(*skip)))),
        }
    }

    fn update<'a>(
        &'a self,
        lut: &'a Lut<F>,
        cv: Cv<'a, Self::V>,
        f: BoxUpdate<'a, Self::V>,
    ) -> ValXs<'a, Self::V> {
        let err = box_once(Err(Exn::from(Error::PathExp)));
        match &lut.terms[self.0] {
            Ast::ToString => err,
            Ast::Int(_) | Ast::Num(_) | Ast::Str(_) => err,
            Ast::Arr(_) | Ast::ObjEmpty | Ast::ObjSingle(..) => err,
            Ast::Neg(_) | Ast::Logic(..) | Ast::Math(..) | Ast::Ord(..) => err,
            Ast::Update(..) | Ast::UpdateMath(..) | Ast::UpdateAlt(..) | Ast::Assign(..) => err,

            // these are up for grabs to implement :)
            Ast::TryCatch(..) | Ast::Label(_) | Ast::Alt(..) | Ast::Fold(..) => {
                unimplemented!("updating with this operator is not supported yet")
            }

            Ast::Id => f(cv.1),
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
            Ast::Pipe(l, false, r) => l.update(
                lut,
                (cv.0.clone(), cv.1),
                Box::new(move |v| r.update(lut, (cv.0.clone(), v), f.clone())),
            ),
            Ast::Pipe(l, true, r) => reduce(l.run(lut, cv.clone()), cv.1, move |x, v| {
                r.update(lut, (cv.0.clone().cons_var(x), v), f.clone())
            }),
            Ast::Comma(l, r) => {
                let l = l.update(lut, (cv.0.clone(), cv.1), f.clone());
                Box::new(
                    l.flat_map(move |v| then(v, |v| r.update(lut, (cv.0.clone(), v), f.clone()))),
                )
            }
            Ast::Ite(if_, then_, else_) => reduce(if_.run(lut, cv.clone()), cv.1, move |x, v| {
                if x.as_bool() { then_ } else { else_ }.update(lut, (cv.0.clone(), v), f.clone())
            }),

            Ast::Var(v, skip) => match cv.0.vars.get(*v).unwrap() {
                Bind::Var(_) => err,
                Bind::Fun(l) => label_skip(
                    l.0.update(lut, (cv.0.with_vars(l.1.clone()), cv.1), f),
                    *skip,
                ),
            },
            Ast::CallDef(id, args, skip, _tailrec) => {
                let init = cv.1.clone();
                let cvs = bind_vars(args, lut, cv.0.clone().skip_vars(*skip), cv);
                reduce(cvs, init, move |cv, v| id.update(lut, (cv.0, v), f.clone()))
            }
            Ast::Native(id, args) => {
                let init = cv.1.clone();
                let cvs = bind_vars(args, lut, Ctx::new([], cv.0.inputs), cv);
                reduce(cvs, init, move |cv, v| {
                    lut.funs[*id].update(lut, (cv.0, v), f.clone())
                })
            }
            Ast::Break(skip) => box_once(Err(Exn(exn::Inner::Break(*skip)))),
        }
    }
}

/// Function from a value to a stream of value results.
///
/// `F` is the type of (natively implemented) filter functions.
pub trait FilterT<F: FilterT<F, V = Self::V> = Self> {
    /// Type of values that the filter takes and yields.
    ///
    /// This is an associated type because it is strictly determined by `F`.
    type V: ValT;

    /// `f.run((c, v))` returns the output of `v | f` in the context `c`.
    fn run<'a>(&'a self, lut: &'a Lut<F>, cv: Cv<'a, Self::V>) -> ValXs<'a, Self::V>;

    /// `p.update((c, v), f)` returns the output of `v | p |= f` in the context `c`.
    fn update<'a>(
        &'a self,
        lut: &'a Lut<F>,
        cv: Cv<'a, Self::V>,
        f: BoxUpdate<'a, Self::V>,
    ) -> ValXs<'a, Self::V>;

    /// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
    ///
    /// This has a special optimisation for the case where only a single `v` is returned.
    /// In that case, we can consume `cv` instead of cloning it.
    fn pipe<'a, T: 'a>(
        &'a self,
        lut: &'a Lut<F>,
        cv: Cv<'a, Self::V>,
        f: impl Fn(Cv<'a, Self::V>, Self::V) -> Results<'a, T, Exn<'a, Self::V>> + 'a,
    ) -> Results<'a, T, Exn<'a, Self::V>> {
        flat_map_with(self.run(lut, cv.clone()), cv, move |y, cv| {
            then(y, |y| f(cv, y))
        })
    }

    /// Run `self` and `r` and return the cartesian product of their outputs.
    fn cartesian<'a>(
        &'a self,
        r: &'a Self,
        lut: &'a Lut<F>,
        cv: Cv<'a, Self::V>,
    ) -> BoxIter<'a, (ValX<'a, Self::V>, ValX<'a, Self::V>)> {
        flat_map_with(self.run(lut, cv.clone()), cv, move |l, cv| {
            map_with(r.run(lut, cv), l, |r, l| (l, r))
        })
    }
}
