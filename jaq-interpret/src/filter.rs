use crate::box_iter::{box_once, flat_map_with};
use crate::path::{self, Path};
use crate::results::{fold, recurse, then, Results};
use crate::val::{Val, ValR, ValRs};
use crate::{rc_lazy_list, Ctx, Error};
use alloc::{boxed::Box, string::String, vec::Vec};
use dyn_clone::DynClone;
use jaq_syn::filter::FoldType;
use jaq_syn::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Debug, Default, Clone)]
pub struct Owned(Ast, Vec<(usize, Ast)>);

#[derive(Debug, Copy, Clone)]
pub struct Ref<'a>(&'a Ast, &'a [(usize, Ast)]);

impl Owned {
    pub(crate) fn new(main: Ast, recs: Vec<(usize, Ast)>) -> Self {
        Self(main, recs)
    }
}

/// Function from a value to a stream of value results.
#[derive(Clone, Debug, Default)]
pub enum Ast {
    #[default]
    Id,
    ToString,

    Int(isize),
    Float(f64),
    Str(String),
    Array(Box<Self>),
    Object(Vec<(Self, Self)>),

    Try(Box<Self>, Box<Self>),
    Neg(Box<Self>),
    Pipe(Box<Self>, bool, Box<Self>),
    Comma(Box<Self>, Box<Self>),
    Alt(Box<Self>, Box<Self>),
    Ite(Box<Self>, Box<Self>, Box<Self>),
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
    Fold(FoldType, Box<Self>, Box<Self>, Box<Self>),

    Path(Box<Self>, Path<Self>),

    Update(Box<Self>, Box<Self>),
    UpdateMath(Box<Self>, MathOp, Box<Self>),
    Assign(Box<Self>, Box<Self>),

    Logic(Box<Self>, bool, Box<Self>),
    Math(Box<Self>, MathOp, Box<Self>),
    Ord(Box<Self>, OrdOp, Box<Self>),

    Recurse(Box<Self>),

    Var(usize),
    Call {
        skip: usize,
        id: usize,
    },

    Native(Native, Vec<Self>),
}

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
pub trait Update<'a>: Fn(Val) -> ValRs<'a> + DynClone {}

impl<'a, T: Fn(Val) -> ValRs<'a> + Clone> Update<'a> for T {}

dyn_clone::clone_trait_object!(<'a> Update<'a>);

fn reduce<'a>(xs: ValRs<'a>, init: Val, f: impl Fn(Val, Val) -> ValRs<'a> + 'a) -> ValRs {
    let xs = rc_lazy_list::List::from_iter(xs);
    Box::new(fold(false, xs, box_once(Ok(init)), f))
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
pub struct Args<'a>(&'a [Ast], &'a [(usize, Ast)]);

impl<'a> Args<'a> {
    /// Obtain the n-th argument passed to the filter, crash if it is not given.
    // This function returns an `impl` in order not to expose the `Filter` type publicly.
    // It would be more elegant to implement `Index<usize>` here instead,
    // but because of returning `impl`, we cannot do this right now, see:
    // <https://github.com/rust-lang/rust/issues/63063>.
    pub fn get(self, i: usize) -> impl FilterT<'a> {
        Ref(&self.0[i], self.1)
    }
}

impl<'a> FilterT<'a> for &'a Owned {
    fn run(self, cv: Cv<'a>) -> ValRs<'a> {
        Ref(&self.0, &self.1).run(cv)
    }

    fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        Ref(&self.0, &self.1).update(cv, f)
    }
}

impl<'a> FilterT<'a> for Ref<'a> {
    fn run(self, cv: Cv<'a>) -> ValRs<'a> {
        use core::iter::once;
        use itertools::Itertools;
        // wrap a filter AST with the filter definitions
        let w = move |f: &'a Ast| Ref(f, self.1);
        match &self.0 {
            Ast::Id => box_once(Ok(cv.1)),
            Ast::ToString => box_once(Ok(Val::str(cv.1.to_string_or_clone()))),
            Ast::Int(n) => box_once(Ok(Val::Int(*n))),
            Ast::Float(x) => box_once(Ok(Val::Float(*x))),
            Ast::Str(s) => box_once(Ok(Val::str(s.clone()))),
            Ast::Array(f) => box_once(w(f).run(cv).collect::<Result<_, _>>().map(Val::arr)),
            Ast::Object(o) if o.is_empty() => box_once(Ok(Val::Obj(Default::default()))),
            Ast::Object(o) => Box::new(
                o.iter()
                    .map(|(k, v)| Self::cartesian(w(k), w(v), cv.clone()).collect::<Vec<_>>())
                    .multi_cartesian_product()
                    .map(|kvs| {
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
            Ast::Pipe(l, false, r) => Box::new(
                w(l).run((cv.0.clone(), cv.1))
                    .flat_map(move |y| then(y, |y| w(r).run((cv.0.clone(), y)))),
            ),
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
                match typ {
                    FoldType::Reduce => Box::new(fold(false, xs, init, f)),
                    FoldType::For => Box::new(fold(true, xs, init, f)),
                    FoldType::Foreach => {
                        Box::new(init.flat_map(move |i| {
                            fold(true, xs.clone(), box_once(i), f.clone()).skip(1)
                        }))
                    }
                }
            }
            Ast::Recurse(f) => w(f).recurse(true, true, cv),

            Ast::Var(v) => box_once(Ok(cv.0.vars.get(*v).unwrap().clone())),
            Ast::Call { skip, id } => Box::new(crate::LazyIter::new(move || {
                let (save, rec) = &self.1[*id];
                //std::dbg!(save, skip, &cv.0.vars, &cv.0.clone().save_skip_vars(*save, *skip).vars);
                w(rec).run((cv.0.save_skip_vars(*save, *skip), cv.1))
            })),

            Ast::Native(Native { run, .. }, args) => (run)(Args(args, self.1), cv),
        }
    }

    fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        let err = box_once(Err(Error::PathExp));
        let w = move |f: &'a Ast| Ref(f, self.1);
        match self.0 {
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
            Ast::Recurse(l) => w(l).recurse_update(cv, f),

            Ast::Var(_) => err,
            Ast::Call { skip, id } => {
                let (save, rec) = &self.1[*id];
                w(rec).update((cv.0.save_skip_vars(*save, *skip), cv.1), f)
            }

            Ast::Native(Native { update, .. }, args) => (update)(Args(args, self.1), cv, f),
        }
    }
}

impl Ast {
    /// `..`, also known as `recurse/0`, is defined as `recurse(.[]?)`
    pub fn recurse0() -> Self {
        // `[]?`
        let path = (path::Part::Range(None, None), path::Opt::Optional);
        // `.[]?`
        let path = Ast::Path(Box::new(Ast::Id), Path(Vec::from([path])));
        Ast::Recurse(Box::new(path))
    }

    /// `{}[]` returns zero values.
    pub fn empty() -> Self {
        // `{}`
        let obj = Ast::Object(Vec::new());
        // `[]`
        let path = (path::Part::Range(None, None), path::Opt::Essential);
        Ast::Path(Box::new(obj), Path(Vec::from([path])))
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
        let l = self.run(cv.clone());
        let r: Vec<_> = r.run(cv).collect();
        if r.len() == 1 {
            // this special case is to avoid cloning the left-hand side,
            // which massively improves performance of filters like `add`
            Box::new(l.map(move |l| (l, r[0].clone())))
        } else {
            use itertools::Itertools;
            Box::new(l.cartesian_product(r)) as Box<dyn Iterator<Item = _>>
        }
    }

    /// Return the output of `recurse(f)` if `inner` and `outer` are true.
    ///
    /// This function implements a generalisation of `recurse(f)`:
    /// if `inner` is true, it returns values for which `f` yields at least one output, and
    /// if `outer` is true, it returns values for which `f` yields no output.
    /// This is useful to implement `while` and `until`.
    fn recurse(self, inner: bool, outer: bool, cv: Cv<'a>) -> ValRs {
        let f = move |v| self.clone().run((cv.0.clone(), v));
        Box::new(recurse(inner, outer, box_once(Ok(cv.1)), f))
    }

    /// Return the output of `recurse(l) |= f`.
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
