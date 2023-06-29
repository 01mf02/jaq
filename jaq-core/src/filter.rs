pub use crate::native::Native;
use crate::path::{self, Path};
use crate::results::{box_once, fold, recurse, then};
use crate::val::{Val, ValR, ValRs};
use crate::{rc_lazy_list, Ctx, Error};
use alloc::{boxed::Box, string::String, vec::Vec};
use dyn_clone::DynClone;
use jaq_parse::filter::FoldType;
use jaq_parse::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Clone, Debug, Default)]
pub enum Filter {
    #[default]
    Id,
    Int(isize),
    Float(f64),
    Str(String),
    Array(Option<Box<Self>>),
    Object(Vec<(Self, Self)>),

    Try(Box<Self>),
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

    Native(Native, Vec<Filter>),
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

pub type Cv<'c> = (Ctx<'c>, Val);

/*
pub struct Owned(Filter);
*/

#[derive(Copy, Clone)]
pub struct Ref<'a>(pub &'a Filter);

impl<'a> Ref<'a> {
    pub fn run(self, cv: Cv<'a>) -> ValRs<'a> {
        self.0.run(cv)
    }

    pub fn update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        self.0.update(cv, f)
    }
}

/*
#[derive(Copy, Clone)]
pub struct ArgsRef<'a>(&'a [Filter]);

impl<'a> ArgsRef<'a> {
    pub fn get(self, i: usize) -> Ref<'a> {
        Ref(&self.0[i])
    }
}
*/

impl Filter {
    pub fn run<'a>(&'a self, cv: Cv<'a>) -> ValRs<'a> {
        use core::iter::once;
        use itertools::Itertools;
        match self {
            Self::Id => box_once(Ok(cv.1)),
            Self::Int(n) => box_once(Ok(Val::Int(*n))),
            Self::Float(x) => box_once(Ok(Val::Float(*x))),
            Self::Str(s) => box_once(Ok(Val::str(s.clone()))),
            Self::Array(None) => box_once(Ok(Val::Arr(Default::default()))),
            Self::Array(Some(f)) => box_once(f.run(cv).collect::<Result<_, _>>().map(Val::arr)),
            Self::Object(o) if o.is_empty() => box_once(Ok(Val::Obj(Default::default()))),
            Self::Object(o) => Box::new(
                o.iter()
                    .map(|(kf, vf)| Ref(kf).cartesian(Ref(vf), cv.clone()).collect::<Vec<_>>())
                    .multi_cartesian_product()
                    .map(|kvs| {
                        kvs.into_iter()
                            .map(|(k, v)| Ok((k?.to_str()?, v?)))
                            .collect::<Result<_, _>>()
                            .map(Val::obj)
                    }),
            ),
            Self::Try(f) => Box::new(f.run(cv).filter(|y| y.is_ok())),
            Self::Neg(f) => Box::new(f.run(cv).map(|v| -v?)),

            // `l | r`
            Self::Pipe(l, false, r) => Box::new(
                l.run((cv.0.clone(), cv.1))
                    .flat_map(move |y| then(y, |y| r.run((cv.0.clone(), y)))),
            ),
            // `l as $x | r`
            Self::Pipe(l, true, r) => Ref(l).pipe(cv, |cv, y| r.run((cv.0.cons_var(y), cv.1))),

            Self::Comma(l, r) => Box::new(l.run(cv.clone()).chain(r.run(cv))),
            Self::Alt(l, r) => {
                let mut l = l
                    .run(cv.clone())
                    .filter(|v| v.as_ref().map_or(true, |v| v.as_bool()));
                match l.next() {
                    Some(head) => Box::new(once(head).chain(l)),
                    None => r.run(cv),
                }
            }
            Self::Ite(if_, then_, else_) => Box::new(if_.run(cv.clone()).flat_map(move |v| {
                then(v, |v| {
                    if v.as_bool() { then_ } else { else_ }.run(cv.clone())
                })
            })),
            Self::Path(f, path) => path.run(cv, f),
            Self::Update(path, f) => path.update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| f.run((cv.0.clone(), v))),
            ),
            Self::UpdateMath(path, op, f) => Ref(f).pipe(cv, move |cv, y| {
                path.update(cv, Box::new(move |x| box_once(op.run(x, y.clone()))))
            }),
            Self::Assign(path, f) => Ref(f).pipe(cv, |cv, y| {
                path.update(cv, Box::new(move |_| box_once(Ok(y.clone()))))
            }),
            Self::Logic(l, stop, r) => Box::new(l.run(cv.clone()).flat_map(move |l| {
                then(l, |l| {
                    if l.as_bool() == *stop {
                        box_once(Ok(Val::Bool(*stop)))
                    } else {
                        Box::new(r.run(cv.clone()).map(|r| Ok(Val::Bool(r?.as_bool()))))
                    }
                })
            })),
            Self::Math(l, op, r) => {
                let prod = Ref(l).cartesian(Ref(r), cv);
                Box::new(prod.map(|(x, y)| op.run(x?, y?)))
            }
            Self::Ord(l, op, r) => {
                let prod = Ref(l).cartesian(Ref(r), cv);
                Box::new(prod.map(|(x, y)| Ok(Val::Bool(op.run(&x?, &y?)))))
            }

            Self::Fold(typ, xs, init, f) => {
                let xs = rc_lazy_list::List::from_iter(xs.run(cv.clone()));
                let init = init.run(cv.clone());
                let f = move |x, v| f.run((cv.0.clone().cons_var(x), v));
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
            Self::Recurse(f) => Ref(f).recurse1(true, true, cv),

            Self::Var(v) => box_once(Ok(cv.0.vars.get(*v).unwrap().clone())),
            Self::Call { skip, id } => Box::new(crate::LazyIter::new(move || {
                let (save, rec) = &cv.0.recs[*id];
                //std::dbg!(save, skip, &cv.0.vars, &cv.0.clone().save_skip_vars(*save, *skip).vars);
                rec.run((cv.0.save_skip_vars(*save, *skip), cv.1))
            })),

            Self::Native(Native { run, .. }, args) => (run)(args, cv),
        }
    }

    /// `p.update(cv, f)` returns the output of `v | p |= f`
    fn update<'a>(&'a self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs {
        let err = box_once(Err(Error::PathExp));
        match self {
            Self::Int(_) | Self::Float(_) | Self::Str(_) => err,
            Self::Array(_) | Self::Object(_) => err,
            Self::Neg(_) | Self::Logic(..) | Self::Math(..) | Self::Ord(..) => err,
            Self::Update(..) | Self::UpdateMath(..) | Self::Assign(..) => err,

            // these are up for grabs to implement :)
            Self::Try(_) | Self::Alt(..) => todo!(),
            Self::Fold(..) => todo!(),

            Self::Id => f(cv.1),
            Self::Path(l, path) => l.update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| path.update((cv.0.clone(), v), |v| f(v))),
            ),
            Self::Pipe(l, false, r) => l.update(
                (cv.0.clone(), cv.1),
                Box::new(move |v| r.update((cv.0.clone(), v), f.clone())),
            ),
            Self::Pipe(l, true, r) => reduce(l.run(cv.clone()), cv.1, move |x, v| {
                r.update((cv.0.clone().cons_var(x), v), f.clone())
            }),
            Self::Comma(l, r) => {
                let l = l.update((cv.0.clone(), cv.1), f.clone());
                Box::new(l.flat_map(move |v| then(v, |v| r.update((cv.0.clone(), v), f.clone()))))
            }
            Self::Ite(if_, then_, else_) => reduce(if_.run(cv.clone()), cv.1, move |x, v| {
                if x.as_bool() { then_ } else { else_ }.update((cv.0.clone(), v), f.clone())
            }),
            Self::Recurse(l) => Ref(l).recurse_update(cv, f),

            Self::Var(_) => err,
            Self::Call { skip, id } => {
                let (save, rec) = &cv.0.recs[*id];
                rec.update((cv.0.save_skip_vars(*save, *skip), cv.1), f)
            }

            Self::Native(Native { update, .. }, args) => (update)(args, cv, f),
        }
    }

    /// `..`, also known as `recurse`, is defined as `recurse(.[]?)`
    pub fn recurse() -> Self {
        // `[]?`
        let path = (path::Part::Range(None, None), path::Opt::Optional);
        // `.[]?`
        let path = Filter::Path(Box::new(Filter::Id), Path(Vec::from([path])));
        Filter::Recurse(Box::new(path))
    }
}

impl<'a> Ref<'a> {
    /// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
    ///
    /// This has a special optimisation for the case where only a single `v` is returned.
    /// In that case, we can consume `cv` instead of cloning it.
    pub fn pipe(self, cv: Cv<'a>, f: impl Fn(Cv<'a>, Val) -> ValRs<'a> + 'a) -> ValRs<'a> {
        let mut l = self.run(cv.clone());

        // if we expect at most one element from the left side,
        // we do not need to clone the input value to run the right side;
        // this can have a significant impact on performance!
        if l.size_hint().1 == Some(1) {
            if let Some(y) = l.next() {
                // the Rust documentation states that
                // "a buggy iterator may yield [..] more than the upper bound of elements",
                // but so far, it seems that all iterators here are not buggy :)
                assert!(l.next().is_none());
                return then(y, |y| f(cv, y));
            }
        };
        Box::new(l.flat_map(move |y| then(y, |y| f(cv.clone(), y))))
    }

    /// Run `self` and `r` and return the cartesian product of their outputs.
    pub fn cartesian(self, r: Self, cv: Cv<'a>) -> impl Iterator<Item = (ValR, ValR)> + 'a {
        let l = self.run(cv.clone());
        let r: Vec<_> = r.run(cv).collect();
        if r.len() == 1 {
            // this special case is to avoid cloning the left-hand side,
            // which massively improves performance of filters like `add`
            Box::new(l.map(move |l| (l, r[0].clone())))
        } else {
            use itertools::Itertools;
            Box::new(l.into_iter().cartesian_product(r)) as Box<dyn Iterator<Item = _>>
        }
    }

    pub fn recurse1(self, inner: bool, outer: bool, cv: Cv<'a>) -> ValRs {
        let f = move |v| self.run((cv.0.clone(), v));
        Box::new(recurse(inner, outer, box_once(Ok(cv.1)), f))
    }

    /// Return the output of `recurse(l) |= f`.
    pub fn recurse_update(self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs<'a> {
        // implemented by the expansion of `def recurse(l): ., (l | recurse(l))`
        Box::new(f(cv.1).flat_map(move |v| {
            then(v, |v| {
                let (c, f) = (cv.0.clone(), f.clone());
                let rec = move |v| self.recurse_update((c.clone(), v), f.clone());
                self.update((cv.0.clone(), v), Box::new(rec))
            })
        }))
    }

    pub fn regex(self, flags: Self, s: bool, m: bool, cv: Cv<'a>) -> ValRs {
        let flags_re = flags.cartesian(self, (cv.0, cv.1.clone()));
        Box::new(flags_re.map(move |(flags, re)| Ok(Val::arr(cv.1.regex(&re?, &flags?, (s, m))?))))
    }
}

impl Path<Filter> {
    fn update<'a: 'f, 'f, F>(&'a self, cv: Cv<'a>, f: F) -> ValRs<'f>
    where
        F: Fn(Val) -> ValRs<'f> + Copy,
    {
        let path = self.0.iter().map(|(p, opt)| Ok((p.idx(cv.clone())?, *opt)));
        then(path.collect(), |path: Vec<_>| {
            path::Part::update(path.iter(), cv.1, f)
        })
    }

    fn run<'a>(&'a self, cv: Cv<'a>, init: &'a Filter) -> ValRs {
        let path = self.0.iter().map(|(p, opt)| Ok((p.idx(cv.clone())?, *opt)));
        let path = match path.collect::<Result<Vec<_>, _>>() {
            Ok(path) => path,
            Err(e) => return box_once(Err(e)),
        };
        let outs = init.run(cv).map(move |i| {
            path.iter().try_fold(Vec::from([i?]), |acc, (part, opt)| {
                opt.collect(acc.into_iter().flat_map(|x| part.collect(x)))
            })
        });
        Box::new(outs.flat_map(|vals| then(vals, |vals| Box::new(vals.into_iter().map(Ok)))))
    }
}

impl path::Part<Filter> {
    fn idx<'a>(&'a self, cv: Cv<'a>) -> Result<path::Part<Vec<Val>>, Error> {
        use path::Part::*;
        match self {
            Index(i) => Ok(Index(i.run(cv).collect::<Result<_, _>>()?)),
            Range(from, until) => {
                let from = from.as_ref().map(|f| f.run(cv.clone()).collect());
                let until = until.as_ref().map(|u| u.run(cv).collect());
                Ok(Range(from.transpose()?, until.transpose()?))
            }
        }
    }
}
