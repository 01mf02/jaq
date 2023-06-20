use crate::path::{self, Path};
use crate::results::{fold, recurse, then};
use crate::val::{Val, ValR, ValRs};
use crate::{rc_lazy_list, Ctx, Error};
use alloc::string::{String, ToString};
use alloc::{boxed::Box, vec::Vec};
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

type RunPtr = for<'a> fn(&'a [Filter], Cv<'a>) -> ValRs<'a>;
type UpdatePtr = for<'a> fn(&'a [Filter], Cv<'a>, Box<dyn Update<'a> + 'a>) -> ValRs<'a>;

fn box_once<'a, T: 'a>(x: T) -> Box<dyn Iterator<Item = T> + 'a> {
    Box::new(core::iter::once(x))
}

const CORE: [(&str, usize, RunPtr); 31] = [
    ("inputs", 0, |_, cv| {
        Box::new(cv.0.inputs.map(|r| r.map_err(Error::Parse)))
    }),
    ("length", 0, |_, cv| box_once(cv.1.len())),
    ("keys", 0, |_, cv| box_once(cv.1.keys().map(Val::arr))),
    ("floor", 0, |_, cv| box_once(cv.1.round(|f| f.floor()))),
    ("round", 0, |_, cv| box_once(cv.1.round(|f| f.round()))),
    ("ceil", 0, |_, cv| box_once(cv.1.round(|f| f.ceil()))),
    ("fromjson", 0, |_, cv| box_once(cv.1.from_json())),
    ("tojson", 0, |_, cv| {
        box_once(Ok(Val::str(cv.1.to_string())))
    }),
    ("explode", 0, |_, cv| box_once(cv.1.explode().map(Val::arr))),
    ("implode", 0, |_, cv| box_once(cv.1.implode().map(Val::str))),
    ("ascii_downcase", 0, |_, cv| {
        box_once(cv.1.mutate_str(|s| s.make_ascii_lowercase()))
    }),
    ("ascii_upcase", 0, |_, cv| {
        box_once(cv.1.mutate_str(|s| s.make_ascii_uppercase()))
    }),
    ("reverse", 0, |_, cv| {
        box_once(cv.1.mutate_arr(|a| a.reverse()))
    }),
    ("now", 0, |_, _| {
        use std::time::{SystemTime, UNIX_EPOCH};
        let duration = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| Error::SystemTime(e.to_string()));
        box_once(duration.map(|x| x.as_secs_f64()).map(Val::Float))
    }),
    ("fromdateiso8601", 0, |_, cv| {
        box_once(cv.1.from_iso8601().map(Val::Float))
    }),
    ("todateiso8601", 0, |_, cv| {
        box_once(cv.1.to_iso8601().map(Val::str))
    }),
    ("sort", 0, |_, cv| box_once(cv.1.mutate_arr(|a| a.sort()))),
    ("sort_by", 1, |args, cv| {
        box_once(cv.1.sort_by(|v| args[0].run((cv.0.clone(), v))))
    }),
    ("group_by", 1, |args, cv| {
        box_once(cv.1.group_by(|v| args[0].run((cv.0.clone(), v))))
    }),
    ("has", 1, |args, cv| {
        let keys = args[0].run(cv.clone());
        Box::new(keys.map(move |k| Ok(Val::Bool(cv.1.has(&k?)?))))
    }),
    ("contains", 1, |args, cv| {
        let vals = args[0].run(cv.clone());
        Box::new(vals.map(move |y| Ok(Val::Bool(cv.1.contains(&y?)))))
    }),
    ("split", 1, |args, cv| {
        let seps = args[0].run(cv.clone());
        Box::new(seps.map(move |sep| Ok(Val::arr(cv.1.split(&sep?)?))))
    }),
    ("matches", 2, |args, cv| {
        args[0].regex(&args[1], false, true, cv)
    }),
    ("split_matches", 2, |args, cv| {
        args[0].regex(&args[1], true, true, cv)
    }),
    ("split_", 2, |args, cv| {
        args[0].regex(&args[1], true, false, cv)
    }),
    ("first", 1, |args, cv| Box::new(args[0].run(cv).take(1))),
    ("last", 1, |args, cv| {
        then(args[0].run(cv).try_fold(None, |_, x| Ok(Some(x?))), |y| {
            Box::new(y.map(Ok).into_iter())
        })
    }),
    ("limit", 2, |args, cv| {
        let n = args[0].run(cv.clone()).map(|n| n?.as_int());
        let f = move |n| args[1].run(cv.clone()).take(core::cmp::max(0, n) as usize);
        Box::new(n.flat_map(move |n| then(n, |n| Box::new(f(n)))))
    }),
    // `range(min; max)` returns all integers `n` with `min <= n < max`.
    //
    // This implements a ~10x faster version of:
    // ~~~ text
    // range(min; max):
    //   min as $min | max as $max | $min | select(. < $max) |
    //   recurse(.+1 | select(. < $max))
    // ~~~
    ("range", 2, |args, cv| {
        let prod = Filter::cartesian(&args[0], &args[1], cv);
        let ranges = prod.map(|(l, u)| Ok((l?.as_int()?, u?.as_int()?)));
        let f = |(l, u)| (l..u).map(|i| Ok(Val::Int(i)));
        Box::new(ranges.flat_map(move |range| then(range, |lu| Box::new(f(lu)))))
    }),
    ("recurse_inner", 1, |args, cv| {
        args[0].recurse1(true, false, cv)
    }),
    ("recurse_outer", 1, |args, cv| {
        args[0].recurse1(false, true, cv)
    }),
];

const CORE_UPDATE: [(&str, usize, RunPtr, UpdatePtr); 4] = [
    (
        "empty",
        0,
        |_, _| Box::new(core::iter::empty()),
        |_, cv, _| box_once(Ok(cv.1)),
    ),
    (
        "error",
        0,
        |_, cv| box_once(Err(Error::Val(cv.1))),
        |_, cv, _| box_once(Err(Error::Val(cv.1))),
    ),
    (
        "debug",
        0,
        |_, cv| box_once(Ok(cv.1.debug())),
        |_, cv, f| f(cv.1.debug()),
    ),
    (
        "recurse",
        1,
        |args, cv| args[0].recurse1(true, true, cv),
        |args, cv, f| args[0].recurse_update(cv, f),
    ),
];

pub fn natives() -> impl Iterator<Item = (String, usize, Native)> {
    // TODO: make this more compact
    let cores = CORE
        .iter()
        .map(|(name, arity, f)| (name.to_string(), *arity, Native::new(*f)));
    let core_update = CORE_UPDATE.iter().map(|(name, arity, run, update)| {
        (name.to_string(), *arity, Native::with_update(*run, *update))
    });
    cores.chain(core_update)
}

/// A filter whose behaviour is specified by function pointers.
#[derive(Clone)]
pub struct Native {
    run: RunPtr,
    update: UpdatePtr,
}

impl core::fmt::Debug for Native {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Native").finish()
    }
}

impl Native {
    /// Create a new custom filter from a function.
    pub const fn new(run: RunPtr) -> Self {
        Self::with_update(run, |_, _, _| box_once(Err(Error::PathExp)))
    }

    /// Create a new custom filter from a run function and an update function (used for `filter |= ...`).
    pub const fn with_update(run: RunPtr, update: UpdatePtr) -> Self {
        Self { run, update }
    }
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
                    .map(|(kf, vf)| Self::cartesian(kf, vf, cv.clone()).collect::<Vec<_>>())
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
            Self::Pipe(l, true, r) => l.pipe(cv, |cv, y| r.run((cv.0.cons_var(y), cv.1))),

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
            Self::UpdateMath(path, op, f) => f.pipe(cv, move |cv, y| {
                path.update(cv, Box::new(move |x| box_once(op.run(x, y.clone()))))
            }),
            Self::Assign(path, f) => f.pipe(cv, |cv, y| {
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
                Box::new(Self::cartesian(l, r, cv).map(|(x, y)| op.run(x?, y?)))
            }
            Self::Ord(l, op, r) => {
                Box::new(Self::cartesian(l, r, cv).map(|(x, y)| Ok(Val::Bool(op.run(&x?, &y?)))))
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
            Self::Recurse(f) => f.recurse1(true, true, cv),

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
            Self::Recurse(l) => l.recurse_update(cv, f),

            Self::Var(_) => err,
            Self::Call { skip, id } => {
                let (save, rec) = &cv.0.recs[*id];
                rec.update((cv.0.save_skip_vars(*save, *skip), cv.1), f)
            }

            Self::Native(Native { update, .. }, args) => (update)(args, cv, f),
        }
    }

    /// For every value `v` returned by `self.run(cv)`, call `f(cv, v)` and return all results.
    ///
    /// This has a special optimisation for the case where only a single `v` is returned.
    /// In that case, we can consume `cv` instead of cloning it.
    fn pipe<'a>(&'a self, cv: Cv<'a>, f: impl Fn(Cv<'a>, Val) -> ValRs<'a> + 'a) -> ValRs<'a> {
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

    fn cartesian<'a>(&'a self, r: &'a Self, cv: Cv<'a>) -> impl Iterator<Item = (ValR, ValR)> + 'a {
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

    /// `..`, also known as `recurse`, is defined as `recurse(.[]?)`
    pub(crate) fn recurse() -> Self {
        // `[]?`
        let path = (path::Part::Range(None, None), path::Opt::Optional);
        // `.[]?`
        let path = Filter::Path(Box::new(Filter::Id), Path(Vec::from([path])));
        Filter::Recurse(Box::new(path))
    }

    fn recurse1<'a>(&'a self, inner: bool, outer: bool, cv: Cv<'a>) -> ValRs {
        let f = move |v| self.run((cv.0.clone(), v));
        Box::new(recurse(inner, outer, box_once(Ok(cv.1)), f))
    }

    fn recurse_update<'a>(&'a self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs {
        // implemented by the expansion of `def recurse(l): ., (l | recurse(l))`
        Box::new(f(cv.1).flat_map(move |v| {
            then(v, |v| {
                let (c, f) = (cv.0.clone(), f.clone());
                let rec = move |v| self.recurse_update((c.clone(), v), f.clone());
                self.update((cv.0.clone(), v), Box::new(rec))
            })
        }))
    }

    fn regex<'a>(&'a self, flags: &'a Self, s: bool, m: bool, cv: Cv<'a>) -> ValRs {
        let flags_re = Self::cartesian(flags, self, (cv.0, cv.1.clone()));
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
