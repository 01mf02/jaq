use crate::path::{self, Path};
use crate::val::{Val, ValR, ValRs};
use crate::{fold, Ctx, Error};
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use dyn_clone::DynClone;
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
    /// `reduce` and `foreach`
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
    /// and `foreach xs as $x (init; f)` evaluates to
    ///
    /// ~~~ text
    /// init
    /// | ., (x0 as $x | f
    /// | ...
    /// | ., (xn as $x | f)...)
    /// ~~~
    Fold(bool, Box<Self>, Box<Self>, Box<Self>),

    Path(Box<Self>, Path<Self>),

    Update(Box<Self>, Box<Self>),
    UpdateMath(Box<Self>, MathOp, Box<Self>),
    Assign(Box<Self>, Box<Self>),

    Logic(Box<Self>, bool, Box<Self>),
    Math(Box<Self>, MathOp, Box<Self>),
    Ord(Box<Self>, OrdOp, Box<Self>),

    Empty,
    Error,
    Debug,
    Inputs,
    Length,
    Floor,
    Round,
    Ceil,
    FromJson,
    ToJson,
    Keys,
    Explode,
    Implode,
    AsciiDowncase,
    AsciiUpcase,
    Reverse,
    Sort,
    SortBy(Box<Self>),
    Has(Box<Self>),
    Split(Box<Self>),
    First(Box<Self>),
    Last(Box<Self>),
    Recurse(Box<Self>, bool, bool),
    Contains(Box<Self>),
    Limit(Box<Self>, Box<Self>),
    /// `range(min; max)` returns all integers `n` with `min <= n < max`.
    ///
    /// This implements a ~10x faster version of:
    /// ~~~ text
    /// range(min; max):
    ///   min as $min | max as $max | $min | select(. < $max) |
    ///   recurse(.+1 | select(. < $max))
    /// ~~~
    Range(Box<Self>, Box<Self>),

    SkipCtx(usize, Box<Self>),
    Var(usize),
    Arg(usize),
}

// we can unfortunately not make a `Box<dyn ... + Clone>`
// that is why we have to go through the pain of making a new trait here
trait Update<'a>: Fn(Val) -> ValRs<'a> + DynClone {}

impl<'a, T: Fn(Val) -> ValRs<'a> + Clone> Update<'a> for T {}

dyn_clone::clone_trait_object!(<'a> Update<'a>);

fn then<'a, T, U: 'a, E: 'a>(
    x: Result<T, E>,
    f: impl FnOnce(T) -> Box<dyn Iterator<Item = Result<U, E>> + 'a>,
) -> Box<dyn Iterator<Item = Result<U, E>> + 'a> {
    x.map(f)
        .unwrap_or_else(|e| Box::new(core::iter::once(Err(e))))
}

fn reduce<'a>(xs: ValRs<'a>, init: Val, f: impl Fn(Val, Val) -> ValRs<'a> + 'a) -> ValRs {
    Box::new(fold(false, xs, Box::new(core::iter::once(Ok(init))), f))
}

type Cv<'c> = (Ctx<'c>, Val);

impl Filter {
    pub(crate) fn core() -> Vec<((String, usize), Self)> {
        let arg = |v| Box::new(Self::Arg(v));
        macro_rules! make_builtin {
            ($name: expr, 0, $cons: expr) => {
                (($name.to_string(), 0), $cons)
            };
            ($name: expr, 1, $cons: expr) => {
                (($name.to_string(), 1), $cons(arg(0)))
            };
            ($name: expr, 2, $cons: expr) => {
                (($name.to_string(), 2), $cons(arg(0), arg(1)))
            };
        }
        Vec::from([
            make_builtin!("empty", 0, Self::Empty),
            make_builtin!("error", 0, Self::Error),
            make_builtin!("debug", 0, Self::Debug),
            make_builtin!("inputs", 0, Self::Inputs),
            make_builtin!("length", 0, Self::Length),
            make_builtin!("keys", 0, Self::Keys),
            make_builtin!("floor", 0, Self::Floor),
            make_builtin!("round", 0, Self::Round),
            make_builtin!("ceil", 0, Self::Ceil),
            make_builtin!("fromjson", 0, Self::FromJson),
            make_builtin!("tojson", 0, Self::ToJson),
            make_builtin!("explode", 0, Self::Explode),
            make_builtin!("implode", 0, Self::Implode),
            make_builtin!("ascii_downcase", 0, Self::AsciiDowncase),
            make_builtin!("ascii_upcase", 0, Self::AsciiUpcase),
            make_builtin!("reverse", 0, Self::Reverse),
            make_builtin!("sort", 0, Self::Sort),
            make_builtin!("sort_by", 1, Self::SortBy),
            make_builtin!("has", 1, Self::Has),
            make_builtin!("contains", 1, Self::Contains),
            make_builtin!("split", 1, Self::Split),
            make_builtin!("first", 1, Self::First),
            make_builtin!("last", 1, Self::Last),
            make_builtin!("recurse", 1, |f| Self::Recurse(f, true, true)),
            make_builtin!("recurse_inner", 1, |f| Self::Recurse(f, true, false)),
            make_builtin!("recurse_outer", 1, |f| Self::Recurse(f, false, true)),
            make_builtin!("limit", 2, Self::Limit),
            make_builtin!("range", 2, Self::Range),
        ])
    }

    pub fn run<'a>(&'a self, cv: Cv<'a>) -> ValRs<'a> {
        use core::iter::once;
        use itertools::Itertools;
        match self {
            Self::Id => Box::new(once(Ok(cv.1))),
            Self::Int(n) => Box::new(once(Ok(Val::Int(*n)))),
            Self::Float(x) => Box::new(once(Ok(Val::Float(*x)))),
            Self::Str(s) => Box::new(once(Ok(Val::Str(Rc::new(s.clone()))))),
            Self::Array(None) => Box::new(once(Ok(Val::Arr(Default::default())))),
            Self::Array(Some(f)) => Box::new(once(
                f.run(cv)
                    .collect::<Result<_, _>>()
                    .map(|v| Val::Arr(Rc::new(v))),
            )),
            Self::Object(o) if o.is_empty() => Box::new(once(Ok(Val::Obj(Default::default())))),
            Self::Object(o) => Box::new(
                o.iter()
                    .map(|(kf, vf)| Self::cartesian(kf, vf, cv.clone()).collect::<Vec<_>>())
                    .multi_cartesian_product()
                    .map(|kvs| {
                        kvs.into_iter()
                            .map(|(k, v)| Ok((k?.str()?, v?)))
                            .collect::<Result<_, _>>()
                            .map(|kvs| Val::Obj(Rc::new(kvs)))
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
                path.update(cv, Box::new(move |x| Box::new(once(op.run(x, y.clone())))))
            }),
            Self::Assign(path, f) => f.pipe(cv, |cv, y| {
                path.update(cv, Box::new(move |_| Box::new(once(Ok(y.clone())))))
            }),
            Self::Logic(l, stop, r) => Box::new(l.run(cv.clone()).flat_map(move |l| {
                then(l, |l| {
                    if l.as_bool() == *stop {
                        Box::new(once(Ok(Val::Bool(*stop))))
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

            Self::Empty => Box::new(core::iter::empty()),
            Self::Error => Box::new(once(Err(Error::Val(cv.1)))),
            Self::Debug => Box::new(once(Ok(cv.1.debug()))),
            Self::Inputs => Box::new(cv.0.inputs.map(|r| r.map_err(Error::Parse))),
            Self::Length => Box::new(once(cv.1.len())),
            Self::Keys => Box::new(once(cv.1.keys().map(|a| Val::Arr(Rc::new(a))))),
            Self::Floor => Box::new(once(cv.1.round(|f| f.floor()))),
            Self::Round => Box::new(once(cv.1.round(|f| f.round()))),
            Self::Ceil => Box::new(once(cv.1.round(|f| f.ceil()))),
            Self::FromJson => Box::new(once(cv.1.from_json())),
            Self::ToJson => Box::new(once(Ok(Val::Str(Rc::new(cv.1.to_string()))))),
            Self::Explode => Box::new(once(cv.1.explode().map(|a| Val::Arr(Rc::new(a))))),
            Self::Implode => Box::new(once(cv.1.implode().map(|s| Val::Str(Rc::new(s))))),
            Self::AsciiDowncase => Box::new(once(cv.1.mutate_str(|s| s.make_ascii_lowercase()))),
            Self::AsciiUpcase => Box::new(once(cv.1.mutate_str(|s| s.make_ascii_uppercase()))),
            Self::Reverse => Box::new(once(cv.1.mutate_arr(|a| a.reverse()))),
            Self::Sort => Box::new(once(cv.1.mutate_arr(|a| a.sort()))),
            Self::SortBy(f) => Box::new(once(cv.1.sort_by(|v| f.run((cv.0.clone(), v))))),
            Self::Has(f) => Box::new(
                f.run(cv.clone())
                    .map(move |k| Ok(Val::Bool(cv.1.has(&k?)?))),
            ),
            Self::Contains(f) => Box::new(
                f.run(cv.clone())
                    .map(move |y| Ok(Val::Bool(cv.1.contains(&y?)))),
            ),
            Self::Split(f) => Box::new(
                f.run(cv.clone())
                    .map(move |sep| Ok(Val::Arr(Rc::new(cv.1.split(&sep?)?)))),
            ),

            Self::First(f) => Box::new(f.run(cv).take(1)),
            Self::Last(f) => then(f.run(cv).try_fold(None, |_, x| Ok(Some(x?))), |y| {
                Box::new(y.map(Ok).into_iter())
            }),
            Self::Limit(n, f) => {
                let n = n.run(cv.clone()).map(|n| n?.as_int());
                Box::new(n.flat_map(move |n| {
                    then(n, |n| {
                        Box::new(f.run(cv.clone()).take(core::cmp::max(0, n) as usize))
                    })
                }))
            }
            Self::Range(from, until) => {
                let prod = Self::cartesian(from, until, cv);
                let ranges = prod.map(|(l, u)| Ok((l?.as_int()?, u?.as_int()?)));
                Box::new(ranges.flat_map(|range| {
                    then(range, |(l, u)| Box::new((l..u).map(|i| Ok(Val::Int(i)))))
                }))
            }
            // if `inner` is true, output values that yield non-empty output;
            // if `outer` is true, output values that yield     empty output
            Self::Recurse(f, inner, outer) => {
                let mut stack = Vec::from([(Box::new(once(Ok(cv.1))) as ValRs).peekable()]);
                Box::new(core::iter::from_fn(move || loop {
                    let v = loop {
                        let mut iter = stack.pop()?;
                        match iter.next() {
                            None => continue,
                            Some(Ok(v)) => {
                                if iter.peek().is_some() {
                                    stack.push(iter)
                                }
                                break v;
                            }
                            e => return e,
                        }
                    };
                    let mut iter = f.run((cv.0.clone(), v.clone())).peekable();
                    match (*inner, *outer) {
                        (true, true) => {
                            stack.push(iter);
                            return Some(Ok(v));
                        }
                        (true, false) => {
                            if iter.peek().is_some() {
                                stack.push(iter);
                                return Some(Ok(v));
                            }
                        }
                        (false, true) => {
                            if iter.peek().is_some() {
                                stack.push(iter);
                            } else {
                                return Some(Ok(v));
                            }
                        }
                        // pathological case, included only for completeness
                        (false, false) => stack.push(iter),
                    }
                }))
            }
            Self::Fold(inner, xs, init, f) => {
                let xs = xs.run(cv.clone());
                let init = init.run(cv.clone());
                let f = move |x, v| f.run((cv.0.clone().cons_var(x), v));
                Box::new(fold(*inner, xs, init, f))
            }

            Self::SkipCtx(n, f) => f.run((cv.0.skip_vars(*n), cv.1)),
            Self::Var(v) => Box::new(once(Ok(cv.0.vars.get(*v).unwrap().clone()))),
            Self::Arg(_) => panic!("BUG: unsubstituted argument encountered"),
        }
    }

    /// `p.update(cv, f)` returns the output of `v | p |= f`
    fn update<'a>(&'a self, cv: Cv<'a>, f: Box<dyn Update<'a> + 'a>) -> ValRs {
        use core::iter::once;
        let err = Box::new(core::iter::once(Err(Error::PathExp)));
        match self {
            Self::Int(_) | Self::Float(_) | Self::Str(_) => err,
            Self::Array(_) | Self::Object(_) => err,
            Self::Neg(_) | Self::Logic(..) | Self::Math(..) | Self::Ord(..) => err,
            Self::Update(..) | Self::UpdateMath(..) | Self::Assign(..) => err,

            Self::Length | Self::Keys => err,
            Self::Floor | Self::Round | Self::Ceil => err,
            Self::FromJson | Self::ToJson => err,
            Self::Explode | Self::Implode => err,
            Self::AsciiDowncase | Self::AsciiUpcase => err,
            Self::Reverse | Self::Sort | Self::SortBy(_) => err,
            Self::Has(_) | Self::Contains(_) => err,
            Self::Split(_) => err,
            Self::Inputs | Self::Range(..) => err,

            // these are up for grabs to implement :)
            Self::Try(_) | Self::Alt(..) => todo!(),
            Self::First(_) | Self::Last(_) | Self::Limit(..) => todo!(),
            Self::Fold(..) => todo!(),

            Self::Error => Box::new(once(Err(Error::Val(cv.1)))),
            Self::Debug => f(cv.1.debug()),
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
            // implemented by the expansion of `def recurse(l): ., (l | recurse(l))`
            Self::Recurse(l, true, true) => Box::new(f(cv.1).flat_map(move |v| {
                then(v, |v| {
                    let (c, f) = (cv.0.clone(), f.clone());
                    let rec = move |v| self.update((c.clone(), v), f.clone());
                    l.update((cv.0.clone(), v), Box::new(rec))
                })
            })),
            Self::Recurse(..) => todo!(),
            Self::Empty => Box::new(once(Ok(cv.1))),

            Self::SkipCtx(n, l) => l.update((cv.0.skip_vars(*n), cv.1), f),
            Self::Var(_) => err,
            Self::Arg(_) => panic!("BUG: unsubstituted argument encountered"),
        }
    }

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
        Filter::Recurse(Box::new(path), true, true)
    }

    pub fn subst(self, args: &[Self]) -> Self {
        let subst = |f: Self| f.subst(args);
        let sub = |f: Box<Self>| Box::new(subst(*f));

        match self {
            Self::Id => self,
            Self::Int(_) | Self::Float(_) | Self::Str(_) => self,
            Self::Array(f) => Self::Array(f.map(sub)),
            Self::Object(kvs) => {
                Self::Object(kvs.into_iter().map(|(k, v)| (subst(k), subst(v))).collect())
            }
            Self::Try(f) => Self::Try(sub(f)),
            Self::Neg(f) => Self::Neg(sub(f)),
            Self::Pipe(l, bind, r) => Self::Pipe(sub(l), bind, sub(r)),
            Self::Comma(l, r) => Self::Comma(sub(l), sub(r)),
            Self::Alt(l, r) => Self::Alt(sub(l), sub(r)),
            Self::Ite(if_, then_, else_) => Self::Ite(sub(if_), sub(then_), sub(else_)),
            Self::Fold(inner, xs, init, f) => Self::Fold(inner, sub(xs), sub(init), sub(f)),
            Self::Path(f, path) => Self::Path(sub(f), path.map(subst)),
            Self::Update(path, f) => Self::Update(sub(path), sub(f)),
            Self::Assign(path, f) => Self::Assign(sub(path), sub(f)),
            Self::UpdateMath(path, op, f) => Self::UpdateMath(sub(path), op, sub(f)),
            Self::Logic(l, stop, r) => Self::Logic(sub(l), stop, sub(r)),
            Self::Math(l, op, r) => Self::Math(sub(l), op, sub(r)),
            Self::Ord(l, op, r) => Self::Ord(sub(l), op, sub(r)),
            Self::Empty | Self::Error | Self::Debug | Self::Inputs => self,
            Self::Length | Self::Keys => self,
            Self::Floor | Self::Round | Self::Ceil => self,
            Self::FromJson | Self::ToJson => self,
            Self::Explode | Self::Implode => self,
            Self::AsciiDowncase | Self::AsciiUpcase => self,
            Self::Reverse | Self::Sort => self,
            Self::SortBy(f) => Self::SortBy(sub(f)),
            Self::Has(f) => Self::Has(sub(f)),
            Self::Contains(f) => Self::Contains(sub(f)),
            Self::Split(f) => Self::Split(sub(f)),
            Self::First(f) => Self::First(sub(f)),
            Self::Last(f) => Self::Last(sub(f)),
            Self::Recurse(f, inner, outer) => Self::Recurse(sub(f), inner, outer),
            Self::Limit(n, f) => Self::Limit(sub(n), sub(f)),
            Self::Range(lower, upper) => Self::Range(sub(lower), sub(upper)),

            Self::SkipCtx(drop, f) => Self::SkipCtx(drop, sub(f)),
            Self::Var(_) => self,
            Self::Arg(a) => args[a].clone(),
        }
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
            Err(e) => return Box::new(core::iter::once(Err(e))),
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
