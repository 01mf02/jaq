use crate::path::{self, Path};
use crate::val::{Val, ValR, ValRs};
use crate::Error;
use alloc::string::{String, ToString};
use alloc::{boxed::Box, collections::VecDeque, rc::Rc, vec::Vec};
use jaq_parse::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Clone, Debug)]
pub enum Filter {
    Pos(usize),
    Float(f64),
    Str(Rc<String>),
    Array(Option<Box<Self>>),
    Object(Vec<(Self, Self)>),

    Neg(Box<Self>),
    Pipe(Box<Self>, bool, Box<Self>),
    Comma(Box<Self>, Box<Self>),
    Alt(Box<Self>, Box<Self>),
    IfThenElse(Vec<(Self, Self)>, Box<Self>),
    Reduce(Box<Self>, Box<Self>, Box<Self>),

    Path(Path<Self>),
    Assign(Path<Self>, Box<Self>),
    Update(Path<Self>, Box<Self>),

    Logic(Box<Self>, bool, Box<Self>),
    Math(Box<Self>, MathOp, Box<Self>),
    Ord(Box<Self>, OrdOp, Box<Self>),

    Error,
    Length,
    Type,
    Floor,
    Round,
    Ceil,
    FromJson,
    ToJson,
    Keys,
    Sort,
    SortBy(Box<Self>),
    Has(Box<Self>),
    Split(Box<Self>),
    First(Box<Self>),
    Last(Box<Self>),
    Recurse(Box<Self>),
    Contains(Box<Self>),
    Limit(Box<Self>, Box<Self>),
    Range(Box<Self>, Box<Self>),

    SkipCtx(usize, Box<Self>),
    Var(usize),
    Arg(usize),
}

#[derive(Clone, Debug)]
enum Ctx {
    Nil,
    Cons(Val, Rc<Ctx>),
}

impl Ctx {
    fn get(&self, n: usize) -> Option<&Val> {
        match (self, n) {
            (Self::Nil, _) => None,
            (Self::Cons(x, _), 0) => Some(x),
            (Self::Cons(_, xs), n) => xs.get(n - 1),
        }
    }

    fn skip(mut self, mut n: usize) -> Self {
        while n > 0 {
            match self {
                Self::Cons(_, xs) => self = (*xs).clone(),
                Self::Nil => return Self::Nil,
            }
            n -= 1;
        }
        self
    }
}

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
            make_builtin!("error", 0, Self::Error),
            make_builtin!("length", 0, Self::Length),
            make_builtin!("type", 0, Self::Type),
            make_builtin!("keys", 0, Self::Keys),
            make_builtin!("floor", 0, Self::Floor),
            make_builtin!("round", 0, Self::Round),
            make_builtin!("ceil", 0, Self::Ceil),
            make_builtin!("fromjson", 0, Self::FromJson),
            make_builtin!("tojson", 0, Self::ToJson),
            make_builtin!("sort", 0, Self::Sort),
            make_builtin!("sort_by", 1, Self::SortBy),
            make_builtin!("has", 1, Self::Has),
            make_builtin!("split", 1, Self::Split),
            make_builtin!("first", 1, Self::First),
            make_builtin!("last", 1, Self::Last),
            make_builtin!("recurse", 1, Self::Recurse),
            make_builtin!("contains", 1, Self::Contains),
            make_builtin!("limit", 2, Self::Limit),
            make_builtin!("range", 2, Self::Range),
        ])
    }

    pub fn run_with_empty_ctx(&self, val: Val) -> ValRs {
        self.run((Ctx::Nil, val))
    }

    fn run(&self, cv: (Ctx, Val)) -> ValRs {
        use core::iter::once;
        use itertools::Itertools;
        match self {
            Self::Pos(n) => Box::new(once(Ok(Val::Pos(*n)))),
            Self::Float(x) => Box::new(once(Ok(Val::Float(*x)))),
            Self::Str(s) => Box::new(once(Ok(Val::Str(Rc::clone(s))))),
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
                            .map(|(k, v)| Ok((k?.as_obj_key()?, v?)))
                            .collect::<Result<_, _>>()
                            .map(|kvs| Val::Obj(Rc::new(kvs)))
                    }),
            ),
            Self::Neg(f) => Box::new(f.run(cv).map(|v| -v?)),
            Self::Pipe(l, false, r) => {
                Box::new(l.run((cv.0.clone(), cv.1)).flat_map(move |y| match y {
                    Ok(y) => r.run((cv.0.clone(), y)),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
            Self::Pipe(l, true, r) => Box::new(l.run(cv.clone()).flat_map(move |y| match y {
                Ok(y) => r.run((Ctx::Cons(y, Rc::new(cv.0.clone())), cv.1.clone())),
                Err(e) => Box::new(once(Err(e))),
            })),
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
            Self::IfThenElse(if_thens, else_) => Self::if_then_else(if_thens.iter(), else_, cv),
            Self::Path(path) => match path.collect(cv) {
                Ok(y) => Box::new(y.into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Assign(path, f) => path.run(cv.clone(), |_| f.run(cv.clone())),
            Self::Update(path, f) => path.run((cv.0.clone(), cv.1), |v| f.run((cv.0.clone(), v))),
            Self::Logic(l, stop, r) => Box::new(l.run(cv.clone()).flat_map(move |l| match l {
                Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                Ok(l) if l.as_bool() == *stop => Box::new(once(Ok(Val::Bool(*stop)))),
                Ok(_) => Box::new(r.run(cv.clone()).map(|r| Ok(Val::Bool(r?.as_bool())))),
            })),
            Self::Math(l, op, r) => {
                Box::new(Self::cartesian(l, r, cv).map(|(x, y)| op.run(x?, y?)))
            }
            Self::Ord(l, op, r) => {
                Box::new(Self::cartesian(l, r, cv).map(|(x, y)| Ok(Val::Bool(op.run(&x?, &y?)))))
            }
            Self::Error => Box::new(once(Err(Error::Val(cv.1)))),
            Self::Length => Box::new(once(cv.1.len())),
            Self::Type => Box::new(once(Ok(Val::Str(Rc::new(cv.1.typ().to_string()))))),
            Self::Keys => match cv.1.keys() {
                Ok(keys) => Box::new(keys.collect::<Vec<_>>().into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Floor => Box::new(once(cv.1.round(|f| f.floor()))),
            Self::Round => Box::new(once(cv.1.round(|f| f.round()))),
            Self::Ceil => Box::new(once(cv.1.round(|f| f.ceil()))),
            Self::FromJson => Box::new(once(cv.1.from_json())),
            Self::ToJson => Box::new(once(Ok(Val::Str(Rc::new(cv.1.to_string()))))),
            Self::Sort => Box::new(once(cv.1.sort())),
            Self::SortBy(f) => Box::new(once(cv.1.sort_by(|v| f.run((cv.0.clone(), v))))),
            Self::Has(f) => Box::new(
                f.run(cv.clone())
                    .map(move |k| Ok(Val::Bool(cv.1.has(&k?)?))),
            ),
            Self::Split(f) => Box::new(f.run(cv.clone()).map(move |sep| {
                match (&cv.1, sep?) {
                    (Val::Str(s), Val::Str(sep)) => Ok(Val::Arr(Rc::new(
                        s.split(&*sep)
                            .map(|s| Val::Str(Rc::new(s.to_string())))
                            .collect(),
                    ))),
                    _ => Err(Error::Split),
                }
            })),
            Self::First(f) => Box::new(f.run(cv).take(1)),
            Self::Last(f) => match f.run(cv).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(y) => Box::new(y.map(Ok).into_iter()),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Limit(n, f) => {
                let n = n.run(cv.clone()).map(|n| n?.as_usize());
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) => Box::new(f.run(cv.clone()).take(n)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Range(from, until) => {
                let prod = Self::cartesian(from, until, cv);
                let ranges = prod.map(|(from, until)| from?.range(&until?));
                Box::new(ranges.flat_map(|range| match range {
                    Ok(range) => Box::new(range.map(Ok)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Recurse(f) => Box::new(Recurse::new(&**f, cv)),
            Self::Contains(f) => Box::new(
                f.run(cv.clone())
                    .map(move |y| y.map(|y| Val::Bool(cv.1.contains(&y)))),
            ),
            Self::Reduce(xs, init, f) => {
                let init: Result<Vec<_>, _> = init.run(cv.clone()).collect();
                let mut xs = xs.run(cv.clone());
                match init.and_then(|init| {
                    xs.try_fold(init, |acc, x| f.reduce_step(cv.0.clone(), acc, &x?))
                }) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }

            Self::SkipCtx(n, f) => f.run((cv.0.skip(*n), cv.1)),
            Self::Var(v) => Box::new(once(Ok(cv.0.get(*v).unwrap().clone()))),
            Self::Arg(_) => panic!("BUG: unsubstituted argument encountered"),
        }
    }

    fn cartesian(&self, other: &Self, cv: (Ctx, Val)) -> impl Iterator<Item = (ValR, ValR)> + '_ {
        let l = self.run(cv.clone());
        let r: Vec<_> = other.run(cv).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }

    fn if_then_else<'a, I>(mut if_thens: I, else_: &'a Self, cv: (Ctx, Val)) -> ValRs
    where
        I: Iterator<Item = &'a (Self, Self)> + Clone + 'a,
    {
        match if_thens.next() {
            None => else_.run(cv),
            Some((if_, then)) => Box::new(if_.run(cv.clone()).flat_map(move |v| match v {
                Ok(v) if v.as_bool() => then.run(cv.clone()),
                Ok(_) => Self::if_then_else(if_thens.clone(), else_, cv.clone()),
                Err(e) => Box::new(core::iter::once(Err(e))),
            })),
        }
    }

    fn reduce_step(&self, ctx: Ctx, acc: Vec<Val>, x: &Val) -> Result<Vec<Val>, Error> {
        acc.into_iter()
            .flat_map(|acc| self.run((Ctx::Cons(x.clone(), Rc::new(ctx.clone())), acc)))
            .collect()
    }

    pub(crate) fn update_math(path: Path<Self>, op: MathOp, f: Self) -> Self {
        let id = Self::Path(Path::new(Vec::new()));
        let math = Self::Math(Box::new(id), op, Box::new(f));
        Self::Update(path, Box::new(math))
    }

    pub fn subst(self, args: &[Self]) -> Self {
        let subst = |f: Self| f.subst(args);
        let sub = |f: Box<Self>| Box::new(subst(*f));

        match self {
            Self::Pos(_) | Self::Float(_) | Self::Str(_) => self,
            Self::Array(f) => Self::Array(f.map(sub)),
            Self::Object(kvs) => {
                Self::Object(kvs.into_iter().map(|(k, v)| (subst(k), subst(v))).collect())
            }
            Self::Neg(f) => Self::Neg(sub(f)),
            Self::Pipe(l, bind, r) => Self::Pipe(sub(l), bind, sub(r)),
            Self::Comma(l, r) => Self::Comma(sub(l), sub(r)),
            Self::Alt(l, r) => Self::Alt(sub(l), sub(r)),
            Self::IfThenElse(if_thens, else_) => Self::IfThenElse(
                if_thens
                    .into_iter()
                    .map(|(if_, then)| (subst(if_), subst(then)))
                    .collect(),
                sub(else_),
            ),
            Self::Reduce(xs, init, f) => Self::Reduce(sub(xs), sub(init), sub(f)),
            Self::Path(path) => Self::Path(path.map(subst)),
            Self::Assign(path, f) => Self::Assign(path.map(subst), sub(f)),
            Self::Update(path, f) => Self::Update(path.map(subst), sub(f)),
            Self::Logic(l, stop, r) => Self::Logic(sub(l), stop, sub(r)),
            Self::Math(l, op, r) => Self::Math(sub(l), op, sub(r)),
            Self::Ord(l, op, r) => Self::Ord(sub(l), op, sub(r)),
            Self::Error | Self::Length | Self::Type | Self::Keys => self,
            Self::Floor | Self::Round | Self::Ceil => self,
            Self::FromJson | Self::ToJson => self,
            Self::Sort => self,
            Self::SortBy(f) => Self::SortBy(sub(f)),
            Self::Has(f) => Self::Has(sub(f)),
            Self::Split(f) => Self::Split(sub(f)),
            Self::First(f) => Self::First(sub(f)),
            Self::Last(f) => Self::Last(sub(f)),
            Self::Recurse(f) => Self::Recurse(sub(f)),
            Self::Contains(f) => Self::Contains(sub(f)),
            Self::Limit(n, f) => Self::Limit(sub(n), sub(f)),
            Self::Range(lower, upper) => Self::Range(sub(lower), sub(upper)),

            Self::SkipCtx(drop, f) => Self::SkipCtx(drop, sub(f)),
            Self::Var(_) => self,
            Self::Arg(a) => args[a].clone(),
        }
    }
}

type PathOptR = Result<(path::Part<Vec<Val>>, path::Opt), Error>;

impl Path<Filter> {
    fn run<'f, F>(&self, cv: (Ctx, Val), f: F) -> ValRs<'f>
    where
        F: Fn(Val) -> ValRs<'f> + Copy,
    {
        match self.run_indices(&cv).collect::<Result<Vec<_>, _>>() {
            Ok(path) => path::Part::run(path.iter(), cv.1, f),
            Err(e) => Box::new(core::iter::once(Err(e))),
        }
    }

    fn collect(&self, cv: (Ctx, Val)) -> Result<Vec<Val>, Error> {
        let init = Vec::from([cv.1.clone()]);
        self.run_indices(&cv).try_fold(init, |acc, p_opt| {
            let (p, opt) = p_opt?;
            opt.collect(acc.into_iter().flat_map(|x| p.collect(x)))
        })
    }

    fn run_indices<'a>(&'a self, cv: &'a (Ctx, Val)) -> impl Iterator<Item = PathOptR> + 'a {
        let path = self.0.iter();
        path.map(move |(p, opt)| Ok((p.run_indices(cv.clone())?, *opt)))
    }
}

impl path::Part<Filter> {
    fn run_indices(&self, cv: (Ctx, Val)) -> Result<path::Part<Vec<Val>>, Error> {
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

pub struct Recurse<F> {
    filter: F,
    ctx: Ctx,
    input: VecDeque<Val>,
    output: VecDeque<ValR>,
}

impl<F> Recurse<F> {
    fn new(filter: F, (ctx, val): (Ctx, Val)) -> Self {
        Self {
            filter,
            ctx,
            input: VecDeque::new(),
            output: VecDeque::from([Ok(val)]),
        }
    }
}

impl Iterator for Recurse<&Filter> {
    type Item = ValR;

    fn next(&mut self) -> Option<Self::Item> {
        match self.output.pop_front() {
            Some(o) => {
                if let Ok(ref o) = o {
                    self.input.push_back(o.clone());
                };
                Some(o)
            }
            None => match self.input.pop_front() {
                None => None,
                Some(i) => {
                    self.output = self.filter.run((self.ctx.clone(), i)).collect();
                    self.next()
                }
            },
        }
    }
}
