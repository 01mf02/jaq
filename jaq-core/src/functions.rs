use crate::{ClosedFilter, Error, Filter, RValRs, Val, ValR};
use alloc::{boxed::Box, rc::Rc, string::ToString, vec::Vec};
use core::convert::TryFrom;

pub const FUNCTIONS: &[(&str, usize, Builtin<usize>)] = &[
    // new filters
    ("null", 0, Builtin::New(New::Null)),
    ("true", 0, Builtin::New(New::True)),
    ("false", 0, Builtin::New(New::False)),
    ("length", 0, Builtin::New(New::Length)),
    ("type", 0, Builtin::New(New::Type)),
    // referencing filters
    ("first", 1, Builtin::Ref(Ref::First(0))),
    ("last", 1, Builtin::Ref(Ref::Last(0))),
    ("limit", 2, Builtin::Ref(Ref::Limit(0, 1))),
    ("range", 2, Builtin::Ref(Ref::Range(0, 1))),
    ("recurse", 1, Builtin::Ref(Ref::Recurse(0))),
    ("fold", 3, Builtin::Ref(Ref::Fold(0, 1, 2))),
];

#[derive(Clone, Debug)]
pub enum Builtin<F> {
    New(New),
    Ref(Ref<F>),
}

/// Filter that returns a single, new value.
#[derive(Clone, Debug)]
pub enum New {
    Null,
    True,
    False,
    Length,
    Type,
}

/// Filter that returns a stream of value references.
#[derive(Clone, Debug)]
pub enum Ref<F> {
    First(F),
    Last(F),
    Limit(F, F),
    Range(F, F),
    Recurse(F),
    Fold(F, F, F),
}

impl Builtin<Box<ClosedFilter>> {
    pub fn run(&self, v: Rc<Val>) -> RValRs {
        match self {
            Self::New(n) => Box::new(core::iter::once(n.run(v).map(Rc::new))),
            Self::Ref(r) => r.run(v),
        }
    }
}

impl New {
    fn run(&self, v: Rc<Val>) -> ValR {
        use New::*;
        match self {
            Null => Ok(Val::Null),
            True => Ok(Val::Bool(true)),
            False => Ok(Val::Bool(false)),
            Length => v.len(),
            Type => Ok(Val::Str(v.typ().to_string())),
        }
    }
}

impl Ref<Box<ClosedFilter>> {
    fn run(&self, v: Rc<Val>) -> RValRs {
        use core::iter::once;
        match self {
            Self::First(f) => Box::new(f.run(v).take(1)),
            Self::Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(y) => Box::new(y.map(Ok).into_iter()),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Limit(n, f) => {
                let n = n.run(Rc::clone(&v)).map(|n| usize::try_from(&*n?));
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) => Box::new(f.run(Rc::clone(&v)).take(n as usize)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Range(from, until) => {
                let prod = Filter::cartesian(from, until, v);
                let ranges = prod.map(|(from, until)| from?.range(&*until?));
                Box::new(ranges.flat_map(|range| match range {
                    Ok(range) => Box::new(range.map(Rc::new).map(Ok)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Recurse(f) => Box::new(crate::Recurse::new(f, v)),
            Self::Fold(xs, init, f) => {
                let mut xs = xs.run(Rc::clone(&v));
                let init: Result<Vec<_>, _> = init.run(Rc::clone(&v)).collect();
                match init.and_then(|init| xs.try_fold(init, |acc, x| f.fold_step(acc, x?))) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }
        }
    }
}

impl ClosedFilter {
    fn fold_step(&self, acc: Vec<Rc<Val>>, x: Rc<Val>) -> Result<Vec<Rc<Val>>, Error> {
        acc.into_iter()
            .map(|acc| {
                let obj = [("acc".to_string(), acc), ("x".to_string(), Rc::clone(&x))];
                Val::Obj(Vec::from(obj).into_iter().collect())
            })
            .flat_map(|obj| self.run(Rc::new(obj)))
            .collect()
    }
}

impl<F> Builtin<F> {
    pub fn map<G>(self, m: &impl Fn(F) -> G) -> Builtin<G> {
        match self {
            Self::New(n) => Builtin::New(n),
            Self::Ref(r) => Builtin::Ref(r.map(m)),
        }
    }
}

impl<N> Builtin<Box<Filter<N>>> {
    pub fn try_map<F, M, E>(self, m: &F) -> Result<Builtin<Box<Filter<M>>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        match self {
            Builtin::New(n) => Ok(Builtin::New(n)),
            Builtin::Ref(r) => Ok(Builtin::Ref(r.try_map(m)?)),
        }
    }
}

impl<F> Ref<F> {
    fn map<G>(self, m: &impl Fn(F) -> G) -> Ref<G> {
        use Ref::*;
        match self {
            First(f) => First(m(f)),
            Last(f) => Last(m(f)),
            Limit(n, f) => Limit(m(n), m(f)),
            Range(from, until) => Range(m(from), m(until)),
            Recurse(f) => Recurse(m(f)),
            Fold(xs, init, f) => Fold(m(xs), m(init), m(f)),
        }
    }
}

impl<N> Ref<Box<Filter<N>>> {
    fn try_map<F, M, E>(self, m: &F) -> Result<Ref<Box<Filter<M>>>, E>
    where
        F: Fn(N) -> Result<Filter<M>, E>,
    {
        let m = |f: Filter<N>| f.try_map(m).map(Box::new);
        use Ref::*;
        match self {
            First(f) => Ok(First(m(*f)?)),
            Last(f) => Ok(Last(m(*f)?)),
            Limit(n, f) => Ok(Limit(m(*n)?, m(*f)?)),
            Range(from, until) => Ok(Range(m(*from)?, m(*until)?)),
            Recurse(f) => Ok(Recurse(m(*f)?)),
            Fold(xs, init, f) => Ok(Fold(m(*xs)?, m(*init)?, m(*f)?)),
        }
    }
}
