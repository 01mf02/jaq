use crate::val::{Val, ValR, ValRs};
use crate::{Error, Path};
use alloc::{boxed::Box, rc::Rc, string::String, string::ToString, vec::Vec};
use jaq_parse::{MathOp, OrdOp};

/// Function from a value to a stream of value results.
#[derive(Clone, Debug)]
pub enum Filter {
    Pos(usize),
    Float(f64),
    Str(String),
    Array(Option<Box<Self>>),
    Object(Vec<(Self, Self)>),

    Neg(Box<Self>),
    Pipe(Box<Self>, Box<Self>),
    Comma(Box<Self>, Box<Self>),
    IfThenElse(Box<Self>, Box<Self>, Box<Self>),

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
    Fold(Box<Self>, Box<Self>, Box<Self>),

    Arg(usize),
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
            ($name: expr, 3, $cons: expr) => {
                (($name.to_string(), 3), $cons(arg(0), arg(1), arg(2)))
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
            make_builtin!("fold", 3, Self::Fold),
        ])
    }

    pub fn run(&self, v: Val) -> ValRs {
        use core::iter::once;
        use itertools::Itertools;
        match self {
            Self::Pos(n) => Box::new(once(Ok(Val::Pos(*n)))),
            Self::Float(x) => Box::new(once(Ok(Val::Float(*x)))),
            Self::Str(s) => Box::new(once(Ok(Val::Str(Rc::new(s.clone()))))),
            Self::Array(None) => Box::new(once(Ok(Val::Arr(Default::default())))),
            Self::Array(Some(f)) => Box::new(once(
                f.run(v)
                    .collect::<Result<_, _>>()
                    .map(|v| Val::Arr(Rc::new(v))),
            )),
            Self::Object(o) if o.is_empty() => Box::new(once(Ok(Val::Obj(Default::default())))),
            Self::Object(o) => Box::new(
                o.iter()
                    .map(|(kf, vf)| Self::cartesian(kf, vf, v.clone()).collect::<Vec<_>>())
                    .multi_cartesian_product()
                    .map(|kvs| {
                        kvs.into_iter()
                            .map(|(k, v)| Ok((k?.as_obj_key()?, v?)))
                            .collect::<Result<_, _>>()
                            .map(|kvs| Val::Obj(Rc::new(kvs)))
                    }),
            ),
            Self::Neg(f) => Box::new(f.run(v).map(|v| -v?)),
            Self::Pipe(l, r) => Box::new(l.run(v).flat_map(|y| match y {
                Ok(y) => r.run(y),
                Err(e) => Box::new(once(Err(e))),
            })),
            Self::Comma(l, r) => Box::new(l.run(v.clone()).chain(r.run(v))),
            Self::IfThenElse(if_, then, else_) => {
                Box::new(if_.run(v.clone()).flat_map(move |y| match y {
                    Ok(y) => (if y.as_bool() { then } else { else_ }).run(v.clone()),
                    Err(e) => Box::new(once(Err(e))),
                }))
            }
            Self::Path(path) => match path.collect(v) {
                Ok(y) => Box::new(y.into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Assign(path, f) => path.run(v.clone(), |_| f.run(v.clone())),
            Self::Update(path, f) => path.run(v, |v| f.run(v)),
            Self::Logic(l, stop, r) => Box::new(l.run(v.clone()).flat_map(move |l| match l {
                Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                Ok(l) if l.as_bool() == *stop => Box::new(once(Ok(Val::Bool(*stop)))),
                Ok(_) => Box::new(r.run(v.clone()).map(|r| Ok(Val::Bool(r?.as_bool())))),
            })),
            Self::Math(l, op, r) => Box::new(Self::cartesian(l, r, v).map(|(x, y)| op.run(x?, y?))),
            Self::Ord(l, op, r) => {
                Box::new(Self::cartesian(l, r, v).map(|(x, y)| Ok(Val::Bool(op.run(&x?, &y?)))))
            }
            Self::Error => Box::new(once(Err(Error::Custom(match v {
                Val::Str(s) => (*s).clone(),
                _ => v.to_string(),
            })))),
            Self::Length => Box::new(once(v.len())),
            Self::Type => Box::new(once(Ok(Val::Str(Rc::new(v.typ().to_string()))))),
            Self::Keys => match v.keys() {
                Ok(keys) => Box::new(keys.collect::<Vec<_>>().into_iter().map(Ok)),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Floor => Box::new(once(v.round(|f| f.floor()))),
            Self::Round => Box::new(once(v.round(|f| f.round()))),
            Self::Ceil => Box::new(once(v.round(|f| f.ceil()))),
            Self::FromJson => match v {
                Val::Str(ref s) => match serde_json::from_str::<serde_json::Value>(s) {
                    Ok(json) => Box::new(once(Ok(Val::from(json)))),
                    Err(e) => Box::new(once(Err(Error::FromJson(v, Some(e.to_string()))))),
                },
                _ => Box::new(once(Err(Error::FromJson(v, None)))),
            },
            Self::ToJson => Box::new(once(Ok(Val::Str(Rc::new(v.to_string()))))),
            Self::Sort => match v {
                Val::Arr(mut a) => {
                    Rc::make_mut(&mut a).sort();
                    Box::new(once(Ok(Val::Arr(a))))
                }
                _ => Box::new(once(Err(Error::Sort(v)))),
            },
            Self::SortBy(f) => match v {
                Val::Arr(mut a) => {
                    let mut err = None;
                    Rc::make_mut(&mut a).sort_by_cached_key(|x| {
                        if err.is_some() {
                            return Vec::new();
                        };
                        match f.run(x.clone()).collect() {
                            Ok(y) => y,
                            Err(e) => {
                                err = Some(e);
                                Vec::new()
                            }
                        }
                    });
                    match err {
                        Some(e) => Box::new(once(Err(e))),
                        None => Box::new(once(Ok(Val::Arr(a)))),
                    }
                }
                _ => Box::new(once(Err(Error::Sort(v)))),
            },
            Self::Has(f) => Box::new(f.run(v.clone()).map(move |k| Ok(Val::Bool(v.has(&k?)?)))),
            Self::Split(f) => Box::new(f.run(v.clone()).map(move |sep| {
                match (&v, sep?) {
                    (Val::Str(s), Val::Str(sep)) => Ok(Val::Arr(Rc::new(
                        s.split(&*sep)
                            .map(|s| Val::Str(Rc::new(s.to_string())))
                            .collect(),
                    ))),
                    _ => Err(Error::Split),
                }
            })),
            Self::First(f) => Box::new(f.run(v).take(1)),
            Self::Last(f) => match f.run(v).try_fold(None, |_, x| Ok(Some(x?))) {
                Ok(y) => Box::new(y.map(Ok).into_iter()),
                Err(e) => Box::new(once(Err(e))),
            },
            Self::Limit(n, f) => {
                let n = n.run(v.clone()).map(|n| n?.as_usize());
                Box::new(n.flat_map(move |n| match n {
                    Ok(n) => Box::new(f.run(v.clone()).take(n as usize)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Range(from, until) => {
                let prod = Filter::cartesian(from, until, v);
                let ranges = prod.map(|(from, until)| from?.range(&until?));
                Box::new(ranges.flat_map(|range| match range {
                    Ok(range) => Box::new(range.map(Ok)),
                    Err(e) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                }))
            }
            Self::Recurse(f) => Box::new(crate::Recurse::new(f, v)),
            Self::Contains(f) => Box::new(
                f.run(v.clone())
                    .map(move |y| y.map(|y| Val::Bool(v.contains(&y)))),
            ),
            Self::Fold(init, xs, f) => {
                let init: Result<Vec<_>, _> = init.run(v.clone()).collect();
                let mut xs = xs.run(v.clone());
                match init.and_then(|init| xs.try_fold(init, |acc, x| f.fold_step(acc, &x?))) {
                    Ok(y) => Box::new(y.into_iter().map(Ok)),
                    Err(e) => Box::new(once(Err(e))),
                }
            }

            Self::Arg(_) => panic!("BUG: unsubstituted argument encountered"),
        }
    }

    fn cartesian(&self, other: &Self, v: Val) -> impl Iterator<Item = (ValR, ValR)> + '_ {
        let l = self.run(v.clone());
        let r: Vec<_> = other.run(v).collect();
        use itertools::Itertools;
        l.into_iter().cartesian_product(r)
    }

    fn fold_step(&self, acc: Vec<Val>, x: &Val) -> Result<Vec<Val>, Error> {
        acc.into_iter()
            .map(|acc| Val::Arr(Rc::new(Vec::from([acc, x.clone()]).into_iter().collect())))
            .flat_map(|obj| self.run(obj))
            .collect()
    }

    pub(crate) fn update_math(path: Path<Self>, op: MathOp, f: Self) -> Self {
        let id = Self::Path(Path::new(Vec::new()));
        let math = Self::Math(Box::new(id), op, Box::new(f));
        Self::Update(path, Box::new(math))
    }

    pub(crate) fn subst(self, args: &[Self]) -> Self {
        let sub = |f: Box<Self>| Box::new(f.subst(args));
        match self {
            Self::Pos(_) | Self::Float(_) | Self::Str(_) => self,
            Self::Array(f) => Self::Array(f.map(sub)),
            Self::Object(kvs) => Self::Object(
                kvs.into_iter()
                    .map(|(k, v)| (k.subst(args), v.subst(args)))
                    .collect(),
            ),
            Self::Neg(f) => Self::Neg(sub(f)),
            Self::Pipe(l, r) => Self::Pipe(sub(l), sub(r)),
            Self::Comma(l, r) => Self::Comma(sub(l), sub(r)),
            Self::IfThenElse(if_, then, else_) => Self::IfThenElse(sub(if_), sub(then), sub(else_)),
            Self::Path(path) => Self::Path(path.map(|f| f.subst(args))),
            Self::Assign(path, f) => Self::Assign(path.map(|f| f.subst(args)), sub(f)),
            Self::Update(path, f) => Self::Update(path.map(|f| f.subst(args)), sub(f)),
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
            Self::Fold(xs, init, f) => Self::Fold(sub(xs), sub(init), sub(f)),
            Self::Arg(v) => args[v].clone(),
        }
    }
}
