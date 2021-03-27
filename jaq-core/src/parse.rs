use crate::filter::{Filter, NewFilter, Ref};
use crate::functions::NewFunc;
use crate::ops::{LogicOp, MathOp};
use crate::path::{Path, PathElem};
use crate::val::Atom;
use alloc::{boxed::Box, string::ToString, vec::Vec};
use core::convert::{TryFrom, TryInto};
use core::fmt::{self, Display};
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::PrecClimber;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct FilterParser;

#[derive(Debug)]
pub enum Error {
    Pest(pest::error::Error<Rule>),
    Undefined(String, usize),
    PathAssign,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Error::*;
        match self {
            Pest(e) => e.fmt(f),
            Undefined(name, arity) => write!(f, "undefined function {}/{}", name, arity),
            PathAssign => write!(f, "path expected before assignment operator"),
        }
    }
}

impl Filter {
    pub fn parse(s: &str) -> Result<Self, Error> {
        let pairs = FilterParser::parse(Rule::main, s).map_err(Error::Pest)?;
        Ok(Self::try_from(pairs)?)
    }
}

lazy_static::lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use pest::prec_climber::{Operator, Assoc::*};

        PrecClimber::new(Vec::from([
            Operator::new(pipe, Left),
            Operator::new(comma, Left),
            Operator::new(assign_op, Right),
            Operator::new(or, Left),
            Operator::new(and, Left),
            Operator::new(eq, Left) | Operator::new(ne, Left),
            Operator::new(gt, Left) | Operator::new(ge, Left) | Operator::new(lt, Left) | Operator::new(le, Left),
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left),
            Operator::new(rem, Left)
        ]))
    };
}

impl TryFrom<Pairs<'_, Rule>> for Filter {
    type Error = Error;
    fn try_from(pairs: Pairs<Rule>) -> Result<Self, Error> {
        PREC_CLIMBER.climb(
            pairs,
            Self::try_from,
            |lhs: Result<Self, _>, op: Pair<Rule>, rhs: Result<Self, _>| {
                let lhs = Box::new(lhs?);
                let rhs = Box::new(rhs?);
                match op.as_rule() {
                    Rule::assign_op => {
                        let path = (*lhs).try_into()?;
                        let op = op.into_inner().next().unwrap();
                        Ok(Self::Ref(match op.as_rule() {
                            Rule::assign => Ref::Assign(path, rhs),
                            Rule::update => Ref::Update(path, rhs),
                            Rule::update_with => {
                                let math = op.into_inner().next().unwrap();
                                let math = MathOp::try_from(math.as_rule()).unwrap();
                                Ref::update_math(path, math, rhs)
                            }
                            _ => unreachable!(),
                        }))
                    }
                    Rule::pipe => Ok(Self::Ref(Ref::Pipe(lhs, rhs))),
                    Rule::comma => Ok(Self::Ref(Ref::Comma(lhs, rhs))),
                    rule => {
                        if let Ok(op) = LogicOp::try_from(rule) {
                            Ok(Self::New(NewFilter::Logic(lhs, op, rhs)))
                        } else if let Ok(op) = MathOp::try_from(rule) {
                            Ok(Self::New(NewFilter::Math(lhs, op, rhs)))
                        } else {
                            unreachable!()
                        }
                    }
                }
            },
        )
    }
}

// TODO: move this somewhere else
impl TryFrom<Filter> for Path {
    type Error = Error;

    fn try_from(f: Filter) -> Result<Self, Self::Error> {
        match f {
            Filter::Ref(Ref::Path(p)) => Ok(p),
            _ => Err(Error::PathAssign),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for Filter {
    type Error = Error;
    fn try_from(pair: Pair<Rule>) -> Result<Self, Error> {
        let rule = pair.as_rule();
        let mut inner = pair.into_inner();
        match rule {
            Rule::expr => Self::try_from(inner),
            Rule::atom => Ok(Atom::from(inner.next().unwrap()).into()),
            Rule::array => {
                let contents = if inner.peek().is_none() {
                    Self::Ref(Ref::Empty)
                } else {
                    Self::try_from(inner)?
                };
                Ok(Self::New(NewFilter::Array(Box::new(contents))))
            }
            Rule::object => {
                let kvs = inner.map(|kv| {
                    let mut iter = kv.into_inner();
                    let key = iter.next().unwrap();
                    match key.as_rule() {
                        Rule::identifier | Rule::string => {
                            let key = Atom::from(key);
                            let value = match iter.next() {
                                Some(value) => Self::try_from(value)?,
                                None => Path::from(PathElem::Index(key.clone().into())).into(),
                            };
                            assert!(iter.next().is_none());
                            Ok((key.into(), value))
                        }
                        Rule::expr => {
                            let value = iter.next().unwrap();
                            assert!(iter.next().is_none());
                            Ok((Self::try_from(key)?, Self::try_from(value)?))
                        }
                        _ => unreachable!(),
                    }
                });
                Ok(Self::New(NewFilter::Object(kvs.collect::<Result<_, _>>()?)))
            }
            Rule::ite => {
                let mut ite = inner.map(|p| Self::try_from(p).map(Box::new));
                let cond = ite.next().unwrap();
                let truth = ite.next().unwrap();
                let falsity = ite.next().unwrap();
                assert!(ite.next().is_none());
                Ok(Self::Ref(Ref::IfThenElse(cond?, truth?, falsity?)))
            }
            Rule::function => {
                let name = inner.next().unwrap().as_str();
                let args = match inner.next() {
                    None => Ok(Vec::new()),
                    Some(args) => args.into_inner().map(Self::try_from).collect(),
                };
                assert_eq!(inner.next(), None);
                Ok(Self::try_from((name, args?))?)
            }
            Rule::path => {
                let path: Result<_, _> = inner.flat_map(PathElem::from_path).collect();
                Ok(Self::Ref(Ref::Path(Path::new(path?))))
            }
            _ => unreachable!(),
        }
    }
}

impl From<Pair<'_, Rule>> for Atom {
    fn from(pair: Pair<Rule>) -> Self {
        use serde_json::Number;
        match pair.as_rule() {
            Rule::null => Self::Null,
            Rule::boole => Self::Bool(pair.as_str().parse::<bool>().unwrap()),
            Rule::number => Self::Num(pair.as_str().parse::<Number>().unwrap().try_into().unwrap()),
            Rule::string => Self::Str(pair.into_inner().next().unwrap().as_str().to_string()),
            Rule::identifier => Self::Str(pair.as_str().to_string()),
            _ => unreachable!(),
        }
    }
}

impl PathElem<Filter> {
    fn from_path(pair: Pair<Rule>) -> impl Iterator<Item = Result<Self, Error>> + '_ {
        use core::iter::{empty, once};
        let mut iter = pair.into_inner();
        let index = iter.next().unwrap();
        let index = match index.as_rule() {
            Rule::path_index => {
                let index = Self::from_index(index).0.to_string();
                Box::new(once(Ok(Self::Index(Atom::Str(index).into()))))
            }
            // just a dot
            _ => Box::new(empty()) as Box<dyn Iterator<Item = _>>,
        };
        index.chain(iter.map(PathElem::from_range))
    }

    fn from_index(pair: Pair<Rule>) -> (&str, bool) {
        let mut iter = pair.into_inner();
        let index = iter.next().unwrap().into_inner().next().unwrap().as_str();
        let question = iter.next().is_some();
        assert_eq!(iter.next(), None);
        (index, question)
    }

    fn from_range(pair: Pair<Rule>) -> Result<Self, Error> {
        //println!("range: {:?}", pair.as_rule());
        match pair.into_inner().next() {
            None => Ok(Self::Range(None, None)),
            Some(range) => match range.as_rule() {
                Rule::at => Ok(Self::Index(Filter::try_from(range.into_inner())?)),
                Rule::from => Ok(Self::Range(
                    Some(Filter::try_from(range.into_inner())?),
                    None,
                )),
                Rule::until => Ok(Self::Range(
                    None,
                    Some(Filter::try_from(range.into_inner())?),
                )),
                Rule::from_until => {
                    let mut iter = range.into_inner().map(Filter::try_from);
                    let from = iter.next().unwrap();
                    let until = iter.next().unwrap();
                    assert!(iter.next().is_none());
                    Ok(Self::Range(Some(from?), Some(until?)))
                }
                _ => unreachable!(),
            },
        }
    }
}

impl TryFrom<Rule> for LogicOp {
    type Error = ();
    fn try_from(rule: Rule) -> Result<Self, Self::Error> {
        match rule {
            Rule::or => Ok(LogicOp::Or),
            Rule::and => Ok(LogicOp::And),
            Rule::eq => Ok(LogicOp::Eq),
            Rule::ne => Ok(LogicOp::Ne),
            Rule::gt => Ok(LogicOp::Gt),
            Rule::ge => Ok(LogicOp::Ge),
            Rule::lt => Ok(LogicOp::Lt),
            Rule::le => Ok(LogicOp::Le),
            _ => Err(()),
        }
    }
}

impl TryFrom<Rule> for MathOp {
    type Error = ();
    fn try_from(rule: Rule) -> Result<Self, Self::Error> {
        match rule {
            Rule::add => Ok(MathOp::Add),
            Rule::sub => Ok(MathOp::Sub),
            Rule::mul => Ok(MathOp::Mul),
            Rule::div => Ok(MathOp::Div),
            Rule::rem => Ok(MathOp::Rem),
            _ => Err(()),
        }
    }
}

impl TryFrom<(&str, [Box<Filter>; 0])> for Filter {
    type Error = Error;
    fn try_from((name, []): (&str, [Box<Filter>; 0])) -> Result<Self, Error> {
        match name {
            "empty" => Ok(Self::Ref(Ref::Empty)),
            "any" => Ok(Self::New(NewFilter::Function(NewFunc::Any))),
            "all" => Ok(Self::New(NewFilter::Function(NewFunc::All))),
            "not" => Ok(Self::New(NewFilter::Function(NewFunc::Not))),
            "length" => Ok(Self::New(NewFilter::Function(NewFunc::Length))),
            "type" => Ok(Self::New(NewFilter::Function(NewFunc::Type))),
            "add" => Ok(Self::New(NewFilter::Function(NewFunc::Add))),
            _ => Err(Error::Undefined(name.to_string(), 0)),
        }
    }
}

impl TryFrom<(&str, [Box<Filter>; 1])> for Filter {
    type Error = Error;
    fn try_from((name, [arg1]): (&str, [Box<Filter>; 1])) -> Result<Self, Error> {
        match name {
            "first" => Ok(Self::Ref(Ref::First(arg1))),
            "last" => Ok(Self::Ref(Ref::Last(arg1))),
            "map" => Ok(Self::New(NewFilter::Function(NewFunc::Map(arg1)))),
            "select" => Ok(Self::Ref(Ref::select(arg1))),
            "recurse" => Ok(Self::Ref(Ref::Recurse(arg1))),
            _ => Err(Error::Undefined(name.to_string(), 1)),
        }
    }
}

impl TryFrom<(&str, [Box<Filter>; 2])> for Filter {
    type Error = Error;
    fn try_from((name, [arg1, arg2]): (&str, [Box<Filter>; 2])) -> Result<Self, Error> {
        match name {
            "limit" => Ok(Self::Ref(Ref::Limit(arg1, arg2))),
            "nth" => Ok(Self::Ref(Ref::nth(arg1, arg2))),
            _ => Err(Error::Undefined(name.to_string(), 2)),
        }
    }
}

impl TryFrom<(&str, [Box<Filter>; 3])> for Filter {
    type Error = Error;
    fn try_from((name, [arg1, arg2, arg3]): (&str, [Box<Filter>; 3])) -> Result<Self, Error> {
        match name {
            "fold" => Ok(Self::Ref(Ref::Fold(arg1, arg2, arg3))),
            _ => Err(Error::Undefined(name.to_string(), 3)),
        }
    }
}

impl TryFrom<(&str, Vec<Filter>)> for Filter {
    type Error = Error;
    fn try_from((name, args): (&str, Vec<Filter>)) -> Result<Self, Error> {
        let mut args = args.into_iter().map(Box::new);
        if let Some(arg1) = args.next() {
            // unary or higher-arity function
            if let Some(arg2) = args.next() {
                // binary or higher-arity function
                if let Some(arg3) = args.next() {
                    // ternary or higher-arity function
                    if let Some(_arg4) = args.next() {
                        // quaternary or higher-arity function
                        Err(Error::Undefined(name.to_string(), 4 + args.len()))
                    } else {
                        // ternary function
                        (name, [arg1, arg2, arg3]).try_into()
                    }
                } else {
                    // binary function
                    (name, [arg1, arg2]).try_into()
                }
            } else {
                // unary function
                (name, [arg1]).try_into()
            }
        } else {
            // nullary function
            (name, []).try_into()
        }
    }
}
