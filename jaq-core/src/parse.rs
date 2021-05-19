use crate::filter::{Filter, New, Ref};
use crate::ops::{LogicOp, MathOp, OrdOp};
use crate::path::{OnError, Path, PathElem};
use crate::preprocess::{Call, PreFilter};
use crate::toplevel::{Definition, Definitions, Main, Module};
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
    PathAssign,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Error::*;
        match self {
            Pest(e) => e.fmt(f),
            PathAssign => write!(f, "path expected before assignment operator"),
        }
    }
}

impl Main {
    pub fn parse(s: &str) -> Result<Self, Error> {
        Self::try_from(FilterParser::parse(Rule::main, s).map_err(Error::Pest)?)
    }
}

impl Module {
    pub fn parse(s: &str) -> Result<Self, Error> {
        Self::try_from(FilterParser::parse(Rule::module, s).map_err(Error::Pest)?)
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

impl TryFrom<Pairs<'_, Rule>> for Definition {
    type Error = Error;
    fn try_from(mut pairs: Pairs<Rule>) -> Result<Self, Error> {
        let name = pairs.next().unwrap().as_str().to_string();
        let args = pairs.next().unwrap();
        let args = args.into_inner().map(|a| a.as_str().to_string()).collect();
        let term = PreFilter::try_from(pairs.next().unwrap())?;
        Ok(Self { name, args, term })
    }
}

impl TryFrom<Pairs<'_, Rule>> for Definitions {
    type Error = Error;
    fn try_from(pairs: Pairs<Rule>) -> Result<Self, Error> {
        let defs = pairs.map(|pair| Definition::try_from(pair.into_inner()));
        Ok(Definitions::new(defs.collect::<Result<_, _>>()?))
    }
}

impl TryFrom<Pairs<'_, Rule>> for Main {
    type Error = Error;
    fn try_from(mut pairs: Pairs<Rule>) -> Result<Self, Error> {
        let defs = Definitions::try_from(pairs.next().unwrap().into_inner())?;
        let term = PreFilter::try_from(pairs.next().unwrap())?;
        Ok(Main { defs, term })
    }
}

impl TryFrom<Pairs<'_, Rule>> for Module {
    type Error = Error;
    fn try_from(mut pairs: Pairs<Rule>) -> Result<Self, Error> {
        let defs = Definitions::try_from(pairs.next().unwrap().into_inner());
        Ok(Self::new(defs?))
    }
}

impl TryFrom<Pairs<'_, Rule>> for PreFilter {
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
                        let path = match *lhs {
                            Filter::Ref(Ref::Path(p)) => p,
                            _ => return Err(Error::PathAssign),
                        };
                        let op = op.into_inner().next().unwrap();
                        Ok(Self::Ref(match op.as_rule() {
                            Rule::assign => Ref::Assign(path, rhs),
                            Rule::update => Ref::Update(path, rhs),
                            Rule::update_with => {
                                let math = op.into_inner().next().unwrap();
                                let math = MathOp::try_from(math.as_rule()).unwrap();
                                Ref::update_math(path, math, *rhs)
                            }
                            _ => unreachable!(),
                        }))
                    }
                    Rule::pipe => Ok(Self::Ref(Ref::Pipe(lhs, rhs))),
                    Rule::comma => Ok(Self::Ref(Ref::Comma(lhs, rhs))),
                    rule => {
                        if let Ok(op) = LogicOp::try_from(rule) {
                            Ok(Self::New(New::Logic(lhs, op, rhs)))
                        } else if let Ok(op) = OrdOp::try_from(rule) {
                            Ok(Self::New(New::Ord(lhs, op, rhs)))
                        } else if let Ok(op) = MathOp::try_from(rule) {
                            Ok(Self::New(New::Math(lhs, op, rhs)))
                        } else {
                            unreachable!()
                        }
                    }
                }
            },
        )
    }
}

impl TryFrom<Pair<'_, Rule>> for PreFilter {
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
                Ok(Self::New(New::Array(Box::new(contents))))
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
                                None => Self::Ref(Ref::Path(Path::from(PathElem::Index(
                                    key.clone().into(),
                                )))),
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
                Ok(Self::New(New::Object(kvs.collect::<Result<_, _>>()?)))
            }
            Rule::ite => {
                let mut ite = inner.map(|p| Self::try_from(p).map(Box::new));
                let cond = ite.next().unwrap();
                let truth = ite.next().unwrap();
                let falsity = ite.next().unwrap();
                assert!(ite.next().is_none());
                Ok(Self::Ref(Ref::IfThenElse(cond?, truth?, falsity?)))
            }
            Rule::call => {
                let name = inner.next().unwrap().as_str();
                let args = inner.next().unwrap();
                let args: Result<_, _> = args.into_inner().map(Self::try_from).collect();
                assert_eq!(inner.next(), None);
                Ok(Self::Named(Call::new(name.to_owned(), args?)))
            }
            Rule::path => Ok(Self::Ref(Ref::Path(Path::try_from(inner)?))),
            _ => unreachable!(),
        }
    }
}

impl From<Pair<'_, Rule>> for Atom {
    fn from(pair: Pair<Rule>) -> Self {
        use serde_json::Number;
        match pair.as_rule() {
            Rule::number => Self::Num(pair.as_str().parse::<Number>().unwrap().try_into().unwrap()),
            Rule::string => Self::Str(pair.into_inner().next().unwrap().as_str().to_string()),
            Rule::identifier => Self::Str(pair.as_str().to_string()),
            _ => unreachable!(),
        }
    }
}

impl TryFrom<Pairs<'_, Rule>> for Path<PreFilter> {
    type Error = Error;
    fn try_from(pairs: Pairs<Rule>) -> Result<Self, Error> {
        let path: Result<_, _> = pairs.flat_map(Self::from_segment).collect();
        Ok(Self::new(path?))
    }
}

impl From<Pair<'_, Rule>> for OnError {
    fn from(pair: Pair<Rule>) -> Self {
        if pair.as_rule() == Rule::optional {
            if pair.into_inner().next().is_some() {
                Self::Empty
            } else {
                Self::Fail
            }
        } else {
            unreachable!()
        }
    }
}

impl Path<PreFilter> {
    fn from_segment(
        pair: Pair<Rule>,
    ) -> impl Iterator<Item = Result<(PathElem<PreFilter>, OnError), Error>> + '_ {
        let mut iter = pair.into_inner();

        let index = PathElem::from_index(iter.next().unwrap());
        let optional = OnError::from(iter.next().unwrap());
        let index = index.map(|index| Ok((index, optional)));

        let ranges = iter.map(|range| {
            let mut iter = range.into_inner();
            let range = PathElem::from_range(iter.next().unwrap())?;
            let optional = OnError::from(iter.next().unwrap());
            assert!(iter.next().is_none());
            Ok((range, optional))
        });

        index.into_iter().chain(ranges)
    }
}

impl PathElem<PreFilter> {
    fn from_index(pair: Pair<Rule>) -> Option<Self> {
        match pair.as_rule() {
            Rule::dot_id | Rule::string => {
                let index = pair.into_inner().next().unwrap().as_str().to_string();
                Some(Self::Index(Atom::Str(index).into()))
            }
            Rule::dot => None,
            _ => unreachable!(),
        }
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
            _ => Err(()),
        }
    }
}

impl TryFrom<Rule> for OrdOp {
    type Error = ();
    fn try_from(rule: Rule) -> Result<Self, Self::Error> {
        match rule {
            Rule::eq => Ok(OrdOp::Eq),
            Rule::ne => Ok(OrdOp::Ne),
            Rule::gt => Ok(OrdOp::Gt),
            Rule::ge => Ok(OrdOp::Ge),
            Rule::lt => Ok(OrdOp::Lt),
            Rule::le => Ok(OrdOp::Le),
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
