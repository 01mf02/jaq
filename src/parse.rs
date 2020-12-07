use crate::filter::{Atom, Filter};
use crate::functions::Function;
use crate::ops::{LogicOp, MathOp};
use crate::path::PathElem;
use core::convert::TryFrom;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::PrecClimber;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct FilterParser;

impl Filter {
    pub fn parse(s: &str) -> Result<Self, pest::error::Error<Rule>> {
        Ok(Self::from(FilterParser::parse(Rule::main, s)?))
    }
}

lazy_static::lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use pest::prec_climber::{Operator, Assoc::*};

        PrecClimber::new(vec![
            Operator::new(pipe, Left),
            Operator::new(comma, Left),
            Operator::new(or, Left),
            Operator::new(and, Left),
            Operator::new(eq, Left) | Operator::new(ne, Left),
            Operator::new(gt, Left) | Operator::new(ge, Left) | Operator::new(lt, Left) | Operator::new(le, Left),
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left),
            Operator::new(rem, Left)
        ])
    };
}

impl From<Pairs<'_, Rule>> for Filter {
    fn from(pairs: Pairs<Rule>) -> Self {
        PREC_CLIMBER.climb(
            pairs,
            |pair: Pair<Rule>| Filter::from(pair),
            |lhs: Filter, op: Pair<Rule>, rhs: Filter| {
                let lhs = Box::new(lhs);
                let rhs = Box::new(rhs);
                match op.as_rule() {
                    Rule::pipe => Filter::Pipe(lhs, rhs),
                    Rule::comma => Filter::Comma(lhs, rhs),
                    rule => {
                        if let Ok(op) = LogicOp::try_from(rule) {
                            Filter::Logic(lhs, op, rhs)
                        } else if let Ok(op) = MathOp::try_from(rule) {
                            Filter::Math(lhs, op, rhs)
                        } else {
                            unreachable!()
                        }
                    }
                }
            },
        )
    }
}

impl From<Pair<'_, Rule>> for Filter {
    fn from(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::expr => Self::from(pair.into_inner()),
            Rule::atom => Filter::Atom(pair.into_inner().next().unwrap().into()),
            Rule::array => {
                let inner = pair.into_inner();
                let contents = if inner.peek().is_none() {
                    Filter::Empty
                } else {
                    Self::from(inner)
                };
                Filter::Array(Box::new(contents))
            }
            Rule::object => {
                let contents = pair.into_inner().map(|kv| {
                    let mut iter = kv.into_inner();
                    let key = iter.next().unwrap();
                    let key = match key.as_rule() {
                        Rule::identifier => Filter::Atom(Atom::Str(key.as_str().to_string())),
                        Rule::string => Filter::Atom(Atom::from(key)),
                        Rule::expr => Filter::from(key),
                        _ => unreachable!(),
                    };
                    let value = match iter.next() {
                        Some(value) => Filter::from(value),
                        None => todo!(),
                    };
                    assert_eq!(iter.next(), None);
                    (key, value)
                });
                Filter::Object(contents.collect())
            }
            Rule::ite => {
                let mut ite = pair.into_inner().map(|p| Box::new(Self::from(p)));
                let cond = ite.next().unwrap();
                let truth = ite.next().unwrap();
                let falsity = ite.next().unwrap();
                assert!(ite.next().is_none());
                Filter::IfThenElse(cond, truth, falsity)
            }
            Rule::function => {
                let mut iter = pair.into_inner();
                let name = iter.next().unwrap().as_str();
                let args = match iter.next() {
                    None => Box::new(core::iter::empty()) as Box<dyn Iterator<Item = _>>,
                    Some(args) => Box::new(args.into_inner().map(Self::from)),
                };
                assert_eq!(iter.next(), None);
                Filter::Function(Function::from(name, args).unwrap())
            }
            Rule::path => Filter::Path(pair.into_inner().flat_map(PathElem::from_path).collect()),
            _ => unreachable!(),
        }
    }
}

impl From<Pair<'_, Rule>> for Atom {
    fn from(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::null => Atom::Null,
            Rule::boole => Atom::Bool(pair.as_str().parse::<bool>().unwrap()),
            Rule::number => Atom::Num(pair.as_str().parse::<f64>().unwrap()),
            Rule::string => Atom::Str(pair.into_inner().next().unwrap().as_str().to_string()),
            _ => unreachable!(),
        }
    }
}

impl PathElem {
    fn from_path(pair: Pair<Rule>) -> impl Iterator<Item = Self> + '_ {
        use core::iter::{empty, once};
        let mut iter = pair.into_inner();
        let index = iter.next().unwrap();
        let index = match index.as_rule() {
            Rule::path_index => {
                let index = Self::from_index(index).0.to_string();
                Box::new(once(Self::Index(Filter::Atom(Atom::Str(index)))))
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

    fn from_range(pair: Pair<Rule>) -> PathElem {
        println!("range: {:?}", pair.as_rule());
        match pair.into_inner().next() {
            None => Self::Range(None, None),
            Some(range) => match range.as_rule() {
                Rule::at => Self::Index(Filter::from(range.into_inner())),
                Rule::from => Self::Range(Some(Filter::from(range.into_inner())), None),
                Rule::until => Self::Range(None, Some(Filter::from(range.into_inner()))),
                Rule::from_until => {
                    let mut iter = range.into_inner().map(|r| Some(Filter::from(r)));
                    let from = iter.next().unwrap();
                    let until = iter.next().unwrap();
                    assert!(iter.next().is_none());
                    Self::Range(from, until)
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

impl Function {
    fn from(name: &str, mut args: impl Iterator<Item = Filter>) -> Option<Self> {
        if let Some(_) = args.next() {
            // unary or higher-arity function
            None
        } else {
            // nullary function
            match name {
                "empty" => Some(Self::Empty),
                "any" => Some(Self::Any),
                "all" => Some(Self::All),
                "not" => Some(Self::Not),
                "length" => Some(Self::Length),
                "add" => Some(Self::Add),
                _ => None,
            }
        }
    }
}
