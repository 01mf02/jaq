use crate::filter::{New, Ref};
use crate::preprocess::{Call, PreFilter};
use crate::val::Atom;
use jaq_parse::parse::{Expr, KeyVal, Spanned};

#[derive(Debug)]
pub enum Error {}

impl TryFrom<Expr> for PreFilter {
    type Error = Error;
    fn try_from(expr: Expr) -> Result<Self, Error> {
        match expr {
            Expr::Num(n) => {
                let atom = if n.contains('.') {
                    Atom::Float(n.parse::<f64>().unwrap())
                } else {
                    Atom::Pos(n.parse::<usize>().unwrap())
                };
                Ok(Self::New(New::Atom(atom)))
            }
            Expr::Str(s) => Ok(Self::New(New::Atom(Atom::Str(s)))),
            Expr::Call(f, args) => {
                let args: Result<_, _> = args.into_iter().map(|a| Self::try_from(a)).collect();
                Ok(Self::Named(Call::new(f, args?)))
            }
            Expr::If(if_, then, else_) => {
                let if_ = Box::new(Self::try_from(*if_)?);
                let then = Box::new(Self::try_from(*then)?);
                let else_ = Box::new(Self::try_from(*else_)?);
                Ok(Self::Ref(Ref::IfThenElse(if_, then, else_)))
            }
            Expr::Array(e) => {
                let contents = match e {
                    None => Self::Ref(Ref::Empty),
                    Some(e) => Self::try_from(*e)?,
                };
                Ok(Self::New(New::Array(Box::new(contents))))
            }
            Expr::Object(kvs) => {
                let kvs = kvs.into_iter().map(|kv| match kv {
                    KeyVal::Expr(k, v) => Ok((Self::try_from(k)?, Self::try_from(v)?)),
                    KeyVal::Str(k, v) => {
                        let v = match v {
                            None => Self::New(New::Atom(Atom::Str(k.clone()))),
                            Some(v) => Self::try_from(v)?,
                        };
                        let k = Self::New(New::Atom(Atom::Str(k)));
                        Ok((k, v))
                    }
                });
                Ok(Self::New(New::Object(kvs.collect::<Result<_, _>>()?)))
            }
            _ => todo!(),
        }
    }
}

impl TryFrom<Spanned<Expr>> for PreFilter {
    type Error = Error;
    fn try_from(expr: Spanned<Expr>) -> Result<Self, Error> {
        Self::try_from(expr.0)
    }
}
