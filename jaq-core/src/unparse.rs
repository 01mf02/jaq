use crate::filter2::Filter;
use crate::ops::{LogicOp, MathOp, OrdOp};
use crate::{Error, Path, RValR, RValRs, Val, ValRs};
use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use jaq_parse::parse::{Expr, Spanned};

fn unparse<F>(fns: F, vars: &[String], body: Spanned<Expr>, errs: &mut Vec<Error>) -> Filter
where
    F: Fn(&(String, usize)) -> Option<Filter>,
{
    match body.0 {
        Expr::Num(n) => {
            if n.contains(['.', 'e', 'E']) {
                if let Ok(f) = n.parse::<f64>() {
                    Filter::Float(f)
                } else {
                    errs.push(todo!());
                    Filter::Float(0.)
                }
            } else {
                if let Ok(f) = n.parse::<usize>() {
                    Filter::Pos(f)
                } else {
                    errs.push(todo!());
                    Filter::Pos(0)
                }
            }
        }
        Expr::Call(name, args) => {
            todo!()
        }
        _ => todo!(),
    }
}
