use crate::ops::LogicOp;
use crate::path::PathElem;
use crate::{Error, Filter, Path};
use alloc::{boxed::Box, string::String, vec::Vec};
use jaq_parse::parse::{AssignOp, BinaryOp, Expr, KeyVal, PathComponent, Spanned};

pub fn unparse<F>(fns: &F, vars: &[String], body: Spanned<Expr>, errs: &mut Vec<Error>) -> Filter
where
    F: Fn(&(String, usize)) -> Option<Filter>,
{
    let get = |f, errs: &mut _| Box::new(unparse(fns, vars, f, errs));
    match body.0 {
        Expr::Num(n) => {
            if n.contains(['.', 'e', 'E']) {
                if let Ok(f) = n.parse::<f64>() {
                    Filter::Float(f)
                } else {
                    errs.push(todo!());
                    Filter::Empty
                }
            } else {
                if let Ok(f) = n.parse::<usize>() {
                    Filter::Pos(f)
                } else {
                    errs.push(todo!());
                    Filter::Empty
                }
            }
        }
        Expr::Str(s) => Filter::Str(s),
        Expr::Array(Some(a)) => Filter::Array(get(*a, errs)),
        Expr::Array(None) => Filter::Array(Box::new(Filter::Empty)),
        Expr::Object(o) => {
            let kvs = o.into_iter().map(|kv| match kv {
                KeyVal::Expr(k, v) => (*get(k, errs), *get(v, errs)),
                KeyVal::Str(k, v) => {
                    let v = match v {
                        None => Filter::Path(Path::from(PathElem::Index(Filter::Str(k.clone())))),
                        Some(v) => *get(v, errs),
                    };
                    (Filter::Str(k), v)
                }
            });
            Filter::Object(kvs.collect())
        }

        Expr::Call(name, args) => match vars.iter().position(|v| *v == name) {
            Some(pos) if args.is_empty() => Filter::Var(pos),
            _ => {
                let fun = fns(&(name, args.len())).unwrap_or_else(|| {
                    errs.push(todo!());
                    Filter::Empty
                });
                let args = args.into_iter().map(|arg| *get(arg, errs));
                fun.subst(&args.collect::<Vec<_>>())
            }
        },
        Expr::Neg(f) => Filter::Neg(get(*f, errs)),
        Expr::Binary(l, BinaryOp::Pipe, r) => Filter::Pipe(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Comma, r) => Filter::Comma(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Or, r) => {
            Filter::Logic(get(*l, errs), LogicOp::Or, get(*r, errs))
        }
        Expr::Binary(l, BinaryOp::And, r) => {
            Filter::Logic(get(*l, errs), LogicOp::And, get(*r, errs))
        }
        Expr::Binary(l, BinaryOp::Math(op), r) => Filter::Math(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Ord(op), r) => Filter::Ord(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Assign(op), r) => {
            let (l, r) = (get(*l, errs), get(*r, errs));
            let l = match *l {
                Filter::Path(path) => path,
                _ => todo!(),
            };
            match op {
                AssignOp::Assign => Filter::Assign(l, r),
                AssignOp::Update => Filter::Update(l, r),
                AssignOp::UpdateWith(op) => Filter::update_math(l, op, *r),
            }
        }
        Expr::If(if_, then, else_) => {
            Filter::IfThenElse(get(*if_, errs), get(*then, errs), get(*else_, errs))
        }
        Expr::Path(path) => {
            let path = path.into_iter().map(|(p, opt)| match p {
                PathComponent::Index(i) => (PathElem::Index(*get(i, errs)), opt),
                PathComponent::Range(lower, upper) => {
                    let lower = lower.map(|f| *get(f, errs));
                    let upper = upper.map(|f| *get(f, errs));
                    (PathElem::Range(lower, upper), opt)
                }
            });
            Filter::Path(Path(path.collect()))
        }
    }
}
