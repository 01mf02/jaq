use crate::path::PathElem;
use crate::{Filter, Path};
use alloc::{boxed::Box, string::String, vec::Vec};
use jaq_parse::filter::{AssignOp, BinaryOp, Expr, KeyVal};
use jaq_parse::{Error, Spanned};

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
                    let err = "cannot interpret as floating-point number";
                    errs.push(Error::custom(body.1, err));
                    Filter::Float(0.)
                }
            } else if let Ok(f) = n.parse::<usize>() {
                Filter::Pos(f)
            } else {
                let err = "cannot interpret as machine-size integer";
                errs.push(Error::custom(body.1, err));
                Filter::Pos(0)
            }
        }
        Expr::Str(s) => Filter::Str(s),
        Expr::Array(a) => Filter::Array(a.map(|a| get(*a, errs))),
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
                    errs.push(Error::custom(body.1, "could not find function"));
                    Filter::Path(Path(Vec::new()))
                });
                let args = args.into_iter().map(|arg| *get(arg, errs));
                fun.subst(&args.collect::<Vec<_>>())
            }
        },
        Expr::Neg(f) => Filter::Neg(get(*f, errs)),
        Expr::Binary(l, BinaryOp::Pipe, r) => Filter::Pipe(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Comma, r) => Filter::Comma(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Or, r) => Filter::Logic(get(*l, errs), true, get(*r, errs)),
        Expr::Binary(l, BinaryOp::And, r) => Filter::Logic(get(*l, errs), false, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Math(op), r) => Filter::Math(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Ord(op), r) => Filter::Ord(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Assign(op), r) => {
            let l_span = l.1.clone();
            let (l, r) = (get(*l, errs), get(*r, errs));
            let l = if let Filter::Path(path) = *l {
                path
            } else {
                let err = "left-hand side of assignment must be a path";
                errs.push(Error::custom(l_span, err));
                Path(Vec::new())
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
            use jaq_parse::path::Part;
            let path = path.into_iter().map(|(p, opt)| match p {
                Part::Index(i) => (PathElem::Index(*get(i, errs)), opt),
                Part::Range(lower, upper) => {
                    let lower = lower.map(|f| *get(f, errs));
                    let upper = upper.map(|f| *get(f, errs));
                    (PathElem::Range(lower, upper), opt)
                }
            });
            Filter::Path(Path(path.collect()))
        }
    }
}
