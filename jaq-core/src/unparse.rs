use crate::filter::Filter;
use crate::path::{self, Path};
use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use jaq_parse::filter::{AssignOp, BinaryOp, Filter as Expr, KeyVal};
use jaq_parse::{Error, Spanned};

pub fn unparse<F>(
    fns: &F,
    args: &[String],
    mut vars: Vec<String>,
    body: Spanned<Expr>,
    errs: &mut Vec<Error>,
) -> Filter
where
    F: Fn(&(String, usize)) -> Option<Filter>,
{
    let get = |f, errs: &mut _| Box::new(unparse(fns, args, vars.clone(), f, errs));
    let mut call = |name, args: Vec<Spanned<Expr>>| {
        let fun = fns(&(name, args.len())).unwrap_or_else(|| {
            errs.push(Error::custom(body.1.clone(), "could not find function"));
            Filter::Id
        });
        let args = args.into_iter().map(|arg| *get(arg, errs));
        fun.subst(&args.collect::<Vec<_>>())
    };
    match body.0 {
        Expr::Id => Filter::Id,
        Expr::Num(n) => {
            if n.contains(['.', 'e', 'E']) {
                if let Ok(f) = n.parse::<f64>() {
                    Filter::Float(f)
                } else {
                    let err = "cannot interpret as floating-point number";
                    errs.push(Error::custom(body.1, err));
                    Filter::Float(0.)
                }
            } else if let Ok(f) = n.parse::<isize>() {
                Filter::Int(f)
            } else {
                let err = "cannot interpret as machine-size integer";
                errs.push(Error::custom(body.1, err));
                Filter::Int(0)
            }
        }
        Expr::Str(s) => Filter::Str(Rc::new(s)),
        Expr::Var(v) => match vars.iter().rev().position(|i| *i == v) {
            None => {
                errs.push(Error::custom(body.1, "undefined variable"));
                Filter::Var(0)
            }
            Some(v) => Filter::Var(v),
        },
        Expr::Array(a) => Filter::Array(a.map(|a| get(*a, errs))),
        Expr::Object(o) => {
            let kvs = o.into_iter().map(|kv| match kv {
                KeyVal::Filter(k, v) => (*get(k, errs), *get(v, errs)),
                KeyVal::Str(k, v) => {
                    let k = Filter::Str(Rc::new(k));
                    let v = match v {
                        None => Filter::Path(
                            Box::new(Filter::Id),
                            Path::from(path::Part::Index(k.clone())),
                        ),
                        Some(v) => *get(v, errs),
                    };
                    (k, v)
                }
            });
            Filter::Object(kvs.collect())
        }
        Expr::Call(name, call_args) => match args.iter().rposition(|v| *v == name) {
            Some(pos) if call_args.is_empty() => {
                let arg = Filter::Arg(pos);
                if vars.is_empty() {
                    arg
                } else {
                    Filter::SkipCtx(vars.len(), Box::new(arg))
                }
            }
            _ => call(name, call_args),
        },
        Expr::Try(f) => Filter::Try(get(*f, errs)),
        Expr::Neg(f) => Filter::Neg(get(*f, errs)),
        Expr::Binary(l, BinaryOp::Pipe(None), r) => {
            Filter::Pipe(get(*l, errs), false, get(*r, errs))
        }
        Expr::Binary(l, BinaryOp::Pipe(Some(v)), r) => {
            let l = get(*l, errs);
            vars.push(v);
            let r = Box::new(unparse(fns, args, vars, *r, errs));
            Filter::Pipe(l, true, r)
        }
        Expr::Reduce(xs, v, init, f) => {
            let xs = get(*xs, errs);
            let init = get(*init, errs);
            vars.push(v);
            let f = Box::new(unparse(fns, args, vars, *f, errs));
            Filter::Reduce(xs, init, f)
        }
        Expr::Binary(l, BinaryOp::Comma, r) => Filter::Comma(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Alt, r) => Filter::Alt(get(*l, errs), get(*r, errs)),
        Expr::Binary(l, BinaryOp::Or, r) => Filter::Logic(get(*l, errs), true, get(*r, errs)),
        Expr::Binary(l, BinaryOp::And, r) => Filter::Logic(get(*l, errs), false, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Math(op), r) => Filter::Math(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Ord(op), r) => Filter::Ord(get(*l, errs), op, get(*r, errs)),
        Expr::Binary(l, BinaryOp::Assign(op), r) => {
            let (l, r) = (get(*l, errs), get(*r, errs));
            match op {
                AssignOp::Assign => Filter::Assign(l, r),
                AssignOp::Update => Filter::Update(l, r),
                AssignOp::UpdateWith(op) => Filter::update_math(l, op, r),
            }
        }
        Expr::If(if_thens, else_) => Filter::IfThenElse(
            if_thens
                .into_iter()
                .map(|(if_, then)| (*get(if_, errs), *get(then, errs)))
                .collect(),
            get(*else_, errs),
        ),
        Expr::Path(f, path) => {
            let f = get(*f, errs);
            use jaq_parse::path::Part;
            let path = path.into_iter().map(|(p, opt)| match p {
                Part::Index(i) => (path::Part::Index(*get(i, errs)), opt),
                Part::Range(lower, upper) => {
                    let lower = lower.map(|f| *get(f, errs));
                    let upper = upper.map(|f| *get(f, errs));
                    (path::Part::Range(lower, upper), opt)
                }
            });
            Filter::Path(f, Path(path.collect()))
        }
    }
}
