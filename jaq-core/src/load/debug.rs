use super::lex::StrPart;
use super::parse::{Def, Pattern, Term};
use core::fmt::{self, Debug};

struct Into<T>(T);

impl<T: Debug> Debug for Into<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)?;
        write!(f, ".into()")
    }
}

struct Apply<T>(&'static str, T);

impl<T: Debug> Debug for Apply<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple(self.0).field(&self.1).finish()
    }
}

impl<S: Debug, F: Debug> Debug for Def<S, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("parse::Def")
            .field("name", &self.name)
            .field("args", &Into(&self.args))
            .field("body", &self.body)
            .finish()
    }
}

impl<S: Debug> Debug for Term<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Term::*;
        match self {
            Id => f.debug_tuple("Id").finish(),
            Recurse => f.debug_tuple("Recurse").finish(),

            Num(s) => f.debug_tuple("Num").field(s).finish(),
            Str(fmt, parts) => f.debug_tuple("Str").field(fmt).field(&Into(parts)).finish(),
            Arr(a) => f.debug_tuple("Arr").field(&a.as_ref().map(Into)).finish(),
            Obj(o) => f.debug_tuple("Obj").field(&Into(o)).finish(),
            Neg(t) => f.debug_tuple("Neg").field(&Into(t)).finish(),
            Pipe(l, pat, r) => {
                let (l, r) = (&Into(l), &Into(r));
                f.debug_tuple("Pipe").field(l).field(pat).field(r).finish()
            }
            BinOp(l, op, r) => {
                let (l, r) = (&Into(l), &Into(r));
                f.debug_tuple("BinOp").field(l).field(op).field(r).finish()
            }
            Label(x, t) => f.debug_tuple("Label").field(x).field(&Into(t)).finish(),
            Break(x) => f.debug_tuple("Break").field(x).finish(),
            Fold(x, xs, pat, args) => f
                .debug_tuple("Fold")
                .field(x)
                .field(&Into(xs))
                .field(pat)
                .field(&Into(args))
                .finish(),
            TryCatch(try_, catch) => {
                let (t, c) = (Into(try_), catch.as_ref().map(Into));
                f.debug_tuple("TryCatch").field(&t).field(&c).finish()
            }
            IfThenElse(if_thens, else_) => {
                let (it, e) = (Into(if_thens), else_.as_ref().map(Into));
                f.debug_tuple("IfThenElse").field(&it).field(&e).finish()
            }
            Def(defs, t) => f
                .debug_tuple("Def")
                .field(&Into(defs))
                .field(&Into(t))
                .finish(),
            Call(x, args) => f.debug_tuple("Call").field(x).field(&Into(args)).finish(),
            Var(x) => f.debug_tuple("Var").field(x).finish(),
            Path(t, path) => {
                let path = Apply("path::Path", Into(&path.0));
                f.debug_tuple("Path").field(&Into(t)).field(&path).finish()
            }
        }
    }
}

impl<S: Debug> Debug for Pattern<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(x) => f.debug_tuple("Pattern::Var").field(x).finish(),
            Self::Arr(a) => f.debug_tuple("Pattern::Arr").field(&Into(a)).finish(),
            Self::Obj(o) => f.debug_tuple("Pattern::Obj").field(&Into(o)).finish(),
        }
    }
}

impl<S: Debug, T: Debug> Debug for StrPart<S, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Str(s) => f.debug_tuple("StrPart::Str").field(s).finish(),
            Self::Term(t) => f.debug_tuple("StrPart::Term").field(t).finish(),
            Self::Char(c) => f.debug_tuple("StrPart::Char").field(c).finish(),
        }
    }
}
