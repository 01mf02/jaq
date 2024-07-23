use crate::filter::{AssignOp, BinaryOp, Filter, Fold, FoldType, KeyVal};
use crate::prec_climb::{self, Associativity};
use crate::{parse, Arg, Call, Def, Main, MathOp, OrdOp, Span, Spanned};
use alloc::string::ToString;
use alloc::{boxed::Box, vec::Vec};

impl parse::Term<&str> {
    fn span(&self, code: &str) -> Span {
        match self {
            Self::Num(s) | Self::Call(s, ..) | Self::Var(s) => crate::lex::span(code, s),
            _ => 0..42,
        }
    }

    fn conv(&self, s: &str) -> Filter {
        use crate::lex::StrPart;
        use crate::path::{Opt, Part};
        use crate::string;
        use Filter::*;

        let span = |tm: &Self| Box::new((tm.conv(s), tm.span(s)));
        let from_part = |(part, opt): &(Part<_>, Opt)| {
            let part = match part {
                Part::Index(i) => Part::Index(*span(i)),
                Part::Range(l, h) => {
                    Part::Range(l.as_ref().map(|l| *span(l)), h.as_ref().map(|h| *span(h)))
                }
            };
            (part, *opt)
        };
        let index_path = |k| {
            let path = Vec::from([(Part::Index(k), Opt::Essential)]);
            Filter::Path(span(&Self::Id), path)
        };
        let from_str = |part: &StrPart<&str, _>| match part {
            StrPart::Str(s) => string::Part::Str(s.to_string()),
            StrPart::Filter(tm) => string::Part::Fun(*span(tm)),
            StrPart::Char(c) => string::Part::Str(c.to_string()),
        };
        let from_obj = |(k, v): &(_, Option<_>)| {
            let f = || (index_path(*span(k)), 0..42);
            let (k, v) = if let (Self::Var(x), None) = (k, v) {
                (*span(&Self::str(&x[1..])), *span(k))
            } else {
                (*span(k), v.as_ref().map_or_else(f, |v| *span(v)))
            };
            KeyVal::Filter(k, v)
        };
        let from_op = |op| match op {
            "," => BinaryOp::Comma,
            "//" => BinaryOp::Alt,
            "or" => BinaryOp::Or,
            "and" => BinaryOp::And,
            "+" => BinaryOp::Math(MathOp::Add),
            "-" => BinaryOp::Math(MathOp::Sub),
            "*" => BinaryOp::Math(MathOp::Mul),
            "/" => BinaryOp::Math(MathOp::Div),
            "%" => BinaryOp::Math(MathOp::Rem),
            "=" => BinaryOp::Assign(AssignOp::Assign),
            "|=" => BinaryOp::Assign(AssignOp::Update),
            "+=" => BinaryOp::Assign(AssignOp::UpdateWith(MathOp::Add)),
            "-=" => BinaryOp::Assign(AssignOp::UpdateWith(MathOp::Sub)),
            "*=" => BinaryOp::Assign(AssignOp::UpdateWith(MathOp::Mul)),
            "/=" => BinaryOp::Assign(AssignOp::UpdateWith(MathOp::Div)),
            "%=" => BinaryOp::Assign(AssignOp::UpdateWith(MathOp::Rem)),
            "<" => BinaryOp::Ord(OrdOp::Lt),
            ">" => BinaryOp::Ord(OrdOp::Gt),
            "<=" => BinaryOp::Ord(OrdOp::Le),
            ">=" => BinaryOp::Ord(OrdOp::Ge),
            "==" => BinaryOp::Ord(OrdOp::Eq),
            "!=" => BinaryOp::Ord(OrdOp::Ne),
            op => panic!("unknown operator: {op}"),
        };
        match self {
            Self::Id => Id,
            Self::Recurse => Recurse,
            Self::Num(n) => Num(n.to_string()),
            Self::Str(fmt, parts) => Str(Box::new(crate::Str {
                fmt: fmt.map(|fmt| span(&Self::Call(fmt, Vec::new()))),
                parts: parts.iter().map(from_str).collect(),
            })),
            Self::Arr(a) => Array(a.as_deref().map(span)),
            Self::Obj(o) => Object(o.iter().map(from_obj).collect()),
            Self::Neg(tm) => Neg(span(tm)),
            Self::Pipe(l, v, r) => Binary(
                span(l),
                BinaryOp::Pipe(v.map(|v| v[1..].to_string())),
                span(r),
            ),
            Self::BinOp(head, tail) => {
                let head = *span(head);
                let tail = tail.iter().map(|(op, tm)| (from_op(op), *span(tm)));
                prec_climb::climb(head, tail).0
            }

            Self::Label(_v, ..) | Self::Break(_v) => {
                unimplemented!("label-break is not supported yet")
            }

            Self::Fold(fold, xs, v, args) => {
                let fold_type = match *fold {
                    "reduce" => FoldType::Reduce,
                    "foreach" => FoldType::Foreach,
                    "for" => FoldType::For,
                    _ => panic!(),
                };
                let (init, update) = match &args[..] {
                    [init, update] => (init, update),
                    _ => unimplemented!("folding filters currently only take two arguments"),
                };
                let fold = self::Fold {
                    xs: span(xs),
                    x: v[1..].to_string(),
                    init: span(init),
                    f: span(update),
                };
                Fold(fold_type, fold)
            }
            Self::TryCatch(try_, catch) => TryCatch(span(try_), catch.as_deref().map(span)),
            Self::IfThenElse(if_thens, else_) => Ite(
                if_thens
                    .iter()
                    .map(|(if_, then_)| (*span(if_), *span(then_)))
                    .collect(),
                else_.as_deref().map(span),
            ),

            Self::Def(_defs, _tm) => {
                unimplemented!("definitions inside terms are not supported yet")
            }
            Self::Call(c, args) => Call(c.to_string(), args.iter().map(|a| *span(a)).collect()),
            Self::Var(v) => Var(v[1..].to_string()),

            Self::Path(tm, path) => Path(span(tm), path.iter().map(from_part).collect()),
        }
    }

    fn conv_main(&self, s: &str) -> Main {
        match self {
            parse::Term::Def(defs, tm) => Main {
                defs: defs.iter().map(|def| def.conv(s)).collect(),
                body: (tm.conv(s), tm.span(s)),
            },
            tm => Main {
                defs: Vec::new(),
                body: (tm.conv(s), tm.span(s)),
            },
        }
    }
}

impl From<&parse::Term<&str>> for Filter {
    fn from(tm: &parse::Term<&str>) -> Self {
        tm.conv("")
    }
}

impl prec_climb::Op for BinaryOp {
    fn precedence(&self) -> usize {
        match self {
            Self::Pipe(_) => 0,
            Self::Comma => 1,
            Self::Assign(_) => 2,
            Self::Alt => 3,
            Self::Or => Self::Alt.precedence() + 1,
            Self::And => Self::Or.precedence() + 1,
            Self::Ord(OrdOp::Eq | OrdOp::Ne) => Self::And.precedence() + 1,
            Self::Ord(OrdOp::Lt | OrdOp::Gt | OrdOp::Le | OrdOp::Ge) => Self::And.precedence() + 2,
            Self::Math(MathOp::Add | MathOp::Sub) => Self::And.precedence() + 3,
            Self::Math(MathOp::Mul | MathOp::Div) => Self::Math(MathOp::Add).precedence() + 1,
            Self::Math(MathOp::Rem) => Self::Math(MathOp::Mul).precedence() + 1,
        }
    }

    fn associativity(&self) -> Associativity {
        match self {
            Self::Pipe(_) | Self::Assign(_) => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

impl prec_climb::Expr<BinaryOp> for Spanned<Filter> {
    fn from_op(lhs: Self, op: BinaryOp, rhs: Self) -> Self {
        Filter::binary(lhs, op, rhs)
    }
}

impl parse::Def<&str, parse::Term<&str>> {
    fn conv(&self, s: &str) -> Def {
        let args = self.args.iter().map(|arg| {
            if let Some(v) = arg.strip_prefix('$') {
                Arg::Var(v.to_string())
            } else {
                Arg::Fun(arg.to_string())
            }
        });
        Def {
            lhs: Call {
                name: self.name.to_string(),
                args: args.collect(),
            },
            rhs: self.body.conv_main(s),
        }
    }
}

impl parse::Module<&str, Vec<parse::Def<&str, parse::Term<&str>>>> {
    /// Convert a definitions module to a [`Def`] vector.
    pub fn conv(&self, s: &str) -> Vec<Def> {
        self.body.iter().map(|def| def.conv(s)).collect()
    }
}

impl parse::Module<&str, parse::Term<&str>> {
    /// Convert a term module to a [`Main`].
    pub fn conv(&self, s: &str) -> Main {
        if !self.mods.is_empty() {
            panic!("include / import is not supported yet");
        }
        self.body.conv_main(s)
    }
}
