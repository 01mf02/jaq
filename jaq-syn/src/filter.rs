//! Functions from values to streams of values.
use crate::{Call, MathOp, OrdOp, Path, Span, Spanned, Str};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Assignment operators, such as `=`, `|=` (update), and `+=`, `-=`, ...
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AssignOp {
    /// `=`
    Assign,
    /// `|=`
    Update,
    /// `+=`, `-=`, `*=`, `/=`, `%=`
    UpdateWith(MathOp),
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign => "=".fmt(f),
            Self::Update => "|=".fmt(f),
            Self::UpdateWith(op) => write!(f, "{op}="),
        }
    }
}

/// Binary operators, such as `|`, `,`, `//`, ...
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum BinaryOp {
    /// Application, i.e. `l | r` if no string is given, else `l as $x | r`
    Pipe(Option<String>),
    /// Concatenation, i.e. `l, r`
    Comma,
    /// Alternation, i.e. `l // r`
    Alt,
    /// Logical disjunction, i.e. `l or r`
    Or,
    /// Logical conjunction, i.e. `l and r`
    And,
    /// Arithmetic operation, e.g. `l + r`, `l - r`, ...
    Math(MathOp),
    /// Assignment, i.e. `l = r`, `l |= r`, `l += r`, `l -= r`, ...
    Assign(AssignOp),
    /// Ordering operation, e.g. `l == r`, `l <= r`, ...
    Ord(OrdOp),
}

/// An element of an object construction filter.
///
/// For example, the object construction filter `{(.): 1, b: 2}`
/// consists of two elements, namely `(.): 1` and `b: 2`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum KeyVal<T> {
    /// Both key and value are proper filters, e.g. `{(.+1): .+2}`
    Filter(T, T),
    /// Key is a string, and value is an optional filter, e.g. `{a: 1, b}`
    /// (this is equivalent to `{("a"): 1, ("b"): .b}`)
    Str(Str<T>, Option<T>),
}

impl<F> KeyVal<F> {
    /// Apply a function to the contained filters.
    pub fn map<G>(self, mut f: impl FnMut(F) -> G) -> KeyVal<G> {
        match self {
            Self::Filter(k, v) => KeyVal::Filter(f(k), f(v)),
            Self::Str(k, v) => KeyVal::Str(k.map(&mut f), v.map(f)),
        }
    }
}

/// Common information for folding filters (such as `reduce` and `foreach`)
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Fold<F> {
    /// Generator
    pub xs: F,
    /// Name of assigned variable
    pub x: String,
    /// Initial values
    pub init: F,
    /// Updater
    pub f: F,
}

/// Type of folding filter.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug)]
pub enum FoldType {
    /// return only the final value of fold
    Reduce,
    /// return initial, intermediate, and final values of fold
    For,
    /// return intermediate and final values of fold
    Foreach,
}

/// Function from value to stream of values, such as `.[] | add / length`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Filter<C = String, V = String, Num = String> {
    /// Call to another filter, e.g. `map(.+1)`
    Call(C, Vec<Spanned<Self>>),
    /// Variable, such as $x (without leading '$')
    Var(V),

    /// Integer or floating-point number.
    Num(Num),
    /// String
    Str(Box<Str<Spanned<Self>>>),
    /// Array, empty if `None`
    Array(Option<Box<Spanned<Self>>>),
    /// Object, specifying its key-value pairs
    Object(Vec<KeyVal<Spanned<Self>>>),

    /// Identity, i.e. `.`
    Id,
    /// Path such as `.`, `.a`, `.[][]."b"`
    Path(Box<Spanned<Self>>, Path<Self>),
    /// If-then-else
    Ite(
        Vec<(Spanned<Self>, Spanned<Self>)>,
        Option<Box<Spanned<Self>>>,
    ),
    /// `reduce` and `foreach`, e.g. `reduce .[] as $x (0; .+$x)`
    ///
    /// The first field indicates whether to yield intermediate results
    /// (`false` for `reduce` and `true` for `foreach`).
    Fold(FoldType, Fold<Box<Spanned<Self>>>),
    /// `try` and optional `catch`
    TryCatch(Box<Spanned<Self>>, Option<Box<Spanned<Self>>>),
    /// Error suppression, e.g. `keys?`
    Try(Box<Spanned<Self>>),
    /// Negation
    Neg(Box<Spanned<Self>>),
    /// Recursion (`..`)
    Recurse,
    /// Binary operation, such as `0, 1`, `[] | .[]`, `.[] += 1`, `0 == 0`, ...
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
}

use crate::parse;
use alloc::string::ToString;

impl From<&parse::Term<&str>> for Filter {
    fn from(tm: &parse::Term<&str>) -> Self {
        use crate::path::{Opt, Part};
        let span = |tm: &parse::Term<_>| Box::new((tm.into(), 0..42));
        let from_part = |(part, opt): &(Part<_>, Opt)| {
            let part = match part {
                Part::Index(i) => Part::Index(*span(i)),
                Part::Range(l, h) => {
                    Part::Range(l.as_ref().map(|l| *span(l)), h.as_ref().map(|h| *span(h)))
                }
            };
            (part, *opt)
        };
        let from_str = |part: &StrPart<&str, _>| match part {
            StrPart::Str(s) => string::Part::Str(s.to_string()),
            StrPart::Filter(tm) => string::Part::Fun(*span(tm)),
            StrPart::Char(c) => string::Part::Str(c.to_string()),
        };
        // TODO: this is wrong when v is not given!
        let from_obj = |(k, v): &(_, Option<_>)| {
            KeyVal::Filter(*span(k), v.as_ref().map_or_else(|| *span(k), |v| *span(v)))
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
            _ => todo!("unknown operator"),
        };
        use crate::lex::StrPart;
        use crate::string;
        use parse::Term::*;
        match tm {
            Id => Self::Id,
            Recurse => Self::Recurse,
            Num(n) => Self::Num(n.to_string()),
            Str(fmt, parts) => Self::Str(Box::new(crate::Str {
                fmt: fmt.map(|fmt| span(&Call(fmt, Vec::new()))),
                parts: parts.iter().map(from_str).collect(),
            })),
            Arr(a) => Self::Array(a.as_deref().map(span)),
            Obj(o) => Self::Object(o.iter().map(from_obj).collect()),
            Neg(tm) => Self::Neg(span(&*tm)),
            Pipe(l, v, r) => Self::Binary(
                span(&*l),
                BinaryOp::Pipe(v.map(|v| v[1..].to_string())),
                span(&*r),
            ),
            BinOp(head, tail) => {
                let head = *span(head);
                let tail = tail.iter().map(|(op, tm)| (from_op(op), *span(tm)));
                prec_climb::climb(head, tail).0
            }

            Label(v, ..) | Break(v) => unimplemented!("label-break is not supported yet"),

            Fold(fold, xs, v, args) => {
                let fold_type = match *fold {
                    "reduce" => FoldType::Reduce,
                    "foreach" => FoldType::Foreach,
                    "for" => FoldType::For,
                    _ => panic!(),
                };
                let [init, update] = &args[..] else { panic!() };
                let fold = self::Fold {
                    xs: span(&*xs),
                    x: v[1..].to_string(),
                    init: span(&init),
                    f: span(&update),
                };
                Self::Fold(fold_type, fold)
            }
            TryCatch(try_, catch) => Self::TryCatch(span(try_), catch.as_deref().map(span)),
            IfThenElse(if_thens, else_) => Self::Ite(
                if_thens
                    .iter()
                    .map(|(if_, then_)| (*span(if_), *span(then_)))
                    .collect(),
                else_.as_deref().map(span),
            ),

            Def(defs, tm) => unimplemented!("definitions inside terms are not supported yet"),
            Call(c, args) => Self::Call(c.to_string(), args.iter().map(|a| *span(a)).collect()),
            Var(v) => Self::Var(v[1..].to_string()),

            Key(s) => {
                let s = Self::Str(Box::new(crate::Str::from(s.to_string())));
                let part = (Part::Index((s, 0..42)), Opt::Essential);
                Self::Path(span(&Id), Vec::from([part]))
            }
            Path(tm, path) => Self::Path(span(tm), path.iter().map(from_part).collect()),
        }
    }
}

use crate::prec_climb::{self, Associativity};

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

impl From<Str<Spanned<Self>>> for Filter {
    fn from(s: Str<Spanned<Self>>) -> Self {
        Self::Str(Box::new(s))
    }
}

impl From<Call<Spanned<Self>>> for Filter {
    fn from(c: Call<Spanned<Self>>) -> Self {
        Self::Call(c.name, c.args)
    }
}

impl Filter {
    /// Create a binary expression, such as `1 + 2`.
    pub fn binary(a: Spanned<Self>, op: BinaryOp, b: Spanned<Self>) -> Spanned<Self> {
        let span = a.1.start..b.1.end;
        (Self::Binary(Box::new(a), op, Box::new(b)), span)
    }

    /// Create a path expression, such as `keys[]` or `.a.b`.
    ///
    /// Here, `f` is a filter on whose output the path is executed on,
    /// such as `keys` and `.` in the example above.
    pub fn path(f: Spanned<Self>, path: Path<Self>, span: Span) -> Spanned<Self> {
        if path.is_empty() {
            f
        } else {
            (Self::Path(Box::new(f), path), span)
        }
    }
}
