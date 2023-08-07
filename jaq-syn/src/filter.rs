//! Functions from values to streams of values.
use crate::{MathOp, OrdOp, Path, Span, Spanned};
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
    Str(String, Option<T>),
}

impl<F> KeyVal<F> {
    /// Apply a function to the contained filters.
    pub fn map<G>(self, mut f: impl FnMut(F) -> G) -> KeyVal<G> {
        match self {
            Self::Filter(k, v) => KeyVal::Filter(f(k), f(v)),
            Self::Str(k, v) => KeyVal::Str(k, v.map(f)),
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
    Str(String),
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

impl From<String> for Filter {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}

impl Filter {
    /// Create a binary expression, such as `1 + 2`.
    pub fn binary(a: Spanned<Self>, op: BinaryOp, b: Spanned<Self>) -> Spanned<Self> {
        let span = a.1.start..b.1.end;
        (Filter::Binary(Box::new(a), op, Box::new(b)), span)
    }

    /// Create a path expression, such as `keys[]` or `.a.b`.
    ///
    /// Here, `f` is a filter on whose output the path is executed on,
    /// such as `keys` and `.` in the example above.
    pub fn path(f: Spanned<Self>, path: Path<Self>, span: Span) -> Spanned<Self> {
        if path.is_empty() {
            f
        } else {
            (Filter::Path(Box::new(f), path), span)
        }
    }
}
