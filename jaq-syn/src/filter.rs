//! Functions from values to streams of values.
use crate::{Call, LogicOp, MathOp, OrdOp, Path, Span, Spanned, Str};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Assignment operators (`=`, `|=`, `//=`, `+=`, …)
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AssignOp {
    /// Assignment operator (`=`)
    Assign,
    /// Update-assignment operator (`|=`)
    Update,
    /// Alternation update-assignment operator (`//=`)
    AltUpdate,
    /// Arithmetic update-assignment operator (`+=`, `-=`, `*=`, `/=`, `%=`, …)
    UpdateWith(MathOp),
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign => "=".fmt(f),
            Self::Update => "|=".fmt(f),
            Self::AltUpdate => "//=".fmt(f),
            Self::UpdateWith(op) => write!(f, "{op}="),
        }
    }
}

/// Binary operators (`|`, `,`, `//`, `or`, `and`, `+=`, …, `=`, …, `<`, …)
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum BinaryOp {
    /// Binding operator (`EXP as $x | EXP`) if identifier (`x`) is given,
    /// otherwise application operator (`EXP | EXP`)
    Pipe(Option<String>),
    /// Concatenation operator (`,`)
    Comma,
    /// Alternation operator (`,`)
    Alt,
    /// Logicalal operator (`or`, `and`, …)
    Logic(LogicOp),
    /// Arithmetical operator (`+`, `-`, `*`, `/`, `%`, …)
    Math(MathOp),
    /// Assignment operator (`=`, `|=`, `//=`, `+=`, …)
    Assign(AssignOp),
    /// Comparative operator (`<`, `<=`, `>`, `>=`, `==`, `!=`, …)
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
    /// Return only the final value of fold
    Reduce,
    /// Return initial, intermediate, and final values of fold
    For,
    /// Return intermediate and final values of fold
    Foreach,
}

/// Function from value to stream of values, such as `.[] | add / length`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Filter<C = String, V = String, Num = String> {
    /// Call to another filter (`FILTER(…)`), e.g. `map(.+1)`
    Call(C, Vec<Spanned<Self>>),
    /// Variable (`$x`), only storing identifier (`x`)
    Var(V),

    /// Integer or floating-point number.
    Num(Num),
    /// String
    Str(Box<Str<Spanned<Self>>>),
    /// Array (`[…]`), empty if `None` (`[]`)
    Array(Option<Box<Spanned<Self>>>),
    /// Object (`{…}`), specifying its key-value pairs
    Object(Vec<KeyVal<Spanned<Self>>>),

    /// Nullary identity operation (`.`)
    Id,
    /// Path, e.g. `.`, `.a`, and `.[][]."b"`
    Path(Box<Spanned<Self>>, Path<Self>),
    /// If-then-else (`if EXP then EXP else EXP end`) if alternative expression
    /// is given, otherwise if-then (`if EXP then EXP end`)
    Ite(
        Vec<(Spanned<Self>, Spanned<Self>)>,
        Option<Box<Spanned<Self>>>,
    ),
    /// Folding filters, such as `reduce` (`reduce f as $x (g; h)`) and
    /// `foreach` (`foreach f as $x (g; h; i)`), e.g.
    /// `reduce .[] as $x (0; .+$x)`
    ///
    /// The first field indicates whether to yield intermediate results
    /// (`false` for `reduce` and `true` for `foreach`).
    Fold(FoldType, Fold<Box<Spanned<Self>>>),
    /// Try-catch (`try EXP catch EXP`) if handler expression is given,
    /// otherwise try (`try EXP`)
    TryCatch(Box<Spanned<Self>>, Option<Box<Spanned<Self>>>),
    /// Error suppression (`EXP?`), e.g. `keys?`
    Try(Box<Spanned<Self>>),
    /// Unary negation operation (`-EXP`)
    Neg(Box<Spanned<Self>>),
    /// Nullary recursive descent operation (`..`)
    Recurse,
    /// Binary operation, e.g. `0, 1`, `[] | .[]`, `.[] += 1`, `0 == 0`, …
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
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
    /// Create a binary operation expression, e.g. `1 + 2`, …
    pub fn binary(a: Spanned<Self>, op: BinaryOp, b: Spanned<Self>) -> Spanned<Self> {
        let span = a.1.start..b.1.end;
        (Self::Binary(Box::new(a), op, Box::new(b)), span)
    }

    /// Create a path expression, e.g. `keys[]`, `.a.b`, …
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
