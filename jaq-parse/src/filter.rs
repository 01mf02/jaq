//! Functions from values to streams of values.
use crate::{MathOp, OrdOp, Path, Spanned, Token};
use alloc::{boxed::Box, string::String, string::ToString, vec::Vec};
use chumsky::prelude::*;
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
#[derive(Debug)]
pub enum KeyVal {
    /// Both key and value are proper filters, e.g. `{(.+1): .+2}`
    Filter(Spanned<Filter>, Spanned<Filter>),
    /// Key is a string, and value is an optional filter, e.g. `{a: 1, b}`
    /// (this is equivalent to `{("a"): 1, ("b"): .b}`)
    Str(String, Option<Spanned<Filter>>),
}

/// Function from value to stream of values, such as `.[] | add / length`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum Filter {
    /// Integer or floating-point number.
    Num(String),
    /// String
    Str(String),
    /// Variable, such as $x (without leading '$')
    Var(String),
    /// Array, empty if `None`
    Array(Option<Box<Spanned<Self>>>),
    /// Object, specifying its key-value pairs
    Object(Vec<KeyVal>),
    /// Path such as `.`, `.a`, `.[][]."b"`
    Path(Path<Self>),
    /// If-then-else
    If(Vec<(Spanned<Self>, Spanned<Self>)>, Box<Spanned<Self>>),
    /// Reduction, e.g. `reduce .[] as $x (0; .+$x)`
    Reduce(
        Box<Spanned<Self>>,
        String,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    /// Call to another filter, e.g. `map(.+1)`
    Call(String, Vec<Spanned<Self>>),
    /// Negation
    Neg(Box<Spanned<Self>>),
    /// Binary operation, such as `0, 1`, `[] | .[]`, `.[] += 1`, `0 == 0`, ...
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
}

impl From<String> for Filter {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}

impl Filter {
    fn binary_with_span(a: Spanned<Self>, op: BinaryOp, b: Spanned<Self>) -> Spanned<Self> {
        let span = a.1.start..b.1.end;
        (Filter::Binary(Box::new(a), op, Box::new(b)), span)
    }
}

fn bin<P, O>(prev: P, op: O) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>> + Clone,
    O: Parser<Token, BinaryOp, Error = P::Error> + Clone,
{
    let args = prev.clone().then(op.then(prev).repeated());
    args.foldl(|a, (op, b)| Filter::binary_with_span(a, op, b))
}

fn binr<P, O>(prev: P, op: O) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>> + Clone,
    O: Parser<Token, BinaryOp, Error = P::Error> + Clone,
{
    let args = prev.clone().then(op).repeated().then(prev);
    args.foldr(|(a, op), b| Filter::binary_with_span(a, op, b))
}

pub(crate) fn args<T, P>(arg: P) -> impl Parser<Token, Vec<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T> + Clone,
{
    arg.separated_by(just(Token::Ctrl(';')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .or_not()
        .map(Option::unwrap_or_default)
}

fn variable() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    filter_map(|span, tok| match tok {
        Token::Var(v) => Ok(v),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("variable")
}

// 'Atoms' are filters that contain no ambiguity
fn atom<P>(filter: P, no_comma: P) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    let val = filter_map(|span, tok| match tok {
        Token::Num(n) => Ok(Filter::Num(n)),
        Token::Str(s) => Ok(Filter::Str(s)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("value");

    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("identifier");

    let key = filter_map(|span, tok| match tok {
        Token::Ident(s) | Token::Str(s) => Ok(s),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("object key");

    // Atoms can also just be normal filters, but surrounded with parentheses
    let parenthesised = filter
        .clone()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let var = variable().map_with_span(|v, span| (Filter::Var(v), span));

    let array = filter
        .clone()
        .or_not()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map_with_span(|arr, span| (Filter::Array(arr.map(Box::new)), span));

    let is_val = just(Token::Ctrl(':')).ignore_then(no_comma);
    let key_str = key
        .then(is_val.clone().or_not())
        .map(|(key, val)| KeyVal::Str(key, val));
    let key_filter = parenthesised
        .clone()
        .then(is_val)
        .map(|(key, val)| KeyVal::Filter(key, val));
    let object = key_str
        .or(key_filter)
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
        .collect();

    let object = object.map_with_span(|obj, span| (Filter::Object(obj), span));

    let path = crate::path::path(filter.clone());
    let path = path.map_with_span(|path, span| (Filter::Path(path), span));

    let if_ = just(Token::If).ignore_then(filter.clone());
    let then = just(Token::Then).ignore_then(filter.clone());
    let elif = just(Token::Elif).ignore_then(filter.clone());
    let else_ = just(Token::Else).ignore_then(filter.clone().map(Box::new));
    let ite = if_
        .then(then.clone())
        .chain::<(Spanned<Filter>, Spanned<Filter>), _, _>(elif.then(then).repeated())
        .then(else_)
        .then_ignore(just(Token::End));
    let ite = ite.map_with_span(|(if_thens, else_), span| (Filter::If(if_thens, else_), span));

    let args2 = filter
        .clone()
        .map(Box::new)
        .then_ignore(just(Token::Ctrl(';')))
        .then(filter.clone().map(Box::new))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));
    let reduce = just(Token::Reduce)
        .ignore_then(filter.clone().map(Box::new))
        .then_ignore(just(Token::As))
        .then(variable())
        .then(args2)
        .map_with_span(|((xs, v), (init, f)), span| (Filter::Reduce(xs, v, init, f), span));

    let call = ident.then(args(filter));
    let call = call.map_with_span(|(f, args), span| (Filter::Call(f, args), span));

    let delim = |open, close| (Token::Ctrl(open), Token::Ctrl(close));
    let strategy = |open, close, others| {
        nested_delimiters(Token::Ctrl(open), Token::Ctrl(close), others, |span| {
            (Filter::Path(Vec::new()), span)
        })
    };

    val.map_with_span(|filter, span| (filter, span))
        .or(parenthesised)
        .or(array)
        .or(object)
        .or(path)
        .or(ite)
        .or(call)
        .or(reduce)
        .or(var)
        .recover_with(strategy('(', ')', [delim('[', ']'), delim('{', '}')]))
        .recover_with(strategy('[', ']', [delim('{', '}'), delim('(', ')')]))
        .recover_with(strategy('{', '}', [delim('(', ')'), delim('[', ']')]))
}

fn math<P>(prev: P) -> impl Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    let neg = just(Token::Op("-".to_string()))
        .map_with_span(|_, span| span)
        .repeated()
        .then(prev)
        .foldr(|a, b| {
            let span = a.start..b.1.end;
            (Filter::Neg(Box::new(b)), span)
        });

    let math = |op: MathOp| just(Token::Op(op.to_string())).to(BinaryOp::Math(op));

    let rem = bin(neg, math(MathOp::Rem));
    // Product ops (multiply and divide) have equal precedence
    let mul_div = bin(rem, math(MathOp::Mul).or(math(MathOp::Div)));
    // Sum ops (add and subtract) have equal precedence
    bin(mul_div, math(MathOp::Add).or(math(MathOp::Sub)))
}

fn ord<P>(prev: P) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>> + Clone,
{
    let ord = |op: OrdOp| just(Token::Op(op.to_string())).to(BinaryOp::Ord(op));

    let lt_gt = choice((
        ord(OrdOp::Lt),
        ord(OrdOp::Gt),
        ord(OrdOp::Le),
        ord(OrdOp::Ge),
    ));
    let lt_gt = bin(prev, lt_gt);
    // Comparison ops (equal, not-equal) have equal precedence
    bin(lt_gt, ord(OrdOp::Eq).or(ord(OrdOp::Ne)))
}

fn assign() -> impl Parser<Token, BinaryOp, Error = Simple<Token>> + Clone {
    let assign = |op: AssignOp| just(Token::Op(op.to_string())).to(BinaryOp::Assign(op));
    let update_with = |op: MathOp| assign(AssignOp::UpdateWith(op));

    choice((
        assign(AssignOp::Assign),
        assign(AssignOp::Update),
        update_with(MathOp::Add),
        update_with(MathOp::Sub),
        update_with(MathOp::Mul),
        update_with(MathOp::Div),
        update_with(MathOp::Rem),
    ))
}

pub(crate) fn filter() -> impl Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone {
    // filters that may or may not contain commas on the toplevel,
    // i.e. not inside parentheses
    let mut with_comma = Recursive::declare();
    let mut sans_comma = Recursive::declare();

    let atom = atom(with_comma.clone(), sans_comma.clone()).boxed();
    let math = math(atom).boxed();
    let ord = ord(math).boxed();
    let and = bin(ord, just(Token::And).to(BinaryOp::And));
    let or = bin(and, just(Token::Or).to(BinaryOp::Or));
    let alt = bin(or, just(Token::Op("//".to_string())).to(BinaryOp::Alt));
    let assign = binr(alt, assign()).boxed();

    let comma = just(Token::Ctrl(',')).to(BinaryOp::Comma);

    let as_var = just(Token::As).ignore_then(variable()).or_not();
    let pipe = as_var
        .then_ignore(just(Token::Op("|".to_string())))
        .map(BinaryOp::Pipe);

    sans_comma.define(binr(assign.clone(), pipe.clone()));
    with_comma.define(binr(bin(assign, comma), pipe));

    with_comma
}
