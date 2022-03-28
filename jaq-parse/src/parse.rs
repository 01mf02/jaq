use crate::{MathOp, Opt, OrdOp, Span, Token};
use chumsky::prelude::*;
use core::fmt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AssignOp {
    Assign,
    Update,
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

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum BinaryOp {
    Pipe,
    Comma,
    Or,
    And,
    Math(MathOp),
    Assign(AssignOp),
    Ord(OrdOp),
}

pub type Spanned<T> = (T, Span);

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum KeyVal {
    Expr(Spanned<Expr>, Spanned<Expr>),
    Str(String, Option<Spanned<Expr>>),
}

// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum Expr {
    Num(String),
    Str(String),
    Array(Option<Box<Spanned<Self>>>),
    Object(Vec<KeyVal>),
    Path(Vec<(PathComponent<Spanned<Self>>, Opt)>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Call(String, Vec<Spanned<Self>>),
    Neg(Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
}

// A function node in the AST.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Def<F> {
    pub name: String,
    pub args: Vec<String>,
    pub body: F,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum PathComponent<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

impl Expr {
    fn binary_with_span(a: Spanned<Self>, op: BinaryOp, b: Spanned<Self>) -> Spanned<Self> {
        let span = a.1.start..b.1.end;
        (Expr::Binary(Box::new(a), op, Box::new(b)), span)
    }
}

fn bin<P, O>(prev: P, op: O) -> impl Parser<Token, Spanned<Expr>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Expr>> + Clone,
    O: Parser<Token, BinaryOp, Error = P::Error> + Clone,
{
    let args = prev.clone().then(op.then(prev).repeated());
    args.foldl(|a, (op, b)| Expr::binary_with_span(a, op, b))
}

fn args<T, P>(arg: P) -> impl Parser<Token, Vec<T>, Error = P::Error> + Clone
where
    P: Parser<Token, T> + Clone,
{
    arg.separated_by(just(Token::Ctrl(';')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .or_not()
        .map(Option::unwrap_or_default)
}

// 'Atoms' are expressions that contain no ambiguity
fn atom<P>(expr: P, sans_comma: P) -> impl Parser<Token, Spanned<Expr>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    let val = filter_map(|span, tok| match tok {
        Token::Num(n) => Ok(Expr::Num(n)),
        Token::Str(s) => Ok(Expr::Str(s)),
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

    // Atoms can also just be normal expressions, but surrounded with parentheses
    let parenthesised = expr
        .clone()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let array = expr
        .clone()
        .or_not()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map_with_span(|arr, span| (Expr::Array(arr.map(Box::new)), span));

    let is_val = just(Token::Ctrl(':')).ignore_then(sans_comma);
    let key_str = key
        .then(is_val.clone().or_not())
        .map(|(key, val)| KeyVal::Str(key, val));
    let key_expr = parenthesised
        .clone()
        .then(is_val)
        .map(|(key, val)| KeyVal::Expr(key, val));
    let object = key_str
        .or(key_expr)
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
        .collect();

    let object = object.map_with_span(|obj, span| (Expr::Object(obj), span));

    let range = {
        let colon = just(Token::Ctrl(':'));
        let e2 = colon.clone().ignore_then(expr.clone().or_not());
        let starts_with_expr = expr.clone().then(e2.or_not()).map(|(e1, e2)| match e2 {
            None => PathComponent::Index(e1),
            Some(e2) => PathComponent::Range(Some(e1), e2),
        });
        let starts_with_colon = colon
            .ignore_then(expr.clone())
            .map(|e2| PathComponent::Range(None, Some(e2)));

        starts_with_expr
            .or(starts_with_colon)
            .or_not()
            .map(|o| o.unwrap_or(PathComponent::Range(None, None)))
    };

    let path = {
        let opt = just(Token::Ctrl('?')).or_not().map(|q| match q {
            Some(_) => Opt::Optional,
            None => Opt::Essential,
        });

        let dot_id = filter_map(|span, tok| match tok {
            Token::Dot(Some(ident)) => Ok((Expr::Str(ident), span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        });
        let dot = just(Token::Dot(None)).then(opt.clone());
        let dot_id = dot_id.map(PathComponent::Index).then(opt.clone());

        let ranges = range
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .then(opt)
            .repeated();

        let head = dot.ignore_then(ranges.clone());
        let tail = dot_id.chain(ranges);

        head.or(tail.clone())
            .chain(tail.repeated().flatten())
            .collect()
    };
    let path = path.map_with_span(|path, span| (Expr::Path(path), span));

    let if_ = just(Token::If).ignore_then(expr.clone().map(Box::new));
    let then = just(Token::Then).ignore_then(expr.clone().map(Box::new));
    let else_ = just(Token::Else).ignore_then(expr.clone().map(Box::new));
    let ite = if_.then(then).then(else_).then_ignore(just(Token::End));
    let ite = ite.map_with_span(|((if_, then), else_), span| (Expr::If(if_, then, else_), span));

    let call = ident.then(args(expr));
    let call = call.map_with_span(|(f, args), span| (Expr::Call(f, args), span));

    let delim = |open, close| (Token::Ctrl(open), Token::Ctrl(close));
    let strategy = |open, close, others| {
        nested_delimiters(Token::Ctrl(open), Token::Ctrl(close), others, |span| {
            (Expr::Path(Vec::new()), span)
        })
    };

    val.map_with_span(|expr, span| (expr, span))
        .or(parenthesised)
        .or(array)
        .or(object)
        .or(path)
        .or(ite)
        .or(call)
        .recover_with(strategy('(', ')', [delim('[', ']'), delim('{', '}')]))
        .recover_with(strategy('[', ']', [delim('{', '}'), delim('(', ')')]))
        .recover_with(strategy('{', '}', [delim('(', ')'), delim('[', ']')]))
}

fn math<P>(prev: P) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    let neg = just(Token::Op("-".to_string()))
        .map_with_span(|_, span| span)
        .repeated()
        .then(prev)
        .foldr(|a, b| {
            let span = a.start..b.1.end;
            (Expr::Neg(Box::new(b)), span)
        });

    let math = |op: MathOp| just(Token::Op(op.to_string())).to(BinaryOp::Math(op));

    let rem = bin(neg, math(MathOp::Rem));
    // Product ops (multiply and divide) have equal precedence
    let mul_div = bin(rem, math(MathOp::Mul).or(math(MathOp::Div)));
    // Sum ops (add and subtract) have equal precedence
    let add_sub = bin(mul_div, math(MathOp::Add).or(math(MathOp::Sub)));
    add_sub
}

fn ord<P>(prev: P) -> impl Parser<Token, Spanned<Expr>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Expr>> + Clone,
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
    let eq_ne = bin(lt_gt, ord(OrdOp::Eq).or(ord(OrdOp::Ne)));
    eq_ne
}

fn assign<P>(prev: P) -> impl Parser<Token, Spanned<Expr>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Expr>> + Clone,
{
    let assign = |op: AssignOp| just(Token::Op(op.to_string())).to(BinaryOp::Assign(op));

    let update_with = |op: MathOp| assign(AssignOp::UpdateWith(op));
    let assign = choice((
        assign(AssignOp::Assign),
        assign(AssignOp::Update),
        update_with(MathOp::Add),
        update_with(MathOp::Sub),
        update_with(MathOp::Mul),
        update_with(MathOp::Div),
        update_with(MathOp::Rem),
    ));

    let args = prev.clone().then(assign).repeated().then(prev);
    args.foldr(|(a, op), b| Expr::binary_with_span(a, op, b))
}

pub fn expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let mut with_comma = Recursive::declare();
    let mut sans_comma = Recursive::declare();

    let atom = atom(with_comma.clone(), sans_comma.clone()).boxed();
    let math = math(atom).boxed();
    let ord = ord(math).boxed();
    let and = bin(ord, just(Token::And).to(BinaryOp::And));
    let or = bin(and, just(Token::Or).to(BinaryOp::Or));
    let assign = assign(or).boxed();

    let comma = just(Token::Ctrl(',')).to(BinaryOp::Comma);
    let pipe = just(Token::Op("|".to_string())).to(BinaryOp::Pipe);

    sans_comma.define(bin(assign.clone(), pipe.clone()));
    with_comma.define(bin(bin(assign, comma), pipe));

    with_comma
}

pub type Defs = Vec<Def<Spanned<Expr>>>;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Main<F> {
    pub defs: Vec<Def<F>>,
    pub body: F,
}

pub fn def() -> impl Parser<Token, Def<Spanned<Expr>>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    just(Token::Def)
        .ignore_then(ident.labelled("filter name"))
        .then(args(ident).labelled("filter args"))
        .then_ignore(just(Token::Ctrl(':')))
        .then(expr())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|((name, args), body)| Def { name, args, body })
        .labelled("definition")
}

pub fn defs() -> impl Parser<Token, Defs, Error = Simple<Token>> + Clone {
    def().repeated().collect()
}

pub fn main() -> impl Parser<Token, Main<Spanned<Expr>>, Error = Simple<Token>> + Clone {
    defs().then(expr()).map(|(defs, body)| Main { defs, body })
}
