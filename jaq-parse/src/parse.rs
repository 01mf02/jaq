use crate::{MathOp, OrdOp, Span, Token};
use chumsky::prelude::*;
use core::fmt;

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

#[derive(Debug)]
pub enum KeyVal {
    Expr(Spanned<Expr>, Spanned<Expr>),
    Str(String, Option<Spanned<Expr>>),
}

// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
#[derive(Debug)]
pub enum Expr {
    Num(String),
    Str(String),
    Call(String, Vec<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Neg(Box<Spanned<Self>>),
    Object(Vec<KeyVal>),
    Array(Option<Box<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Path(Vec<(PathComponent<Spanned<Self>>, Opt)>),
}

// A function node in the AST.
#[derive(Debug)]
pub struct Def {
    pub name: Spanned<String>,
    pub args: Vec<String>,
    pub body: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub enum PathComponent<I> {
    Index(I),
    /// if both are `None`, return iterator over whole array/object
    Range(Option<I>, Option<I>),
}

#[derive(Copy, Clone, Debug)]
pub enum Opt {
    Optional,
    Essential,
}

fn bin<P, O>(prev: P, op: O) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
    O: Parser<Token, BinaryOp, Error = Simple<Token>> + Clone,
{
    let args = prev.clone().then(op.then(prev).repeated());
    args.foldl(|a, (op, b)| {
        let span = a.1.start..b.1.end;
        (Expr::Binary(Box::new(a), op, Box::new(b)), span)
    })
}

fn binr<P, O>(prev: P, op: O) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
    O: Parser<Token, BinaryOp, Error = Simple<Token>> + Clone,
{
    let args = prev.clone().then(op).repeated().then(prev);
    args.foldr(|(a, op), b| {
        let span = a.1.start..b.1.end;
        (Expr::Binary(Box::new(a), op, Box::new(b)), span)
    })
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

fn parse_expr2<'a>(
    expr_with_comma: impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone + 'a,
    expr_sans_comma: impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone + 'a,
    with_comma: bool,
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone + 'a {
    let expr = expr_with_comma;
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

    let if_ = just(Token::If).ignore_then(expr.clone().map(Box::new));
    let then = just(Token::Then).ignore_then(expr.clone().map(Box::new));
    let else_ = just(Token::Else).ignore_then(expr.clone().map(Box::new));
    let ite = if_.then(then).then(else_).then_ignore(just(Token::End));
    let ite = ite.map_with_span(|((if_, then), else_), span| (Expr::If(if_, then, else_), span));

    let call = ident.then(args(expr.clone()));
    let call = call.map_with_span(|(f, args), span| (Expr::Call(f, args), span));

    // Atoms can also just be normal expressions, but surrounded with parentheses
    let parens = expr
        .clone()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let array = expr
        .clone()
        .or_not()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map_with_span(|arr, span| (Expr::Array(arr.map(Box::new)), span));

    let is_val = just(Token::Ctrl(':')).ignore_then(expr_sans_comma);
    let key_str = key
        .then(is_val.clone().or_not())
        .map(|(key, val)| KeyVal::Str(key, val));
    let key_expr = parens
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
            .ignore_then(expr)
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
            Token::DotId(ident) => Ok((Expr::Str(ident), span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        });
        let dot = just(Token::Dot).then(opt.clone());
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

    // 'Atoms' are expressions that contain no ambiguity
    let atom = val
        .map_with_span(|expr, span| (expr, span))
        .or(call)
        .or(object)
        .or(ite)
        .or(parens)
        .or(array)
        .or(path)
        .boxed();

    let neg = just(Token::Op("-".to_string()))
        .map_with_span(|_, span| span)
        .repeated()
        .then(atom)
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
    let add_sub = add_sub.boxed();

    let ord = |op: OrdOp| just(Token::Op(op.to_string())).to(BinaryOp::Ord(op));

    let lt_gt = choice((
        ord(OrdOp::Lt),
        ord(OrdOp::Gt),
        ord(OrdOp::Le),
        ord(OrdOp::Ge),
    ));
    let lt_gt = bin(add_sub, lt_gt);
    // Comparison ops (equal, not-equal) have equal precedence
    let eq_ne = bin(lt_gt, ord(OrdOp::Eq).or(ord(OrdOp::Ne)));
    let eq_ne = eq_ne.boxed();

    let and = bin(eq_ne, just(Token::And).to(BinaryOp::And));
    let or = bin(and, just(Token::Or).to(BinaryOp::Or));

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
    let assign = binr(or, assign);

    let comma = if with_comma {
        bin(assign, just(Token::Ctrl(',')).to(BinaryOp::Comma)).boxed()
    } else {
        assign.boxed()
    };

    bin(comma, just(Token::Op("|".to_string())).to(BinaryOp::Pipe))
}

fn parse_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let mut with_comma = Recursive::declare();
    let mut sans_comma = Recursive::declare();
    with_comma.define(parse_expr2(with_comma.clone(), sans_comma.clone(), true));
    sans_comma.define(parse_expr2(with_comma.clone(), sans_comma.clone(), false));
    with_comma
}

type Main = (Vec<Def>, Spanned<Expr>);

fn parse_def() -> impl Parser<Token, Def, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    let name = ident.map_with_span(|name, span| (name, span));

    just(Token::Def)
        .ignore_then(name.labelled("filter name"))
        .then(args(ident).labelled("filter args"))
        .then_ignore(just(Token::Ctrl(':')))
        .then(parse_expr())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|((name, args), body)| Def { name, args, body })
        .labelled("definition")
}

pub fn parse_defs() -> impl Parser<Token, Vec<Def>, Error = Simple<Token>> + Clone {
    parse_def().repeated().collect()
}

pub fn parse_main() -> impl Parser<Token, Main, Error = Simple<Token>> + Clone {
    parse_defs().then(parse_expr())
}
