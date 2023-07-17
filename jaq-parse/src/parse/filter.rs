use super::{prec_climb, Token};
use crate::filter::{AssignOp, BinaryOp, Filter, Fold, FoldType, KeyVal};
use crate::{MathOp, OrdOp, Spanned};
use alloc::{boxed::Box, string::String, string::ToString, vec::Vec};
use chumsky::prelude::*;

fn variable() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Var(v) => v,
    }
    .labelled("variable")
}

fn if_then_else<P>(filter: P) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    let if_ = just(Token::If).ignore_then(filter.clone());
    let then = just(Token::Then).ignore_then(filter.clone());
    let elif = just(Token::Elif).ignore_then(filter.clone());
    let else_ = just(Token::Else).ignore_then(filter.map(Box::new));
    if_.then(then.clone())
        .chain(elif.then(then).repeated())
        .then(else_)
        .then_ignore(just(Token::End))
        .map_with_span(|(if_thens, else_), span| (Filter::Ite(if_thens, else_), span))
}

fn fold<P>(filter: P) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    let arg = || filter.clone().map(Box::new);
    let args = arg().then_ignore(just(Token::Ctrl(';'))).then(arg());
    let inner = select! {
        Token::Reduce => FoldType::Reduce,
        Token::For => FoldType::For,
        Token::Foreach => FoldType::Foreach,
    };
    inner
        .then(arg())
        .then_ignore(just(Token::As))
        .then(variable())
        .then(args.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .map(|(((inner, xs), x), (init, f))| (inner, Fold { xs, x, init, f }))
        .map_with_span(|(inner, fold), span| (Filter::Fold(inner, fold), span))
}

// 'Atoms' are filters that contain no ambiguity
fn atom<P>(filter: P, no_comma: P) -> impl Parser<Token, Spanned<Filter>, Error = P::Error> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    let val = select! {
        Token::Num(n) => Filter::Num(n),
        Token::Str(s) => Filter::Str(s),
    }
    .labelled("value");

    let ident = select! {
        Token::Ident(ident) => ident,
    }
    .labelled("identifier");

    // Atoms can also just be normal filters, but surrounded with parentheses
    let parenthesised = filter
        .clone()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    let recurse = just(Token::DotDot);

    let array = filter
        .clone()
        .or_not()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

    let is_val = just(Token::Ctrl(':')).ignore_then(no_comma);
    let key_str = super::path::key()
        .then(is_val.clone().or_not())
        .map(|(key, val)| KeyVal::Str(key, val));
    let key_filter = parenthesised
        .clone()
        .then(is_val)
        .map(|(key, val)| KeyVal::Filter(key, val));
    let object = key_str
        .or(key_filter)
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
        .collect();

    let call = ident.then(super::args(filter));

    let delim = |open, close| (Token::Ctrl(open), Token::Ctrl(close));
    let strategy = |open, close, others| {
        nested_delimiters(Token::Ctrl(open), Token::Ctrl(close), others, |span| {
            (Filter::Id, span)
        })
    };

    choice((
        parenthesised,
        val.map_with_span(|filter, span| (filter, span)),
        array.map_with_span(|arr, span| (Filter::Array(arr.map(Box::new)), span)),
        object.map_with_span(|obj, span| (Filter::Object(obj), span)),
        call.map_with_span(|(f, args), span| (Filter::Call(f, args), span)),
        variable().map_with_span(|v, span| (Filter::Var(v), span)),
        recurse.map_with_span(|_, span| (Filter::Recurse, span)),
    ))
    .recover_with(strategy('(', ')', [delim('[', ']'), delim('{', '}')]))
    .recover_with(strategy('[', ']', [delim('{', '}'), delim('(', ')')]))
    .recover_with(strategy('{', '}', [delim('(', ')'), delim('[', ']')]))
}

fn neg<P>(prev: P) -> impl Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
{
    just(Token::Op("-".to_string()))
        .map_with_span(|_, span| span)
        .repeated()
        .then(prev)
        .foldr(|a, b| {
            let span = a.start..b.1.end;
            (Filter::Neg(Box::new(b)), span)
        })
}

impl prec_climb::Op for BinaryOp {
    fn prec(&self) -> usize {
        match self {
            Self::Pipe(_) => 0,
            Self::Comma => 1,
            Self::Assign(_) => 2,
            Self::Alt => 3,
            Self::Or => Self::Alt.prec() + 1,
            Self::And => Self::Or.prec() + 1,
            Self::Ord(OrdOp::Eq | OrdOp::Ne) => Self::And.prec() + 1,
            Self::Ord(OrdOp::Lt | OrdOp::Gt | OrdOp::Le | OrdOp::Ge) => Self::And.prec() + 2,
            Self::Math(MathOp::Add | MathOp::Sub) => Self::And.prec() + 3,
            Self::Math(MathOp::Mul | MathOp::Div) => Self::Math(MathOp::Add).prec() + 1,
            Self::Math(MathOp::Rem) => Self::Math(MathOp::Mul).prec() + 1,
        }
    }

    fn right_assoc(&self) -> bool {
        matches!(self, Self::Pipe(_) | Self::Assign(_))
    }
}

impl prec_climb::Output<BinaryOp> for Spanned<Filter> {
    fn from_op(lhs: Self, op: BinaryOp, rhs: Self) -> Self {
        Filter::binary(lhs, op, rhs)
    }
}

fn binary_op() -> impl Parser<Token, BinaryOp, Error = Simple<Token>> + Clone {
    let as_var = just(Token::As).ignore_then(variable()).or_not();
    let pipe = as_var
        .then_ignore(just(Token::Op("|".to_string())))
        .map(BinaryOp::Pipe);

    let assign = |op: AssignOp| just(Token::Op(op.to_string())).to(BinaryOp::Assign(op));
    let update_with = |op: MathOp| assign(AssignOp::UpdateWith(op));

    let ord = |op: OrdOp| just(Token::Op(op.to_string())).to(BinaryOp::Ord(op));
    let math = |op: MathOp| just(Token::Op(op.to_string())).to(BinaryOp::Math(op));

    choice((
        pipe,
        // normally, here would be `,`,
        // however, in some contexts, we want to exclude `,`
        // (for example, `f` and `g` in `{a: f, b: g}` must not contain `,`)
        // therefore, we add `,` later
        assign(AssignOp::Assign),
        assign(AssignOp::Update),
        update_with(MathOp::Add),
        update_with(MathOp::Sub),
        update_with(MathOp::Mul),
        update_with(MathOp::Div),
        update_with(MathOp::Rem),
        just(Token::Op("//".to_string())).to(BinaryOp::Alt),
        just(Token::Or).to(BinaryOp::Or),
        just(Token::And).to(BinaryOp::And),
        ord(OrdOp::Eq),
        ord(OrdOp::Ne),
        ord(OrdOp::Lt),
        ord(OrdOp::Gt),
        ord(OrdOp::Le),
        ord(OrdOp::Ge),
        math(MathOp::Add),
        math(MathOp::Sub),
        math(MathOp::Mul),
        math(MathOp::Div),
        math(MathOp::Rem),
    ))
}

fn climb<F, O>(f: F, op: O) -> impl Parser<Token, Spanned<Filter>, Error = O::Error> + Clone
where
    F: Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone,
    O: Parser<Token, BinaryOp, Error = Simple<Token>> + Clone,
{
    use prec_climb::Output;
    f.clone()
        .then(op.then(f).repeated().collect::<Vec<_>>())
        .map(|(f, ops)| f.parse(ops))
}

pub fn filter() -> impl Parser<Token, Spanned<Filter>, Error = Simple<Token>> + Clone {
    // filters that may or may not contain commas on the toplevel,
    // i.e. not inside parentheses
    let mut with_comma = Recursive::declare();
    let mut sans_comma = Recursive::declare();

    // e.g. `keys[]`
    let atom = atom(with_comma.clone(), sans_comma.clone());
    let atom_path = || super::path::path(with_comma.clone());
    let atom_with_path = atom.then(atom_path().collect());

    // e.g. `.[].a` or `.a`
    let id = just(Token::Dot).map_with_span(|_, span| (Filter::Id, span));
    let id_path = super::path::index().or_not().chain(atom_path());
    let id_with_path = id.then(id_path.collect());

    let path = atom_with_path.or(id_with_path);

    // named operators, such as `reduce` or `if-then-else`
    let named = choice((
        path.map_with_span(|(f, path), span| Filter::path(f, path, span)),
        fold(with_comma.clone()),
        if_then_else(with_comma.clone()),
    ))
    .boxed();

    let try_ = named
        .then(just(Token::Ctrl('?')).repeated().collect::<Vec<_>>())
        .map_with_span(|(f, try_), span| {
            if try_.is_empty() {
                f
            } else {
                (Filter::Try(Box::new(f)), span)
            }
        });

    let neg = neg(try_).boxed();

    let op = binary_op().boxed();
    let comma = just(Token::Ctrl(',')).to(BinaryOp::Comma);

    sans_comma.define(climb(neg.clone(), op.clone()));
    with_comma.define(climb(neg, op.or(comma)));

    with_comma
}
