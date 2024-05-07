use crate::token::{Delim, Token, Tree};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use parcours::{all, any, consumed, lazy, select, str, Combinator, Parser};

/// Decimal with optional exponent.
fn num<'a>() -> impl Parser<&'a str, O = &'a str> {
    let digits = str::take_while1(|c, _| c.is_numeric());
    let comma = str::matches(".").then(digits.opt());
    let exp = all((
        str::next().filter(|c| "eE".contains(*c)),
        str::next().filter(|c| "+-".contains(*c)).opt(),
        digits,
    ));
    consumed(all((digits, comma.opt(), exp.opt())))
}

/// Hexadecimal number with `len` digits.
fn hex<'a>(len: usize) -> impl Parser<&'a str, O = &'a str> + Clone {
    let mut n = 0;
    str::take_while(move |c, _| {
        n += 1;
        n <= len && c.is_ascii_hexdigit()
    })
    .filter(move |digits| digits.len() == len)
}

/// JSON string character.
fn char_<'a>() -> impl Parser<&'a str, O = char> + Clone {
    let unicode = str::matches("u").ignore_then(hex(4).map(|digits| {
        let num = u32::from_str_radix(&digits, 16).unwrap();
        char::from_u32(num).unwrap_or_else(|| {
            //emit(Simple::custom(span, "invalid unicode character"));
            '\u{FFFD}' // unicode replacement character
        })
    }));

    let bla = str::next().filter_map(select!(
        '\\' => '\\',
        '/' => '/',
        '"' => '"',
        'b' => '\x08',
        'f' => '\x0C',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
    ));
    let escape = str::matches("\\").ignore_then(bla.or(unicode));

    str::next().filter(|c| *c != '\\' && *c != '"').or(escape)
}

fn ident<'a>() -> impl Parser<&'a str, O = &'a str> {
    consumed(all((
        str::matches("@").opt(),
        str::next().filter(|c| c.is_ascii_alphabetic() || *c == '_'),
        str::take_while(|c, _s| c.is_ascii_alphanumeric() || *c == '_'),
    )))
}

fn token<'a>() -> impl Parser<&'a str, O = Token> {
    let op = str::take_while1(|c, _| "|=!<>+-*/%".contains(*c));

    let var = str::matches("$").ignore_then(ident());

    let ident = ident().map(|ident| match ident {
        "def" => Token::Def,
        "if" => Token::If,
        "then" => Token::Then,
        "elif" => Token::Elif,
        "else" => Token::Else,
        "end" => Token::End,
        "or" => Token::Or,
        "and" => Token::And,
        "as" => Token::As,
        "reduce" => Token::Reduce,
        "for" => Token::For,
        "foreach" => Token::Foreach,
        "try" => Token::Try,
        "catch" => Token::Catch,
        _ => Token::Ident(ident.to_string()),
    });

    any((
        ident,
        str::matches("..").map(|_| Token::DotDot),
        str::next().filter_map(select!(
            '.' => Token::Dot,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '?' => Token::Question,
        )),
        op.map(|op| op.to_string()).map(Token::Op),
        var.map(|s| s.to_string()).map(Token::Var),
        num().map(|n| n.to_string()).map(Token::Num),
    ))
}

/// Whitespace and comments.
fn space<'a>() -> impl Parser<&'a str, O = ()> + Clone {
    let space = str::take_while(|c, _| c.is_ascii_whitespace());
    let comment = str::matches("#").then(str::take_while(|c, _| *c != '\n'));
    let comments = space.then(comment).map(|_| ()).repeated::<()>();
    comments.then(space).map(|_| ())
}

fn tree<'a>() -> impl Parser<&'a str, O = Tree> {
    // TODO: span!
    let trees = lazy!(tree).map(|t| (t, 0..42)).repeated();
    let close = |s| space().ignore_then(str::matches(s));
    let paren = trees.delimited_by(str::matches("("), close(")"));
    let brack = trees.delimited_by(str::matches("["), close("]"));
    let brace = trees.delimited_by(str::matches("{"), close("}"));

    let chars = char_().repeated::<String>().map(|s| (s, 0..42));

    let pair = |p| (Tree::Delim(Delim::Paren, p), 0..42);
    let interpol = str::matches("\\").ignore_then(paren.clone().map(pair));

    let string = chars
        .clone()
        .then(interpol.then(chars).repeated())
        .delimited_by(str::matches("\""), str::matches("\""));

    space().ignore_then(any((
        paren.map(|t| Tree::Delim(Delim::Paren, t)),
        brack.map(|t| Tree::Delim(Delim::Brack, t)),
        brace.map(|t| Tree::Delim(Delim::Brace, t)),
        string.map(|(s, interpol)| Tree::String(s, interpol)),
        token().map(Tree::Token),
    )))
}

pub fn lex<'a>() -> impl Parser<&'a str, O = Vec<Tree>> {
    lazy!(tree).repeated().then_ignore(space())
}
