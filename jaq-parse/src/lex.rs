use crate::token::{Delim, Token, Tree};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use parcours::{any, lazy, select, str, Combinator, Parser};

fn strip_digits(i: &str) -> Option<&str> {
    i.strip_prefix(|c: char| c.is_numeric())
        .map(|i| i.trim_start_matches(|c: char| c.is_numeric()))
}

/// Decimal with optional exponent.
fn trim_num(i: &str) -> &str {
    let i = i.trim_start_matches(|c: char| c.is_numeric());
    let i = i.strip_prefix('.').map_or(i, |i| {
        strip_digits(i).unwrap_or_else(|| {
            // TODO: register error
            todo!();
            i
        })
    });
    let i = i.strip_prefix(['e', 'E']).map_or(i, |i| {
        let i = i.strip_prefix(['+', '-']).unwrap_or(i);
        strip_digits(i).unwrap_or_else(|| {
            // TODO: register error
            todo!();
            i
        })
    });
    i
}

fn trim_ident(i: &str) -> &str {
    i.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '_')
}

fn strip_ident(i: &str) -> Option<&str> {
    i.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_')
        .map(trim_ident)
}

fn token(i: &str) -> Option<(Token, &str)> {
    let is_op = |c| "|=!<>+-*/%".contains(c);
    let prefix = |rest: &str| &i[..i.len() - rest.len()];
    let single = |tk: Token| (tk, &i[1..]);

    let mut chars = i.chars();
    Some(match chars.next()? {
        'a'..='z' | 'A'..='Z' | '@' | '_' => {
            let rest = trim_ident(chars.as_str());
            let tk = match prefix(rest) {
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
                ident => Token::Ident(ident.to_string()),
            };
            (tk, rest)
        }
        '$' => {
            // TODO: handle error
            let rest = strip_ident(chars.as_str()).unwrap();
            (Token::Var(i[1..i.len() - rest.len()].to_string()), rest)
        }
        '0'..='9' => {
            let rest = trim_num(chars.as_str());
            (Token::Num(prefix(rest).to_string()), rest)
        }
        '.' if chars.next() == Some('.') => (Token::DotDot, &i[2..]),
        '.' => single(Token::Dot),
        ':' => single(Token::Colon),
        ';' => single(Token::Semicolon),
        ',' => single(Token::Comma),
        '?' => single(Token::Question),
        c if is_op(c) => {
            let rest = chars.as_str().trim_start_matches(is_op);
            (Token::Op(prefix(rest).to_string()), rest)
        }
        _ => return None,
    })
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
        parcours::from_fn(|i, _| token(i)).map(Tree::Token),
    )))
}

pub fn lex<'a>() -> impl Parser<&'a str, O = Vec<Tree>> {
    lazy!(tree).repeated().then_ignore(space())
}
