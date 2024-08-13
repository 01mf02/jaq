//! Parsing.

use crate::lex::{StrPart, Tok, Token};
use crate::path::{self, Path};
use crate::prec_climb::{self, Associativity};
use crate::{MathOp, OrdOp};
use alloc::{boxed::Box, vec::Vec};

/// Parse error, storing what we expected and what we got instead.
pub type Error<S, T = S> = (Expect<S>, T);
/// Parse error that stores what token it found.
pub type TError<'t, S> = Error<S, Option<&'t Token<S>>>;

/// Type of token that we expected.
///
/// Each variant is annoted with jq programs that trigger it.
#[derive(Debug)]
pub enum Expect<S> {
    /// `if 0` (expected "then"), `reduce .` (expected "as"),
    /// `0 as $x` (expected "|"), `{(.)}` (expected ":")
    Just(S),
    /// `0 as`, `label`, `break`
    Var,
    /// `if 0 then 0`
    ElseOrEnd,
    /// `{a;}`
    CommaOrRBrace,
    /// `f(0:)`
    SemicolonOrRParen,
    /// `` (empty input), `-`, `()`
    Term,
    /// `.[].`
    Key,
    /// `def`, `import "foo" as`
    Ident,
    /// `def f()`
    Arg,
    /// `import`
    Str,
    /// `0;`
    Nothing,
}

impl<'a> Expect<&'a str> {
    /// String representation of an expected token.
    pub fn as_str(&self) -> &'a str {
        match self {
            Self::Just(s) => s,
            Self::Var => "variable",
            Self::ElseOrEnd => "else or end",
            Self::CommaOrRBrace => "comma or right brace",
            Self::SemicolonOrRParen => "semicolon or right parenthesis",
            Self::Term => "term",
            Self::Key => "key",
            Self::Ident => "identifier",
            Self::Arg => "argument",
            Self::Str => "string",
            Self::Nothing => "nothing",
        }
    }
}

/// Output of a fallible parsing operation.
pub type Result<'s, 't, T> = core::result::Result<T, TError<'t, &'s str>>;

/// Parser for jq programs.
pub struct Parser<'s, 't> {
    i: core::slice::Iter<'t, Token<&'s str>>,
    e: Vec<TError<'t, &'s str>>,
    /// names of fold-like filters, e.g. "reduce" and "foreach"
    fold: &'s [&'s str],
}

/// Function from value to stream of values, such as `.[] | add / length`.
#[derive(Debug, Default)]
pub enum Term<S> {
    /// Identity, i.e. `.`
    #[default]
    Id,
    /// Recursion (`..`)
    Recurse,

    /// Integer or floating-point number
    Num(S),
    /// String
    ///
    /// This consists of an optional format filter starting with `@` (such as `@text`),
    /// followed by quoted string parts (such as `"Hello, \(.name)! \u263A"`).
    Str(Option<S>, Vec<StrPart<S, Self>>),
    /// Array, empty if `None`
    Arr(Option<Box<Self>>),
    /// Object, specifying its key-value pairs
    Obj(Vec<(Self, Option<Self>)>),

    /// Negation
    Neg(Box<Self>),
    /// Application, i.e. `l | r` if no string is given, else `l as $x | r`
    Pipe(Box<Self>, Option<S>, Box<Self>),

    /// Sequence of binary operations, e.g. `1 + 2 - 3 * 4`
    BinOp(Box<Self>, BinaryOp, Box<Self>),

    /// Control flow variable declaration, e.g. `label $x | ...`
    Label(S, Box<Self>),
    /// Break out from control flow to location variable, e.g. `break $x`
    Break(S),

    /// `reduce` and `foreach`, e.g. `reduce .[] as $x (0; .+$x)`
    Fold(S, Box<Self>, S, Vec<Self>),
    /// `try` and optional `catch`
    TryCatch(Box<Self>, Option<Box<Self>>),
    /// If-then-else
    IfThenElse(Vec<(Self, Self)>, Option<Box<Self>>),

    /// Local definition
    Def(Vec<Def<S, Self>>, Box<Self>),
    /// Call to another filter, e.g. `map(.+1)`
    Call(S, Vec<Self>),
    /// Variable, such as `$x` (including leading '$')
    Var(S),

    /// Path such as `.a`, `.[][]."b"`, `f[0]`
    Path(Box<Self>, Path<Self>),
}

/// Binary operators, such as `|`, `,`, `//`, ...
#[derive(Debug)]
pub enum BinaryOp {
    /// Concatenation, i.e. `l, r`
    Comma,
    /// Alternation, i.e. `l // r`
    Alt,
    /// Logical disjunction, i.e. `l or r`
    Or,
    /// Logical conjunction, i.e. `l and r`
    And,
    /// Mathematical operation, e.g. `l + r`, `l - r`, ...
    Math(MathOp),
    /// Ordering operation, e.g. `l == r`, `l <= r`, ...
    Ord(OrdOp),
    /// Assignment, i.e. `l = r`,
    Assign,
    /// Update, i.e. `l |= r`
    Update,
    /// Mathematical assignment, i.e. `l += r`, `l -= r`, ...
    /// (this is identical to `r as $x | l |= . + $x`, ...)
    UpdateMath(MathOp),
    /// `l //= r`
    UpdateAlt,
}

impl<S> Term<S> {
    pub(crate) fn str(s: S) -> Self {
        Self::Str(None, [StrPart::Str(s)].into())
    }

    /// `..`, also known as `recurse/0`, is defined as `., (.[]? | ..)`.
    pub(crate) fn recurse(recurse: S) -> Self {
        use Term::*;
        // `[]?`
        let path = (path::Part::Range(None, None), path::Opt::Optional);
        // `.[]?` (returns array/object elements or nothing instead)
        let path = Term::Path(Id.into(), Vec::from([path]));

        // `..`
        let f = Term::Call(recurse, Vec::new());
        // .[]? | ..
        let pipe = Term::Pipe(path.into(), None, f.into());
        // ., (.[]? | ..)
        Term::BinOp(Id.into(), BinaryOp::Comma, pipe.into())
    }

    /// `{}[]` returns zero values.
    pub(crate) fn empty() -> Self {
        // `[]`
        let path = (path::Part::Range(None, None), path::Opt::Essential);
        // `{}`
        let obj = Term::Obj(Vec::new());
        // `{}[]`
        Term::Path(obj.into(), Vec::from([path]))
    }
}

impl<'s, 't> Parser<'s, 't> {
    /// Initialise a new parser on a sequence of [`Token`]s.
    #[must_use]
    pub fn new(i: &'t [Token<&'s str>]) -> Self {
        Self {
            i: i.iter(),
            e: Vec::new(),
            fold: &["reduce", "foreach", "for"],
        }
    }

    /// Parse tokens with the given function.
    ///
    /// Returns [`Ok`] if the function consumes the whole output without producing any error.
    pub fn parse<T: Default, F>(mut self, f: F) -> core::result::Result<T, Vec<TError<'t, &'s str>>>
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, T>,
    {
        let y = self.finish("", f);
        if self.e.is_empty() {
            Ok(y)
        } else {
            Err(self.e)
        }
    }

    /// Verifies that the remaining input tokens correspond to the given string.
    fn verify_last(&mut self, last: &'static str) -> Result<'s, 't, ()> {
        match (self.i.as_slice(), last) {
            ([], "") => Ok(()),
            ([Token(c, _)], last) if *c == last => Ok(()),
            ([], _) => Err((Expect::Just(last), None)),
            ([next, ..], "") => Err((Expect::Nothing, Some(next))),
            ([next, ..], _) => Err((Expect::Just(last), Some(next))),
        }
    }

    /// Run given parse function with given tokens, then reset tokens to previous tokens.
    fn with_tok<T>(&mut self, tokens: &'t [Token<&'s str>], f: impl FnOnce(&mut Self) -> T) -> T {
        let i = core::mem::replace(&mut self.i, tokens.iter());
        let y = f(self);
        self.i = i;
        y
    }

    /// Parse with given function, then
    /// ensure that remaining input tokens correspond to `last`, and
    /// return default if any error occurred.
    fn finish<T: Default, F>(&mut self, last: &'static str, f: F) -> T
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, T>,
    {
        f(self)
            .and_then(|y| {
                self.verify_last(last)?;
                Ok(y)
            })
            .unwrap_or_else(|e| {
                self.e.push(e);
                T::default()
            })
    }

    fn with<T: Default, F>(&mut self, tokens: &'t [Token<&'s str>], last: &'static str, f: F) -> T
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, T>,
    {
        self.with_tok(tokens, |p| p.finish(last, f))
    }

    /// Parse with the given function, and rewind input if it returns `None`.
    fn maybe<T>(&mut self, f: impl Fn(&mut Self) -> Option<T>) -> Option<T> {
        let i = self.i.clone();
        let y = f(self);
        // rewind to previous state in case of non-match
        if y.is_none() {
            self.i = i;
        }
        y
    }

    /// Parse with the given function, and rewind input if it returns `Ok(None)`.
    fn try_maybe<T, F>(&mut self, f: F) -> Result<'s, 't, Option<T>>
    where
        F: Fn(&mut Self) -> Result<'s, 't, Option<T>>,
    {
        let i = self.i.clone();
        let y = f(self)?;
        // rewind to previous state in case of non-match
        if y.is_none() {
            self.i = i;
        }
        Ok(y)
    }

    /// Parse sequence of shape `f ("," f)* ","? "}"`.
    fn obj_items<T, F>(&mut self, f: F) -> Result<'s, 't, Vec<T>>
    where
        F: Fn(&mut Self) -> Result<'s, 't, T>,
    {
        let mut y = Vec::from([f(self)?]);
        let rbrace = |p: &mut Self| p.i.next().filter(|tk| matches!(tk, Token("}", _)));
        loop {
            match self.i.next() {
                Some(Token("}", _)) => break,
                Some(Token(",", _)) if self.maybe(rbrace).is_some() => break,
                Some(Token(",", _)) => y.push(f(self)?),
                next => return Err((Expect::CommaOrRBrace, next)),
            }
        }
        Ok(y)
    }

    /// Parse sequence of shape `f (";" f)* ")"`.
    fn arg_items<T, F>(&mut self, f: F) -> Result<'s, 't, Vec<T>>
    where
        F: Fn(&mut Self) -> Result<'s, 't, T>,
    {
        let mut y = Vec::from([f(self)?]);
        loop {
            match self.i.next() {
                Some(Token(";", _)) => y.push(f(self)?),
                Some(Token(")", _)) => break,
                next => return Err((Expect::SemicolonOrRParen, next)),
            }
        }
        Ok(y)
    }

    /// Parse `("(" arg (";" arg)* ")")?`.
    fn args<T>(&mut self, f: fn(&mut Self) -> Result<'s, 't, T>) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token(full, Tok::Block(tokens))) if full.starts_with('(') => {
                Some(p.with(tokens, "", |p| p.arg_items(f)))
            }
            _ => None,
        })
        .unwrap_or_default()
    }

    /// Parse a binary operator, including `,` if `with_comma` is true.
    fn op(&mut self, with_comma: bool) -> Option<BinaryOp> {
        self.maybe(|p| match p.i.next() {
            Some(Token(s, _)) => Some(match *s {
                "," if with_comma => BinaryOp::Comma,
                "+" => BinaryOp::Math(MathOp::Add),
                "-" => BinaryOp::Math(MathOp::Sub),
                "*" => BinaryOp::Math(MathOp::Mul),
                "/" => BinaryOp::Math(MathOp::Div),
                "%" => BinaryOp::Math(MathOp::Rem),
                "=" => BinaryOp::Assign,
                "|=" => BinaryOp::Update,
                "+=" => BinaryOp::UpdateMath(MathOp::Add),
                "-=" => BinaryOp::UpdateMath(MathOp::Sub),
                "*=" => BinaryOp::UpdateMath(MathOp::Mul),
                "/=" => BinaryOp::UpdateMath(MathOp::Div),
                "%=" => BinaryOp::UpdateMath(MathOp::Rem),
                "<" => BinaryOp::Ord(OrdOp::Lt),
                ">" => BinaryOp::Ord(OrdOp::Gt),
                "<=" => BinaryOp::Ord(OrdOp::Le),
                ">=" => BinaryOp::Ord(OrdOp::Ge),
                "==" => BinaryOp::Ord(OrdOp::Eq),
                "!=" => BinaryOp::Ord(OrdOp::Ne),
                "//" => BinaryOp::Alt,
                "//=" => BinaryOp::UpdateAlt,
                "or" => BinaryOp::Or,
                "and" => BinaryOp::And,
                _ => return None,
            }),
            None => None,
        })
    }

    fn char0(&mut self, c: char) -> Option<&'s str> {
        self.maybe(|p| match p.i.next() {
            Some(Token(s, _)) if s.chars().eq([c]) => Some(*s),
            _ => None,
        })
    }

    fn dot(&mut self) -> Option<&'s str> {
        self.maybe(|p| match p.i.next() {
            Some(Token(c, _)) if *c != ".." => c.strip_prefix('.'),
            _ => None,
        })
    }

    fn terminated<T, F>(&mut self, f: F) -> Result<'s, 't, T>
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, T>,
    {
        let y = f(self)?;
        self.just(";")?;
        Ok(y)
    }

    fn just(&mut self, c: &'static str) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            Some(Token(s, _)) if *s == c => Ok(*s),
            next => Err((Expect::Just(c), next)),
        }
    }

    fn var(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            Some(Token(x, Tok::Var)) => Ok(*x),
            next => Err((Expect::Var, next)),
        }
    }

    /// Parse a term.
    ///
    /// Only if `with_comma` is true, the parsed term may be of the shape `t, u`.
    /// This matters for the parsing of object values, such as `{k1: v1, k2: v2}`:
    /// if we would permit terms of the shape `t, u` inside objects,
    /// then this would be parsed like `{k1: (v1, k2): v2}`, which is invalid.
    fn term_with_comma(&mut self, with_comma: bool) -> Result<'s, 't, Term<&'s str>> {
        let head = self.atom()?;
        let tail = core::iter::from_fn(|| self.op(with_comma).map(|op| Ok((op, self.atom()?))))
            .collect::<Result<Vec<_>>>()?;
        let tm = prec_climb::climb(head, tail);

        let pipe = self.try_maybe(|p| match p.i.next() {
            Some(Token("|", _)) => Ok(Some(None)),
            Some(Token("as", _)) => {
                let x = p.var()?;
                p.just("|")?;
                Ok(Some(Some(x)))
            }
            _ => Ok(None),
        })?;
        Ok(match pipe {
            None => tm,
            Some(x) => Term::Pipe(Box::new(tm), x, Box::new(self.term_with_comma(with_comma)?)),
        })
    }

    /// Parse an atomic term.
    ///
    /// A term `t` is atomic if and only if `try t catch 0` is syntactically correct.
    /// For example, the term `1 + 2` is not atomic, because `try 1 + 2 catch 0` is invalid.
    /// However, the term `.[]` is atomic, because `try .[] catch 0` is valid.
    fn atom(&mut self) -> Result<'s, 't, Term<&'s str>> {
        let tm = match self.i.next() {
            Some(Token("-", _)) => Term::Neg(Box::new(self.atom()?)),
            Some(Token("def", _)) => {
                let head = self.def_tail()?;
                let tail = self.defs()?;
                let tm = self.term()?;
                Term::Def(core::iter::once(head).chain(tail).collect(), Box::new(tm))
            }
            Some(Token("if", _)) => {
                let if_then = |p: &mut Self| -> Result<_> {
                    let if_ = p.term()?;
                    p.just("then")?;
                    Ok((if_, p.term()?))
                };
                let mut if_thens = Vec::from([if_then(self)?]);
                let else_ = loop {
                    match self.i.next() {
                        Some(Token("elif", _)) => if_thens.push(if_then(self)?),
                        Some(Token("else", _)) => {
                            let else_ = self.term()?;
                            self.just("end")?;
                            break Some(else_);
                        }
                        Some(Token("end", _)) => break None,
                        next => return Err((Expect::ElseOrEnd, next)),
                    }
                };
                Term::IfThenElse(if_thens, else_.map(Box::new))
            }
            Some(Token("try", _)) => {
                let try_ = self.atom()?;
                let catch = self.try_maybe(|p| match p.i.next() {
                    Some(Token("catch", _)) => Ok(Some(p.atom()?)),
                    _ => Ok(None),
                })?;
                Term::TryCatch(Box::new(try_), catch.map(Box::new))
            }
            Some(Token("label", _)) => {
                let x = self.var()?;
                self.just("|")?;
                let tm = self.term()?;
                Term::Label(x, Box::new(tm))
            }
            Some(Token("break", _)) => Term::Break(self.var()?),
            Some(Token(fold, Tok::Word)) if self.fold.contains(fold) => {
                let xs = self.atom()?;
                self.just("as")?;
                let x = self.var()?;
                let args = self.args(Self::term);
                Term::Fold(*fold, Box::new(xs), x, args)
            }
            Some(Token(id, Tok::Var)) => Term::Var(*id),
            Some(Token(id, Tok::Fmt)) => {
                let s = self.maybe(|p| match p.i.next() {
                    Some(Token(_, Tok::Str(parts))) => Some(p.str_parts(parts)),
                    _ => None,
                });
                match s {
                    None => Term::Call(*id, self.args(Self::term)),
                    Some(parts) => Term::Str(Some(*id), parts),
                }
            }
            Some(Token(id, Tok::Word)) => Term::Call(*id, self.args(Self::term)),
            Some(Token("..", _)) => Term::Recurse,
            Some(Token(c, Tok::Sym)) if c.starts_with('.') => {
                let key = if c.len() > 1 {
                    Some(Term::str(&c[1..]))
                } else {
                    // TODO: this returns None on things like "@json .",
                    // whereas it should return an error instead
                    self.maybe(|p| p.key().ok())
                };

                if let Some(key) = key {
                    let head = (path::Part::Index(key), self.opt());
                    let path = core::iter::once(head).chain(self.path()?).collect();
                    Term::Path(Box::new(Term::Id), path)
                } else {
                    Term::Id
                }
            }
            Some(Token(n, Tok::Num)) => Term::Num(*n),
            Some(Token(full, Tok::Block(tokens))) => match &full[..1] {
                "[" if matches!(tokens[..], [Token("]", _)]) => Term::Arr(None),
                "{" if matches!(tokens[..], [Token("}", _)]) => Term::Obj(Vec::new()),
                "[" => Term::Arr(Some(Box::new(self.with(tokens, "]", Self::term)))),
                "{" => self.with(tokens, "", |p| p.obj_items(Self::obj_entry).map(Term::Obj)),
                "(" => self.with(tokens, ")", Self::term),
                _ => panic!(),
            },
            Some(Token(_, Tok::Str(parts))) => Term::Str(None, self.str_parts(parts)),
            next => return Err((Expect::Term, next)),
        };

        let tm = match self.opt() {
            path::Opt::Optional => Term::TryCatch(Box::new(tm), None),
            path::Opt::Essential => tm,
        };

        let path = self.path()?;
        Ok(if path.is_empty() {
            tm
        } else {
            Term::Path(Box::new(tm), path)
        })
    }

    /// Parse a term such as `.[] | .+1`.
    pub fn term(&mut self) -> Result<'s, 't, Term<&'s str>> {
        self.term_with_comma(true)
    }

    /// Parse an object entry.
    ///
    /// An object is written as `{e1, ..., en}`, where `ei` is an object entry.
    /// An example of an object entry is `"key": value` or `(key): value`.
    /// When the key is a term surrounded by parentheses, a value is required,
    /// otherwise the value may be omitted (e.g. `"key"` or `$x`).
    fn obj_entry(&mut self) -> Result<'s, 't, (Term<&'s str>, Option<Term<&'s str>>)> {
        let i = self.i.clone();
        let key = match self.i.next() {
            Some(Token(full, Tok::Block(tokens))) if full.starts_with('(') => {
                let k = self.with(tokens, ")", Self::term);
                self.just(":")?;
                return Ok((k, Some(self.term_with_comma(false)?)));
            }
            Some(Token(id, Tok::Word)) if !id.contains("::") => Term::str(*id),
            _ => {
                self.i = i;
                self.key()?
            }
        };
        let v = self.char0(':').map(|_| self.term_with_comma(false));
        Ok((key, v.transpose()?))
    }

    fn str_parts(
        &mut self,
        parts: &'t [StrPart<&'s str, Token<&'s str>>],
    ) -> Vec<StrPart<&'s str, Term<&'s str>>> {
        let parts = parts.iter().map(|part| match part {
            StrPart::Str(s) => StrPart::Str(*s),
            StrPart::Term(Token(full, Tok::Block(tokens))) if full.starts_with('(') => {
                StrPart::Term(self.with(tokens, ")", Self::term))
            }
            StrPart::Term(_) => unreachable!(),
            StrPart::Char(c) => StrPart::Char(*c),
        });
        parts.collect()
    }

    fn path(&mut self) -> Result<'s, 't, Path<Term<&'s str>>> {
        let mut path: Vec<_> = core::iter::from_fn(|| self.path_part_opt()).collect();
        while let Some(key) = self.dot() {
            let key = if key.is_empty() {
                self.key()?
            } else {
                Term::str(key)
            };
            path.push((path::Part::Index(key), self.opt()));
            path.extend(core::iter::from_fn(|| self.path_part_opt()));
        }
        Ok(path)
    }

    /// Parse `[]`, `[t]`, `[t:]`, `[t:t]`, `[:t]` (all without brackets).
    fn path_part(&mut self) -> Result<'s, 't, path::Part<Term<&'s str>>> {
        use path::Part::{Index, Range};
        let done = |p: &Self| matches!(p.i.as_slice(), [Token("]", _)]);
        Ok(if done(self) {
            Range(None, None)
        } else if self.char0(':').is_some() {
            Range(None, Some(self.term()?))
        } else {
            let tm = self.term()?;
            if self.char0(':').is_some() {
                if done(self) {
                    Range(Some(tm), None)
                } else {
                    Range(Some(tm), Some(self.term()?))
                }
            } else {
                Index(tm)
            }
        })
    }

    fn path_part_opt(&mut self) -> Option<(path::Part<Term<&'s str>>, path::Opt)> {
        let part = self.maybe(|p| match p.i.next() {
            Some(Token(full, Tok::Block(tokens))) if full.starts_with('[') => {
                Some(p.with(tokens, "]", Self::path_part))
            }
            _ => None,
        })?;
        Some((part, self.opt()))
    }

    fn key(&mut self) -> Result<'s, 't, Term<&'s str>> {
        match self.i.next() {
            Some(Token(id, Tok::Var)) => Ok(Term::Var(*id)),
            Some(Token(id, Tok::Fmt)) => match self.i.next() {
                Some(Token(_, Tok::Str(parts))) => Ok(Term::Str(Some(*id), self.str_parts(parts))),
                next => Err((Expect::Str, next)),
            },
            Some(Token(_, Tok::Str(parts))) => Ok(Term::Str(None, self.str_parts(parts))),
            next => Err((Expect::Key, next)),
        }
    }

    fn opt(&mut self) -> path::Opt {
        let mut opt = path::Opt::Essential;
        while self.char0('?').is_some() {
            opt = path::Opt::Optional;
        }
        opt
    }

    /// Parse a sequence of definitions, such as `def x: 1; def y: 2;`.
    pub fn defs(&mut self) -> Result<'s, 't, Defs<&'s str>> {
        let head = |p: &mut Self| p.just("def").ok();
        core::iter::from_fn(|| self.maybe(head).map(|_| self.def_tail())).collect()
    }

    /// Parse `name args ":" term ";"`.
    fn def_tail(&mut self) -> Result<'s, 't, Def<&'s str, Term<&'s str>>> {
        let name = match self.i.next() {
            Some(Token(w, Tok::Word | Tok::Fmt)) if !w.contains("::") => w,
            next => return Err((Expect::Ident, next)),
        };
        let args = self.args(|p| match p.i.next() {
            Some(Token(w, Tok::Word | Tok::Var)) if !w.contains("::") => Ok(*w),
            next => Err((Expect::Arg, next)),
        });
        self.just(":")?;

        let body = self.term()?;
        self.just(";")?;

        Ok(Def { name, args, body })
    }

    fn bare_str(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            next @ Some(Token(_, Tok::Str(parts))) => match parts[..] {
                [StrPart::Str(s)] => Ok(s),
                _ => Err((Expect::Str, next)),
            },
            next => Err((Expect::Str, next)),
        }
    }

    fn include(&mut self) -> Result<'s, 't, (&'s str, Option<&'s str>)> {
        self.bare_str().map(|path| (path, None))
    }

    fn import(&mut self) -> Result<'s, 't, (&'s str, Option<&'s str>)> {
        let path = self.bare_str()?;
        self.just("as")?;
        let name = match self.i.next() {
            Some(Token(v, Tok::Word | Tok::Var)) => *v,
            next => return Err((Expect::Ident, next)),
        };
        Ok((path, Some(name)))
    }

    /// Parse a module with a body returned by the given function.
    pub fn module<B, F>(&mut self, f: F) -> Result<'s, 't, Module<&'s str, B>>
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, B>,
    {
        let meta = self
            .maybe(|p| match p.i.next() {
                Some(Token("module", _)) => Some(p.terminated(Self::term)),
                _ => None,
            })
            .transpose()?;

        let deps = core::iter::from_fn(|| {
            self.maybe(|p| match p.i.next() {
                Some(Token("include", _)) => Some(p.terminated(Self::include)),
                Some(Token("import", _)) => Some(p.terminated(Self::import)),
                _ => None,
            })
        })
        .collect::<Result<_>>()?;

        let body = f(self)?;

        Ok(Module { meta, deps, body })
    }
}

/// jq module, consisting of metadata, imports/includes, and a body.
///
/// Example (where the body is a sequence of definitions):
///
/// ~~~ jq
/// module {};
///
/// import "foo" as foo;
/// include "bar";
///
/// def iter: .[];
/// ~~~
#[derive(Debug, Default)]
pub struct Module<S, B> {
    #[allow(dead_code)]
    pub(crate) meta: Option<Term<S>>,
    pub(crate) deps: Vec<(S, Option<S>)>,
    pub body: B,
}

/// jq definition, consisting of a name, optional arguments, and a body.
///
/// Examples:
///
/// ~~~ jq
/// def pi: 3.1415;
/// def double($x): $x + $x;
/// def map(f): [.[] | f];
/// def recurse(f; cond): recurse(f | select(cond));
/// ~~~
#[derive(Debug)]
pub struct Def<S, F> {
    pub name: S,
    pub args: Vec<S>,
    /// Body of the filter, e.g. `[.[] | f]`.
    pub body: F,
}

impl<S, F> Def<S, F> {
    pub(crate) fn new(name: S, args: Vec<S>, body: F) -> Self {
        Self { name, args, body }
    }
}

pub type Defs<S> = Vec<Def<S, Term<S>>>;

impl prec_climb::Op for BinaryOp {
    fn precedence(&self) -> usize {
        match self {
            Self::Comma => 1,
            Self::Assign | Self::Update | Self::UpdateMath(_) => 2,
            Self::Alt => 3,
            Self::Or => Self::Alt.precedence() + 1,
            Self::And => Self::Or.precedence() + 1,
            Self::Ord(OrdOp::Eq | OrdOp::Ne) => Self::And.precedence() + 1,
            Self::Ord(OrdOp::Lt | OrdOp::Gt | OrdOp::Le | OrdOp::Ge) => Self::And.precedence() + 2,
            Self::Math(MathOp::Add | MathOp::Sub) => Self::And.precedence() + 3,
            Self::Math(MathOp::Mul | MathOp::Div) => Self::Math(MathOp::Add).precedence() + 1,
            Self::Math(MathOp::Rem) => Self::Math(MathOp::Mul).precedence() + 1,
            Self::UpdateAlt => todo!(),
        }
    }

    fn associativity(&self) -> prec_climb::Associativity {
        use prec_climb::Associativity;
        match self {
            Self::Assign | Self::Update | Self::UpdateMath(_) => Associativity::Right,
            Self::UpdateAlt => todo!(),
            _ => Associativity::Left,
        }
    }
}

impl<S> prec_climb::Expr<BinaryOp> for Term<S> {
    fn from_op(lhs: Self, op: BinaryOp, rhs: Self) -> Self {
        Self::BinOp(Box::new(lhs), op, Box::new(rhs))
    }
}
