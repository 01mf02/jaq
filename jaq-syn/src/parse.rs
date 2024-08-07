//! Parsing.

use crate::lex::{StrPart, Token};
use crate::path;
use alloc::{boxed::Box, vec::Vec};

/// Parse error, storing what we expected and what we got instead.
pub type Error<S, T = S> = (Expect<S>, T);
/// Parse error that stores what token it found.
pub type TError<'t, S> = Error<S, Option<&'t Token<S>>>;

type Path<T> = Vec<(path::Part<T>, path::Opt)>;

/// Type of token that we expected.
///
/// Each variant is annoted with jq programs that trigger it.
#[derive(Debug)]
pub enum Expect<S> {
    /// `if 0` (expected "then"), `reduce .` (expected "as")
    Keyword(S),
    /// `0 as $x` (expected "|"), `{(.)}` (expected ":")
    Char(S),
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
            Self::Keyword(s) | Self::Char(s) => s,
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
    BinOp(Box<Self>, Vec<(S, Self)>),

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

impl<S> Term<S> {
    pub(crate) fn str(s: S) -> Self {
        Self::Str(None, [StrPart::Str(s)].into())
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
            ([Token::Char(c)], last) if *c == last => Ok(()),
            ([], _) => Err((Expect::Char(last), None))?,
            ([next, ..], "") => Err((Expect::Nothing, Some(next)))?,
            ([next, ..], _) => Err((Expect::Char(last), Some(next)))?,
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
        let rbrace = |p: &mut Self| p.i.next().filter(|tk| matches!(tk, Token::Char("}")));
        loop {
            match self.i.next() {
                Some(Token::Char("}")) => break,
                Some(Token::Char(",")) if self.maybe(rbrace).is_some() => break,
                Some(Token::Char(",")) => y.push(f(self)?),
                next => Err((Expect::CommaOrRBrace, next))?,
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
                Some(Token::Char(";")) => y.push(f(self)?),
                Some(Token::Char(")")) => break,
                next => Err((Expect::SemicolonOrRParen, next))?,
            }
        }
        Ok(y)
    }

    /// Parse `("(" arg (";" arg)* ")")?`.
    fn args<T>(&mut self, f: fn(&mut Self) -> Result<'s, 't, T>) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Block(full, tokens)) if full.starts_with('(') => {
                Some(p.with(tokens, "", |p| p.arg_items(f)))
            }
            _ => None,
        })
        .unwrap_or_default()
    }

    /// Parse a binary operator, including `,` if `with_comma` is true.
    fn op(&mut self, with_comma: bool) -> Option<&'s str> {
        self.maybe(|p| match p.i.next() {
            // handle pipe directly in `term()`
            Some(Token::Op("|")) => None,
            Some(Token::Op(o) | Token::Word(o @ ("and" | "or"))) => Some(*o),
            Some(Token::Char(o @ ",")) if with_comma => Some(*o),
            _ => None,
        })
    }

    fn char0(&mut self, c: char) -> Option<&'s str> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Char(s)) if s.chars().eq([c]) => Some(*s),
            _ => None,
        })
    }

    fn dot(&mut self) -> Option<&'s str> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Char(c)) if *c != ".." => c.strip_prefix('.'),
            _ => None,
        })
    }

    fn terminated<T, F>(&mut self, f: F) -> Result<'s, 't, T>
    where
        F: FnOnce(&mut Self) -> Result<'s, 't, T>,
    {
        let y = f(self)?;
        self.char1(";")?;
        Ok(y)
    }

    fn char1(&mut self, c: &'static str) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            Some(Token::Char(s)) if *s == c => Ok(*s),
            next => Err((Expect::Char(c), next))?,
        }
    }

    fn keyword(&mut self, kw: &'static str) -> Result<'s, 't, ()> {
        match self.i.next() {
            Some(Token::Word(w)) if *w == kw => Ok(()),
            next => Err((Expect::Keyword(kw), next))?,
        }
    }

    fn var(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            Some(Token::Word(x)) if x.starts_with('$') => Ok(*x),
            next => Err((Expect::Var, next))?,
        }
    }

    fn pipe(&mut self) -> Result<'s, 't, ()> {
        match self.i.next() {
            Some(Token::Op("|")) => Ok(()),
            next => Err((Expect::Char("|"), next))?,
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

        let tm = if tail.is_empty() {
            head
        } else {
            Term::BinOp(Box::new(head), tail)
        };

        let pipe = self.try_maybe(|p| match p.i.next() {
            Some(Token::Op("|")) => Ok(Some(None)),
            Some(Token::Word("as")) => {
                let x = p.var()?;
                p.pipe()?;
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
            Some(Token::Op("-")) => Term::Neg(Box::new(self.atom()?)),
            Some(Token::Word("def")) => {
                let head = self.def_tail()?;
                let tail = self.defs()?;
                let tm = self.term()?;
                Term::Def(core::iter::once(head).chain(tail).collect(), Box::new(tm))
            }
            Some(Token::Word("if")) => {
                let if_then = |p: &mut Self| -> Result<_> {
                    let if_ = p.term()?;
                    p.keyword("then")?;
                    Ok((if_, p.term()?))
                };
                let mut if_thens = Vec::from([if_then(self)?]);
                let else_ = loop {
                    match self.i.next() {
                        Some(Token::Word("elif")) => if_thens.push(if_then(self)?),
                        Some(Token::Word("else")) => {
                            let else_ = self.term()?;
                            self.keyword("end")?;
                            break Some(else_);
                        }
                        Some(Token::Word("end")) => break None,
                        next => Err((Expect::ElseOrEnd, next))?,
                    }
                };
                Term::IfThenElse(if_thens, else_.map(Box::new))
            }
            Some(Token::Word("try")) => {
                let try_ = self.atom()?;
                let catch = self.try_maybe(|p| match p.i.next() {
                    Some(Token::Word("catch")) => Ok(Some(p.atom()?)),
                    _ => Ok(None),
                })?;
                Term::TryCatch(Box::new(try_), catch.map(Box::new))
            }
            Some(Token::Word("label")) => {
                let x = self.var()?;
                self.pipe()?;
                let tm = self.term()?;
                Term::Label(x, Box::new(tm))
            }
            Some(Token::Word("break")) => Term::Break(self.var()?),
            Some(Token::Word(fold)) if self.fold.contains(fold) => {
                let xs = self.atom()?;
                self.keyword("as")?;
                let x = self.var()?;
                let args = self.args(Self::term);
                Term::Fold(*fold, Box::new(xs), x, args)
            }
            Some(Token::Word(id)) if id.starts_with('$') => Term::Var(*id),
            Some(Token::Word(id)) if id.starts_with('@') => {
                let s = self.maybe(|p| match p.i.next() {
                    Some(Token::Str(_, parts)) => Some(p.str_parts(parts)),
                    _ => None,
                });
                match s {
                    None => Term::Call(*id, Vec::new()),
                    Some(parts) => Term::Str(Some(*id), parts),
                }
            }
            Some(Token::Word(id)) => Term::Call(*id, self.args(Self::term)),
            Some(Token::Char("..")) => Term::Recurse,
            Some(Token::Char(c)) if c.starts_with('.') => {
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
            Some(Token::Num(n)) => Term::Num(*n),
            Some(Token::Block(full, tokens)) => match &full[..1] {
                "[" if matches!(tokens[..], [Token::Char("]")]) => Term::Arr(None),
                "{" if matches!(tokens[..], [Token::Char("}")]) => Term::Obj(Vec::new()),
                "[" => Term::Arr(Some(Box::new(self.with(tokens, "]", Self::term)))),
                "{" => self.with(tokens, "", |p| p.obj_items(Self::obj_entry).map(Term::Obj)),
                "(" => self.with(tokens, ")", Self::term),
                _ => panic!(),
            },
            Some(Token::Str(_, parts)) => Term::Str(None, self.str_parts(parts)),
            next => Err((Expect::Term, next))?,
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
            Some(Token::Block(full, tokens)) if full.starts_with('(') => {
                let k = self.with(tokens, ")", Self::term);
                self.char1(":")?;
                return Ok((k, Some(self.term_with_comma(false)?)));
            }
            Some(Token::Word(id)) if !id.starts_with(['$', '@']) && !id.contains("::") => {
                Term::str(*id)
            }
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
            StrPart::Filter(Token::Block(full, tokens)) if full.starts_with('(') => {
                StrPart::Filter(self.with(tokens, ")", Self::term))
            }
            StrPart::Filter(_) => unreachable!(),
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
        let done = |p: &Self| matches!(p.i.as_slice(), [Token::Char("]")]);
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
            Some(Token::Block(full, tokens)) if full.starts_with('[') => {
                Some(p.with(tokens, "]", Self::path_part))
            }
            _ => None,
        })?;
        Some((part, self.opt()))
    }

    fn key(&mut self) -> Result<'s, 't, Term<&'s str>> {
        Ok(match self.i.next() {
            Some(Token::Word(id)) if id.starts_with('$') => Term::Var(*id),
            Some(Token::Word(id)) if id.starts_with('@') => match self.i.next() {
                Some(Token::Str(_, parts)) => Term::Str(Some(*id), self.str_parts(parts)),
                next => Err((Expect::Str, next))?,
            },
            Some(Token::Str(_, parts)) => Term::Str(None, self.str_parts(parts)),
            next => Err((Expect::Key, next))?,
        })
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
        let head = |p: &mut Self| p.keyword("def").ok();
        core::iter::from_fn(|| self.maybe(head).map(|_| self.def_tail())).collect()
    }

    /// Parse `name args ":" term ";"`.
    fn def_tail(&mut self) -> Result<'s, 't, Def<&'s str, Term<&'s str>>> {
        let name = match self.i.next() {
            Some(Token::Word(w)) if !w.starts_with('$') && !w.contains("::") => w,
            next => Err((Expect::Ident, next))?,
        };
        let args = self.args(|p| {
            Ok(match p.i.next() {
                Some(Token::Word(w)) if !w.contains("::") => *w,
                next => Err((Expect::Arg, next))?,
            })
        });
        self.char1(":")?;

        let body = self.term()?;
        self.char1(";")?;

        Ok(Def { name, args, body })
    }

    fn bare_str(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            next @ Some(Token::Str(_, parts)) => match parts[..] {
                [StrPart::Str(s)] => Ok(s),
                _ => Err((Expect::Str, next))?,
            },
            next => Err((Expect::Str, next))?,
        }
    }

    fn include(&mut self) -> Result<'s, 't, (&'s str, Option<&'s str>)> {
        self.bare_str().map(|path| (path, None))
    }

    fn import(&mut self) -> Result<'s, 't, (&'s str, Option<&'s str>)> {
        let path = self.bare_str()?;
        self.keyword("as")?;
        let name = match self.i.next() {
            Some(Token::Word(w)) if !w.starts_with(['$', '@']) && !w.contains("::") => *w,
            next => Err((Expect::Ident, next))?,
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
                Some(Token::Word("module")) => Some(p.terminated(Self::term)),
                _ => None,
            })
            .transpose()?;

        let deps = core::iter::from_fn(|| {
            self.maybe(|p| match p.i.next() {
                Some(Token::Word("include")) => Some(p.terminated(Self::include)),
                Some(Token::Word("import")) => Some(p.terminated(Self::import)),
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
    pub(crate) body: B,
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
    pub(crate) name: S,
    pub(crate) args: Vec<S>,
    /// Body of the filter, e.g. `[.[] | f]`.
    pub(crate) body: F,
}

pub(crate) type Defs<S> = Vec<Def<S, Term<S>>>;
