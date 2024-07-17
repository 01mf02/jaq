//! Parsing.

use crate::lex::{StrPart, Token};
use crate::path;
use alloc::{boxed::Box, vec::Vec};

/// Parse error, storing what we expected and what we got instead.
pub type Error<'s, 't> = (Expect<&'s str>, Option<&'t Token<&'s str>>);

/// Type of token that we expected.
#[derive(Debug)]
pub enum Expect<S> {
    Keyword(S),
    Char(S),
    Var,
    ElseOrEnd,
    CommaOrRBrace,
    SemicolonOrRParen,
    Term,
    Key,
    Ident,
    Arg,
    Str,
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
            Self::Ident => "ident",
            Self::Arg => "argument",
            Self::Str => "string",
            Self::Nothing => "nothing",
        }
    }
}

/// Output of a fallible parsing operation.
pub type Result<'s, 't, T> = core::result::Result<T, Error<'s, 't>>;

/// Parser for jq programs.
pub struct Parser<'s, 't> {
    i: core::slice::Iter<'t, Token<&'s str>>,
    e: Vec<Error<'s, 't>>,
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

    /// Integer or floating-point number.
    Num(S),
    /// String
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

    /// Path such as `.`, `.a`, `.[][]."b"`
    Path(Box<Self>, Vec<(path::Part<Self>, path::Opt)>),
}

impl<S> Term<S> {
    fn str(s: S) -> Self {
        Self::Str(None, [StrPart::Str(s)].into())
    }
}

/// Keywords that may not appear at the beginning of an expression.
///
/// Note that for example `reduce` is not part of this list,
/// because it *can* appear at the beginning of an expression.
const KEYWORDS: &[&str] = &[
    "include", "import", "def", "as", "and", "or", "catch", "then", "elif", "else", "end",
];

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
    pub fn parse<T: Default, F>(mut self, f: F) -> core::result::Result<T, Vec<Error<'s, 't>>>
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

    fn verify_last(&mut self, last: &'static str) -> Result<'s, 't, ()> {
        match (self.i.as_slice(), last) {
            ([], "") => Ok(()),
            ([Token::Char(c)], last) if *c == last => Ok(()),
            ([], _) => Err((Expect::Char(last), None)),
            ([next, ..], "") => Err((Expect::Nothing, Some(next))),
            ([next, ..], _) => Err((Expect::Char(last), Some(next))),
        }
    }

    /// Run given parse function with given tokens, then reset tokens to previous tokens.
    fn with_tok<T>(&mut self, tokens: &'t [Token<&'s str>], f: impl FnOnce(&mut Self) -> T) -> T {
        let i = core::mem::replace(&mut self.i, tokens.iter());
        let y = f(self);
        self.i = i;
        y
    }

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

    fn maybe<T>(&mut self, f: impl Fn(&mut Self) -> Option<T>) -> Option<T> {
        let i = self.i.clone();
        let y = f(self);
        // rewind to previous state in case of non-match
        if y.is_none() {
            self.i = i;
        }
        y
    }

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
                Some(Token::Char(";")) => y.push(f(self)?),
                Some(Token::Char(")")) => break,
                next => return Err((Expect::SemicolonOrRParen, next)),
            }
        }
        Ok(y)
    }

    fn args<T>(&mut self, f: fn(&mut Self) -> Result<'s, 't, T>) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Block("(", tokens)) => Some(p.with(tokens, "", |p| p.arg_items(f))),
            _ => None,
        })
        .unwrap_or_default()
    }

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
            next => Err((Expect::Char(c), next)),
        }
    }

    fn keyword(&mut self, kw: &'static str) -> Result<'s, 't, ()> {
        match self.i.next() {
            Some(Token::Word(w)) if *w == kw => Ok(()),
            next => Err((Expect::Keyword(kw), next)),
        }
    }

    fn var(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            Some(Token::Word(x)) if x.starts_with('$') => Ok(*x),
            next => Err((Expect::Var, next)),
        }
    }

    fn pipe(&mut self) -> Result<'s, 't, ()> {
        match self.i.next() {
            Some(Token::Op("|")) => Ok(()),
            next => Err((Expect::Char("|"), next)),
        }
    }

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
                let if_then = |p: &mut Self| {
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
                        next => return Err((Expect::ElseOrEnd, next)),
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
                    Some(Token::Str(_, parts, _)) => Some(p.str_parts(parts)),
                    _ => None,
                });
                match s {
                    None => Term::Call(*id, Vec::new()),
                    Some(parts) => Term::Str(Some(*id), parts),
                }
            }
            Some(Token::Word(id)) if !KEYWORDS.contains(id) => {
                Term::Call(*id, self.args(Self::term))
            }
            Some(Token::Char(".")) => {
                let key_opt = self.maybe(|p| p.key_opt().ok());
                let path: Vec<_> = key_opt.into_iter().chain(self.path()?).collect();
                if path.is_empty() {
                    Term::Id
                } else {
                    Term::Path(Box::new(Term::Id), path)
                }
            }
            Some(Token::Char("..")) => Term::Recurse,
            Some(Token::Num(n)) => Term::Num(*n),
            Some(Token::Block("[", tokens)) if matches!(tokens[..], [Token::Char("]")]) => {
                Term::Arr(None)
            }
            Some(Token::Block("{", tokens)) if matches!(tokens[..], [Token::Char("}")]) => {
                Term::Obj(Vec::new())
            }
            Some(Token::Block("(", tokens)) => self.with(tokens, ")", Self::term),
            Some(Token::Block("[", tokens)) => {
                Term::Arr(Some(Box::new(self.with(tokens, "]", Self::term))))
            }
            Some(Token::Block("{", tokens)) => {
                self.with(tokens, "", |p| p.obj_items(Self::obj_entry).map(Term::Obj))
            }
            Some(Token::Str(_, parts, _)) => Term::Str(None, self.str_parts(parts)),
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

    fn obj_entry(&mut self) -> Result<'s, 't, (Term<&'s str>, Option<Term<&'s str>>)> {
        let key = match self.i.next() {
            Some(Token::Str(_, parts, _)) => Term::Str(None, self.str_parts(parts)),
            Some(Token::Word(k)) if k.starts_with('@') => match self.i.next() {
                Some(Token::Str(_, parts, _)) => Term::Str(Some(*k), self.str_parts(parts)),
                next => return Err((Expect::Str, next)),
            },
            Some(Token::Word(k)) if k.starts_with('$') => Term::Var(*k),
            Some(Token::Word(k)) if !KEYWORDS.contains(k) => Term::str(*k),
            Some(Token::Block("(", tokens)) => {
                let k = self.with(tokens, ")", Self::term);
                self.char1(":")?;
                return Ok((k, Some(self.term_with_comma(false)?)));
            }
            next => return Err((Expect::Key, next)),
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
            StrPart::Filter(Token::Block("(", tokens)) => {
                StrPart::Filter(self.with(tokens, ")", Self::term))
            }
            StrPart::Filter(_) => unreachable!(),
            StrPart::Char(c) => StrPart::Char(*c),
        });
        parts.collect()
    }

    fn path(&mut self) -> Result<'s, 't, Vec<(path::Part<Term<&'s str>>, path::Opt)>> {
        let mut path: Vec<_> = core::iter::from_fn(|| self.path_part_opt()).collect();
        while self.char0('.').is_some() {
            path.push(self.key_opt()?);
            path.extend(core::iter::from_fn(|| self.path_part_opt()));
        }
        Ok(path)
    }

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
            Some(Token::Block("[", tokens)) => Some(p.with(tokens, "]", Self::path_part)),
            _ => None,
        })?;
        Some((part, self.opt()))
    }

    fn key_opt(&mut self) -> Result<'s, 't, (path::Part<Term<&'s str>>, path::Opt)> {
        let key = match self.i.next() {
            Some(Token::Word(id)) if id.starts_with('@') => todo!(),
            Some(Token::Str(_, parts, _)) => Term::Str(None, self.str_parts(parts)),
            Some(Token::Word(id)) if !id.starts_with('$') && !KEYWORDS.contains(id) => {
                Term::str(*id)
            }
            next => return Err((Expect::Key, next)),
        };
        Ok((path::Part::Index(key), self.opt()))
    }

    fn opt(&mut self) -> path::Opt {
        let mut opt = path::Opt::Essential;
        while self.char0('?').is_some() {
            opt = path::Opt::Optional;
        }
        opt
    }

    /// Parse a sequence of definitions, such as `def x: 1; def y: 2;`.
    pub fn defs(&mut self) -> Result<'s, 't, Vec<Def<&'s str, Term<&'s str>>>> {
        core::iter::from_fn(|| self.def_head().map(|()| self.def_tail())).collect()
    }

    fn def_head(&mut self) -> Option<()> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Word("def")) => Some(()),
            _ => None,
        })
    }

    fn def_tail(&mut self) -> Result<'s, 't, Def<&'s str, Term<&'s str>>> {
        let name = match self.i.next() {
            Some(Token::Word(name)) if !name.starts_with('$') && is_id(name) => name,
            next => return Err((Expect::Ident, next)),
        };
        let args = self.args(|p| {
            Ok(match p.i.next() {
                Some(Token::Word(arg)) if is_id(arg) => *arg,
                next => return Err((Expect::Arg, next)),
            })
        });
        self.char1(":")?;

        let body = self.term()?;
        self.char1(";")?;

        Ok(Def { name, args, body })
    }

    fn bare_str(&mut self) -> Result<'s, 't, &'s str> {
        match self.i.next() {
            next @ Some(Token::Str(_, parts, _)) => match parts[..] {
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
        self.keyword("as")?;
        let name = match self.i.next() {
            Some(Token::Word(name)) if !name.starts_with(['$', '@']) && is_id(name) => *name,
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
                Some(Token::Word("module")) => Some(p.terminated(Self::term)),
                _ => None,
            })
            .transpose()?;

        let mods = core::iter::from_fn(|| {
            self.maybe(|p| match p.i.next() {
                Some(Token::Word("include")) => Some(p.terminated(Self::include)),
                Some(Token::Word("import")) => Some(p.terminated(Self::import)),
                _ => None,
            })
        })
        .collect::<Result<_>>()?;

        let body = f(self)?;

        Ok(Module { meta, mods, body })
    }
}

fn is_id(s: &str) -> bool {
    !s.contains("::") && !KEYWORDS.contains(&s)
}

#[derive(Debug, Default)]
pub struct Module<S, B> {
    meta: Option<Term<S>>,
    pub(crate) mods: Vec<(S, Option<S>)>,
    pub(crate) body: B,
}

#[derive(Debug)]
pub struct Def<S, F> {
    pub(crate) name: S,
    pub(crate) args: Vec<S>,
    /// Body of the filter, e.g. `[.[] | f]`.
    pub(crate) body: F,
}
