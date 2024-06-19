use crate::lex::{StrPart, Token};
use alloc::{boxed::Box, vec::Vec};
use jaq_syn::path;

type Error<'a> = (Expect, Option<&'a Token<&'a str>>);
#[derive(Debug)]
pub enum Expect {
    Keyword(&'static str),
    Char(char),
    Var,
    ElseOrEnd,
    Term,
    Key,
    Ident,
    Arg,
    Str,
    Nothing,
}

type Result<'a, T> = core::result::Result<T, Error<'a>>;

pub struct Parser<'a> {
    i: core::slice::Iter<'a, Token<&'a str>>,
    pub e: Vec<Error<'a>>,
    /// names of fold-like filters, e.g. "reduce" and "foreach"
    fold: &'a [&'a str],
}

#[derive(Debug, Default)]
pub enum Term<S> {
    #[default]
    Id,
    Recurse,

    Num(S),
    Str(Option<S>, Vec<StrPart<S, Self>>),
    Arr(Option<Box<Self>>),
    Obj(Vec<(Self, Option<Self>)>),

    Neg(Box<Self>),
    Pipe(Box<Self>, Option<S>, Box<Self>),
    BinOp(Box<Self>, Vec<(S, Self)>),

    Label(S, Box<Self>),
    Break(S),

    Fold(S, Box<Self>, S, Vec<Self>),
    TryCatch(Box<Self>, Option<Box<Self>>),
    IfThenElse(Vec<(Self, Self)>, Option<Box<Self>>),

    Def(Vec<Def<S, Self>>, Box<Self>),
    Call(S, Vec<Self>),
    Var(S),

    Key(S),
    Path(Box<Self>, Vec<(path::Part<Self>, path::Opt)>),
}

/// Keywords that may not appear at the beginning of an expression.
///
/// Note that for example `reduce` is not part of this list,
/// because it *can* appear at the beginning of an expression.
const KEYWORDS: &[&str] = &[
    "include", "import", "def", "as", "and", "or", "catch", "then", "elif", "else", "end",
];

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(i: &'a [Token<&'a str>]) -> Self {
        Self {
            i: i.iter(),
            e: Vec::new(),
            fold: &["reduce", "foreach"],
        }
    }

    fn with<T: Default, F>(&mut self, tokens: &'a [Token<&'a str>], last: &'a str, f: F) -> T
    where
        F: FnOnce(&mut Self) -> Result<'a, T>,
    {
        let i = core::mem::replace(&mut self.i, tokens.iter());
        let y = match f(self) {
            Ok(y) => {
                match (self.i.as_slice(), last) {
                    ([], "") => (),
                    ([], _) => panic!(),
                    ([next, ..], "") => self.e.push((Expect::Nothing, Some(next))),
                    ([Token::Char(c)], last) if *c == last => (),
                    ([next, ..], last) => self
                        .e
                        .push((Expect::Char(last.chars().next().unwrap()), Some(next))),
                }
                y
            }
            Err(e) => {
                self.e.push(e);
                T::default()
            }
        };
        self.i = i;
        y
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

    fn try_maybe<T, F>(&mut self, f: F) -> Result<'a, Option<T>>
    where
        F: Fn(&mut Self) -> Result<'a, Option<T>>,
    {
        let i = self.i.clone();
        let y = f(self)?;
        // rewind to previous state in case of non-match
        if y.is_none() {
            self.i = i;
        }
        Ok(y)
    }

    fn sep_by1<T, F>(&mut self, sep: char, f: F) -> Result<'a, Vec<T>>
    where
        F: Fn(&mut Self) -> Result<'a, T>,
    {
        let mut ys = Vec::from([f(self)?]);
        loop {
            match self.i.next() {
                Some(Token::Char(c)) if c.chars().eq([sep]) => ys.push(f(self)?),
                Some(Token::Char(")" | "}")) => return Ok(ys),
                next => return Err((Expect::Char(sep), next)),
            }
        }
    }

    fn args<T>(&mut self, f: impl Fn(&mut Self) -> Result<'a, T> + Copy) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Block("(", tokens)) => Some(p.with(tokens, "", |p| p.sep_by1(';', f))),
            _ => None,
        })
        .unwrap_or_default()
    }

    fn op(&mut self, with_comma: bool) -> Option<&'a str> {
        self.maybe(|p| match p.i.next() {
            // handle pipe directly in `term()`
            Some(Token::Op("|")) => None,
            Some(Token::Op(o) | Token::Word(o @ ("and" | "or"))) => Some(*o),
            Some(Token::Char(o @ ",")) if with_comma => Some(*o),
            _ => None,
        })
    }

    fn char0(&mut self, c: char) -> Option<&'a str> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Char(s)) if s.chars().eq([c]) => Some(*s),
            _ => None,
        })
    }

    fn terminated<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<'a, T>) -> Result<'a, T> {
        let y = f(self)?;
        self.char1(';')?;
        Ok(y)
    }

    fn char1(&mut self, c: char) -> Result<'a, &'a str> {
        match self.i.next() {
            Some(Token::Char(s)) if s.chars().eq([c]) => Ok(*s),
            next => Err((Expect::Char(c), next)),
        }
    }

    fn keyword(&mut self, kw: &'static str) -> Result<'a, ()> {
        match self.i.next() {
            Some(Token::Word(w)) if *w == kw => Ok(()),
            next => Err((Expect::Keyword(kw), next)),
        }
    }

    fn var(&mut self) -> Result<'a, &'a str> {
        match self.i.next() {
            Some(Token::Word(x)) if x.starts_with('$') => Ok(*x),
            next => Err((Expect::Var, next)),
        }
    }

    fn pipe(&mut self) -> Result<'a, ()> {
        match self.i.next() {
            Some(Token::Op("|")) => Ok(()),
            next => Err((Expect::Char('|'), next)),
        }
    }

    pub fn term_with_comma(&mut self, with_comma: bool) -> Result<'a, Term<&'a str>> {
        if let Some(tm) = self.try_maybe(|p| match p.i.next() {
            Some(Token::Word("label")) => {
                let v = p.var()?;
                p.pipe()?;
                let tm = p.term_with_comma(with_comma)?;
                Ok(Some(Term::Label(v, Box::new(tm))))
            }
            _ => Ok(None),
        })? {
            return Ok(tm);
        }

        let head = self.atom_path()?;
        let mut tail = Vec::new();
        while let Some(op) = self.op(with_comma) {
            tail.push((op, self.atom_path()?));
        }

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

    fn atom(&mut self) -> Result<'a, Term<&'a str>> {
        Ok(match self.i.next() {
            Some(Token::Op("-")) => Term::Neg(Box::new(self.atom_path()?)),
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
                let try_ = self.atom_path()?;
                let catch = self.try_maybe(|p| match p.i.next() {
                    Some(Token::Word("catch")) => Ok(Some(p.atom_path()?)),
                    _ => Ok(None),
                })?;
                Term::TryCatch(Box::new(try_), catch.map(Box::new))
            }
            Some(Token::Word("break")) => Term::Break(self.var()?),
            Some(Token::Word(fold)) if self.fold.contains(fold) => {
                let xs = self.atom_path()?;
                self.keyword("as")?;
                let x = self.var()?;
                let args = self.args(Self::term);
                Term::Fold(*fold, Box::new(xs), x, args)
            }
            Some(Token::Word(id)) if id.starts_with('$') => Term::Var(*id),
            Some(Token::Word(id)) if id.starts_with('@') => {
                let s = self.maybe(|p| match p.i.next() {
                    Some(Token::Str(parts)) if id.starts_with('@') => Some(p.str_parts(parts)),
                    _ => None,
                });
                match s {
                    None => Term::Call(*id, Vec::new()),
                    Some(parts) => Term::Str(Some(id), parts),
                }
            }
            Some(Token::Word(id)) if !KEYWORDS.contains(id) => {
                Term::Call(*id, self.args(Self::term))
            }
            Some(Token::Char(".")) => self
                .maybe(|p| p.i.next().and_then(ident_key))
                .map_or(Term::Id, Term::Key),
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
            Some(Token::Block("{", tokens)) => self.with(tokens, "", |p| {
                p.sep_by1(',', Self::obj_entry).map(Term::Obj)
            }),
            Some(Token::Str(parts)) => Term::Str(None, self.str_parts(parts)),
            next => return Err((Expect::Term, next)),
        })
    }

    fn atom_path(&mut self) -> Result<'a, Term<&'a str>> {
        let tm = self.atom()?;

        let tm = match self.opt() {
            path::Opt::Optional => Term::TryCatch(Box::new(tm), None),
            path::Opt::Essential => tm,
        };

        let mut path: Vec<_> = core::iter::from_fn(|| self.path_part_opt()).collect();
        while self.char0('.').is_some() {
            use path::Opt;
            let key = match self.i.next() {
                Some(Token::Word(id)) if !id.starts_with(['$', '@']) => *id,
                next => return Err((Expect::Key, next)),
            };
            let opt = self.char0('?').is_some();
            let key = Term::Str(None, Vec::from([StrPart::Str(key)]));
            let opt = if opt { Opt::Optional } else { Opt::Essential };
            path.push((path::Part::Index(key), opt));
            path.extend(core::iter::from_fn(|| self.path_part_opt()));
        }
        Ok(if path.is_empty() {
            tm
        } else {
            Term::Path(Box::new(tm), path)
        })
    }

    pub fn term(&mut self) -> Result<'a, Term<&'a str>> {
        let defs = self.defs()?;
        let tm = self.term_with_comma(true)?;

        Ok(if defs.is_empty() {
            tm
        } else {
            Term::Def(defs, Box::new(tm))
        })
    }

    fn obj_entry(&mut self) -> Result<'a, (Term<&'a str>, Option<Term<&'a str>>)> {
        let key = match self.i.next() {
            Some(Token::Str(parts)) => Term::Str(None, self.str_parts(parts)),
            Some(Token::Word(k)) if k.starts_with('@') => match self.i.next() {
                Some(Token::Str(parts)) => Term::Str(Some(*k), self.str_parts(parts)),
                next => return Err((Expect::Str, next)),
            },
            Some(Token::Word(k)) if k.starts_with('$') => Term::Var(*k),
            Some(Token::Word(k)) if !KEYWORDS.contains(k) => {
                Term::Str(None, Vec::from([StrPart::Str(*k)]))
            }
            Some(Token::Block("(", tokens)) => {
                let k = self.with(tokens, ")", Self::term);
                self.char1(':')?;
                return Ok((k, Some(self.term()?)));
            }
            next => return Err((Expect::Key, next)),
        };
        let v = self.char0(':').map(|_| self.term_with_comma(false));
        Ok((key, v.transpose()?))
    }

    fn str_parts(
        &mut self,
        parts: &'a [StrPart<&'a str, Token<&'a str>>],
    ) -> Vec<StrPart<&'a str, Term<&'a str>>> {
        let parts = parts.iter().map(|part| match part {
            StrPart::Str(s) => StrPart::Str(*s),
            StrPart::Filter(Token::Block("(", tokens)) => {
                StrPart::Filter(self.with(tokens, ")", Self::term))
            }
            StrPart::Filter(_) => unreachable!(),
            StrPart::Char(c) => StrPart::Char(*c),
            StrPart::Unicode(u) => StrPart::Unicode(*u),
        });
        parts.collect()
    }

    fn path_part(&mut self) -> Result<'a, path::Part<Term<&'a str>>> {
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

    fn path_part_opt(&mut self) -> Option<(path::Part<Term<&'a str>>, path::Opt)> {
        let part = self.maybe(|p| match p.i.next() {
            Some(Token::Block("[", tokens)) => Some(p.with(tokens, "]", Self::path_part)),
            _ => None,
        })?;
        Some((part, self.opt()))
    }

    fn opt(&mut self) -> path::Opt {
        let mut opt = path::Opt::Essential;
        while self.char0('?').is_some() {
            opt = path::Opt::Optional;
        }
        opt
    }

    pub fn defs(&mut self) -> Result<'a, Vec<Def<&'a str, Term<&'a str>>>> {
        core::iter::from_fn(|| self.def_head().map(|()| self.def_tail())).collect()
    }

    fn def_head(&mut self) -> Option<()> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Word("def")) => Some(()),
            _ => None,
        })
    }

    fn def_tail(&mut self) -> Result<'a, Def<&'a str, Term<&'a str>>> {
        let name = match self.i.next() {
            Some(Token::Word(name)) if !name.starts_with(['$']) => name,
            next => return Err((Expect::Ident, next)),
        };
        let args = self.args(|p| {
            Ok(match p.i.next() {
                Some(Token::Word(arg)) if !arg.starts_with('@') => *arg,
                next => return Err((Expect::Arg, next)),
            })
        });
        self.char1(':')?;

        let body = self.term()?;
        self.char1(';')?;

        Ok(Def { name, args, body })
    }

    fn bare_str(&mut self) -> Result<'a, &'a str> {
        match self.i.next() {
            Some(Token::Str(parts)) => match parts[..] {
                [StrPart::Str(s)] => Ok(s),
                _ => todo!(),
            },
            next => Err((Expect::Str, next)),
        }
    }

    fn include(&mut self) -> Result<'a, (&'a str, Option<&'a str>)> {
        self.bare_str().map(|path| (path, None))
    }

    fn import(&mut self) -> Result<'a, (&'a str, Option<&'a str>)> {
        let path = self.bare_str()?;
        self.keyword("as")?;
        let name = match self.i.next() {
            Some(Token::Word(name)) if !name.starts_with(['$', '@']) => *name,
            next => return Err((Expect::Ident, next)),
        };
        Ok((path, Some(name)))
    }

    pub fn module<B, F>(&mut self, f: F) -> Result<'a, Module<&'a str, B>>
    where
        F: FnOnce(&mut Self) -> Result<'a, B>,
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

#[derive(Debug)]
pub struct Module<S, B> {
    meta: Option<Term<S>>,
    mods: Vec<(S, Option<S>)>,
    body: B,
}

#[derive(Debug)]
pub struct Def<S, F> {
    name: S,
    args: Vec<S>,
    /// Body of the filter, e.g. `[.[] | f]`.
    body: F,
}

fn ident_key<'a>(token: &Token<&'a str>) -> Option<&'a str> {
    match token {
        Token::Word(id) if !id.starts_with(['$', '@']) && !KEYWORDS.contains(id) => Some(*id),
        _ => None,
    }
}
