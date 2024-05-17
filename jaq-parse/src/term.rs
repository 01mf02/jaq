use crate::lex::{Punct, Token};
use crate::Delim;
use jaq_syn::filter::KeyVal;
use jaq_syn::{path, string, Arg, Call, Def};

// TODO: problem: we cannot get a position from a None
// example: (0 | )
// include a scope?
type Error<'a> = (Expect, Option<&'a Token<&'a str>>);
#[derive(Debug)]
pub enum Expect {
    Keyword(&'static str),
    Punct(Punct),
    Var,
    ElseOrEnd,
    Term,
    Key,
    Ident,
    Arg,
    Nothing,
}

type Result<'a, T> = core::result::Result<T, Error<'a>>;

pub(crate) struct Parser<'a> {
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
    Str(string::Str<Self>),
    Arr(Option<Box<Self>>),
    Obj(Vec<KeyVal<Self>>),

    Neg(Box<Self>),
    Pipe(Box<Self>, Option<S>, Box<Self>),
    BinOp(Box<Self>, Vec<(S, Self)>),

    Fold(S, Box<Self>, S, Vec<Self>),
    TryCatch(Box<Self>, Option<Box<Self>>),
    IfThenElse(Box<Self>, Box<Self>, Option<Box<Self>>),

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
    "include", "import", "def", "as", "and", "or", "catch", "then", "else", "end",
];

impl<'a> Parser<'a> {
    pub fn new(i: &'a [Token<&'a str>]) -> Self {
        Self {
            i: i.iter(),
            e: Vec::new(),
            fold: &["reduce", "foreach"],
        }
    }

    fn with<T: Default, F>(&mut self, tokens: &'a [Token<&'a str>], f: F) -> T
    where
        F: FnOnce(&mut Self) -> Result<'a, T>,
    {
        let i = core::mem::replace(&mut self.i, tokens.iter());
        let y = match f(self) {
            Ok(y) => {
                if let Some(next) = self.i.next() {
                    self.e.push((Expect::Nothing, Some(next)));
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

    fn sep_by1<T, F>(&mut self, punct: Punct, f: F) -> Result<'a, Vec<T>>
    where
        F: Fn(&mut Self) -> Result<'a, T>,
    {
        let mut ys = Vec::from([f(self)?]);
        loop {
            match self.i.next() {
                Some(Token::Punct(p, _)) if *p == punct => ys.push(f(self)?),
                None => return Ok(ys),
                next => return Err((Expect::Punct(punct), next)),
            }
        }
    }

    fn args<T>(&mut self, f: impl Fn(&mut Self) -> Result<'a, T> + Copy) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Delim(Delim::Paren, tokens)) => {
                Some(p.with(tokens, |p| p.sep_by1(Punct::Semicolon, f)))
            }
            _ => None,
        })
        .unwrap_or_default()
    }

    fn op(&mut self, with_comma: bool) -> Option<&'a str> {
        self.maybe(|p| match p.i.next() {
            // handle pipe directly in `term()`
            Some(Token::Op("|")) => None,
            Some(Token::Op(o) | Token::Word(o @ ("and" | "or"))) => Some(*o),
            Some(Token::Punct(Punct::Comma, o)) if with_comma => Some(*o),
            _ => None,
        })
    }

    fn punct(&mut self, punct: Punct) -> Option<&'a str> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Punct(p, s)) if *p == punct => Some(*s),
            _ => None,
        })
    }

    fn punct1(&mut self, punct: Punct) -> Result<'a, &'a str> {
        match self.i.next() {
            Some(Token::Punct(p, s)) if *p == punct => Ok(*s),
            next => Err((Expect::Punct(punct), next)),
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

    pub fn term_with_comma(&mut self, with_comma: bool) -> Result<'a, Term<&'a str>> {
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
                match p.i.next() {
                    Some(Token::Op("|")) => Ok(Some(Some(x))),
                    next => Err((todo!(), next)),
                }
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
                let if_ = self.term()?;
                self.keyword("then")?;
                let then_ = self.term()?;
                let else_ = match self.i.next() {
                    Some(Token::Word("else")) => {
                        let else_ = self.term()?;
                        self.keyword("end")?;
                        Some(else_)
                    }
                    Some(Token::Word("end")) => None,
                    next => return Err((Expect::ElseOrEnd, next)),
                };
                Term::IfThenElse(Box::new(if_), Box::new(then_), else_.map(Box::new))
            }
            Some(Token::Word("try")) => {
                let try_ = self.atom_path()?;
                let catch = self.try_maybe(|p| match p.i.next() {
                    Some(Token::Word("catch")) => Ok(Some(p.atom_path()?)),
                    _ => Ok(None),
                })?;
                Term::TryCatch(Box::new(try_), catch.map(Box::new))
            }
            Some(Token::Word(fold)) if self.fold.contains(fold) => {
                let xs = self.atom_path()?;
                self.keyword("as")?;
                let x = self.var()?;
                let args = self.args(|p| p.term());
                Term::Fold(*fold, Box::new(xs), x, args)
            }
            Some(Token::Word(id)) if id.starts_with('$') => Term::Var(*id),
            Some(Token::Word(id)) if !KEYWORDS.contains(id) => {
                let head = Term::Call(*id, self.args(|p| p.term()));
                let s = self.maybe(|p| match p.i.next() {
                    Some(Token::Str(parts)) if id.starts_with('@') => Some(p.str_parts(parts)),
                    _ => None,
                });
                match s {
                    None => head,
                    Some(parts) => Term::Str(string::Str {
                        fmt: Some(Box::new(head)),
                        parts,
                    }),
                }
            }
            Some(Token::Punct(Punct::Dot, _)) => self
                .maybe(|p| p.i.next().and_then(ident_key))
                .map_or(Term::Id, Term::Key),
            Some(Token::Punct(Punct::DotDot, _)) => Term::Recurse,
            Some(Token::Num(n)) => Term::Num(*n),
            Some(Token::Delim(Delim::Paren, tokens)) => self.with(tokens, |p| p.term()),
            Some(Token::Delim(Delim::Brack, tokens)) if tokens.is_empty() => Term::Arr(None),
            Some(Token::Delim(Delim::Brack, tokens)) => {
                Term::Arr(Some(Box::new(self.with(tokens, |p| p.term()))))
            }
            Some(Token::Delim(Delim::Brace, tokens)) if tokens.is_empty() => Term::Obj(Vec::new()),
            Some(Token::Delim(Delim::Brace, tokens)) => self.with(tokens, |p| {
                p.sep_by1(Punct::Comma, |p| p.obj_entry()).map(Term::Obj)
            }),
            Some(Token::Str(parts)) => Term::Str(string::Str {
                fmt: None,
                parts: self.str_parts(parts),
            }),
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
        while self.punct(Punct::Dot).is_some() {
            use path::Opt;
            let key = match self.i.next() {
                Some(Token::Word(id)) if !id.starts_with(['$', '@']) => Some(*id),
                next => return Err((Expect::Key, next)),
            };
            let opt = self.punct(Punct::Question).is_some();
            let key = Term::Str(string::Str::from(key.unwrap_or("").to_string()));
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

    fn term(&mut self) -> Result<'a, Term<&'a str>> {
        self.term_with_comma(true)
    }

    fn obj_entry(&mut self) -> Result<'a, KeyVal<Term<&'a str>>> {
        match self.i.next() {
            Some(Token::Word(k)) if !k.starts_with(['$', '@']) => {
                let k = string::Str::from(k.to_string());
                let v = self
                    .punct(Punct::Colon)
                    .map(|_| self.term_with_comma(false))
                    .transpose()?;
                Ok(KeyVal::Str(k, v))
            }
            // TODO: handle $x
            Some(Token::Delim(Delim::Paren, tokens)) => {
                let k = self.with(tokens, |p| p.term());
                self.punct1(Punct::Colon)?;
                Ok(KeyVal::Filter(k, self.term()?))
            }
            next => Err((todo!(), next)),
        }
    }

    fn str_parts(
        &mut self,
        parts: &'a [string::Part<Token<&'a str>>],
    ) -> Vec<string::Part<Term<&'a str>>> {
        let parts = parts.iter().map(|part| match part {
            string::Part::Str(s) => string::Part::Str(s.clone()),
            string::Part::Fun(Token::Delim(Delim::Paren, tokens)) => {
                string::Part::Fun(self.with(tokens, |p| p.term()))
            }
            string::Part::Fun(_) => unreachable!(),
        });
        parts.collect()
    }

    fn path_part(&mut self) -> Result<'a, path::Part<Term<&'a str>>> {
        use path::Part::{Index, Range};
        Ok(if self.i.as_slice().is_empty() {
            Range(None, None)
        } else if self.punct(Punct::Colon).is_some() {
            Range(None, Some(self.term()?))
        } else {
            let tm = self.term()?;
            if self.punct(Punct::Colon).is_some() {
                if self.i.as_slice().is_empty() {
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
            Some(Token::Delim(Delim::Brack, tokens)) => Some(p.with(&tokens, |p| p.path_part())),
            _ => None,
        })?;
        Some((part, self.opt()))
    }

    fn opt(&mut self) -> path::Opt {
        let mut opt = path::Opt::Essential;
        while self.punct(Punct::Question).is_some() {
            opt = path::Opt::Optional;
        }
        opt
    }

    pub fn main(&mut self) -> Result<'a, Main<Term<&'a str>>> {
        Ok(Main {
            defs: self.defs()?,
            body: self.i.as_slice(),
        }
        .map(&mut |tokens| self.with(tokens, |p| p.term())))
    }

    pub fn defs(&mut self) -> Result<'a, Vec<Def<Main<&'a [Token<&'a str>]>>>> {
        core::iter::from_fn(|| self.def_head().map(|()| self.def_tail())).collect()
    }

    pub fn def_rhs(&mut self) -> Result<'a, Main<&'a [Token<&'a str>]>> {
        let defs = self.defs()?;
        let i = self.i.as_slice();
        let is_semicolon = |tk| matches!(tk, &Token::Punct(Punct::Semicolon, _));
        let body = match self.i.position(is_semicolon) {
            None => return Err((Expect::Punct(Punct::Semicolon), None)),
            Some(p) => &i[..p],
        };
        Ok(Main { defs, body })
    }

    fn def_head(&mut self) -> Option<()> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Word("def")) => Some(()),
            _ => None,
        })
    }

    fn def_lhs(&mut self) -> Result<'a, Call> {
        let name = match self.i.next() {
            Some(Token::Word(name)) if !name.starts_with(['$', '@']) => name.to_string(),
            next => return Err((Expect::Ident, next)),
        };
        let args = self.args(|p| {
            Ok(match p.i.next() {
                Some(Token::Word(v)) if v.starts_with('$') => Arg::Var(v.to_string()),
                Some(Token::Word(arg)) if !arg.starts_with('@') => Arg::Fun(arg.to_string()),
                next => return Err((Expect::Arg, next)),
            })
        });
        Ok(Call { name, args })
    }

    fn def_tail(&mut self) -> Result<'a, Def<Main<&'a [Token<&'a str>]>>> {
        let lhs = self.def_lhs()?;
        self.punct1(Punct::Colon)?;
        let rhs = self.def_rhs()?;

        Ok(Def { lhs, rhs })
    }
}

#[derive(Debug)]
pub struct Main<F> {
    /// Definitions at the top of the filter
    pub defs: Vec<Def<Self>>,
    /// Body of the filter, e.g. `[.[] | f]`.
    pub body: F,
}

impl<F> Main<F> {
    fn map<G>(self, f: &mut impl FnMut(F) -> G) -> Main<G> {
        let defs = self.defs.into_iter().map(|def| Def {
            lhs: def.lhs,
            rhs: def.rhs.map(f),
        });
        Main {
            defs: defs.collect(),
            body: f(self.body),
        }
    }
}

fn ident_key<'a>(token: &Token<&'a str>) -> Option<&'a str> {
    match token {
        Token::Word(id) if !id.starts_with(['$', '@']) => Some(*id),
        _ => None,
    }
}
