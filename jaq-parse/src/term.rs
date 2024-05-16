use crate::lex::{Punct, Token};
use crate::Delim;
use jaq_syn::filter::KeyVal;
use jaq_syn::{path, string};

type Error<'a> = (Expect, Option<&'a Token<&'a str>>);
enum Expect {}

pub(crate) struct Parser<'a> {
    i: core::slice::Iter<'a, Token<&'a str>>,
    e: Vec<Error<'a>>,
}

#[derive(Debug)]
pub enum Term<S> {
    Num(S),
    Str(string::Str<Self>),
    Arr(Option<Box<Self>>),
    Obj(Vec<KeyVal<Self>>),
    Id,
    Recurse,
    Neg(Box<Self>),
    Pipe(Box<Self>, Option<S>, Box<Self>),
    BinOp(Box<Self>, Vec<(S, Self)>),
    IfThenElse(Box<Self>, Box<Self>, Option<Box<Self>>),
    TryCatch(Box<Self>, Option<Box<Self>>),
    Fold(S, Box<Self>, S, Vec<Self>),
    Var(S),
    Call(S, Vec<Self>),
    Key(S),
    Path(Box<Self>, Vec<(path::Part<Self>, path::Opt)>),
}

/// Keywords that may not appear at the beginning of an expression.
///
/// Note that for example `reduce` is not part of this list,
/// because it *can* appear at the beginning of an expression.
const KEYWORDS: &[&str] = &["include", "import", "def", "as", "and", "or"];

impl<'a> Parser<'a> {
    pub fn new(i: &'a [Token<&'a str>]) -> Self {
        Self {
            i: i.iter(),
            e: Vec::new(),
        }
    }

    fn with<T>(&mut self, tokens: &'a [Token<&'a str>], f: impl FnOnce(&mut Self) -> T) -> T {
        let i = core::mem::replace(&mut self.i, tokens.iter());
        let tm = f(self);
        self.i = i;
        tm
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

    fn sep_by1<T>(&mut self, punct: Punct, f: impl Fn(&mut Self) -> T) -> Vec<T> {
        let mut ys = Vec::from([f(self)]);
        loop {
            match self.i.next() {
                Some(Token::Punct(p, _)) if *p == punct => ys.push(f(self)),
                None => break,
                _ => todo!(),
            }
        }
        ys
    }

    fn args<T>(&mut self, f: impl Fn(&mut Self) -> T + Copy) -> Vec<T> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Delim(Delim::Paren, tokens)) => {
                Some(p.with(tokens, |p| p.sep_by1(Punct::Semicolon, f)))
            }
            _ => None,
        })
        .unwrap_or_else(|| Vec::new())
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

    fn var(&mut self) -> &'a str {
        match self.i.next() {
            Some(Token::Word(x)) if x.starts_with('$') => *x,
            _ => todo!(),
        }
    }

    pub fn term_with_comma(&mut self, with_comma: bool) -> Term<&'a str> {
        let head = self.atom();
        let mut tail = Vec::new();
        while let Some(op) = self.op(with_comma) {
            tail.push((op, self.atom()));
        }

        let tm = if tail.is_empty() {
            head
        } else {
            Term::BinOp(Box::new(head), tail)
        };

        let pipe = self.maybe(|p| match p.i.next() {
            Some(Token::Op("|")) => Some(None),
            Some(Token::Word("as")) => {
                let x = p.var();
                match p.i.next() {
                    Some(Token::Op("|")) => Some(Some(x)),
                    _ => None,
                }
            }
            _ => None,
        });
        match pipe {
            None => tm,
            Some(x) => Term::Pipe(Box::new(tm), x, Box::new(self.term_with_comma(with_comma))),
        }
    }

    pub fn term(&mut self) -> Term<&'a str> {
        self.term_with_comma(true)
    }

    fn atom(&mut self) -> Term<&'a str> {
        let tm = match self.i.next() {
            Some(Token::Op("-")) => Term::Neg(Box::new(self.atom())),
            Some(Token::Word("if")) => {
                let if_ = self.term();
                if !matches!(self.i.next(), Some(&Token::Word("then"))) {
                    todo!();
                }
                let then_ = self.term();
                let else_ = match self.i.next() {
                    Some(Token::Word("else")) => {
                        let else_ = self.term();
                        if !matches!(self.i.next(), Some(&Token::Word("end"))) {
                            todo!();
                        }
                        Some(else_)
                    }
                    Some(Token::Word("end")) => None,
                    _ => todo!(),
                };
                Term::IfThenElse(Box::new(if_), Box::new(then_), else_.map(Box::new))
            }
            Some(Token::Word("try")) => {
                let try_ = self.atom();
                let catch = self.maybe(|p| match p.i.next() {
                    Some(Token::Word("catch")) => Some(p.atom()),
                    _ => None,
                });
                Term::TryCatch(Box::new(try_), catch.map(Box::new))
            }
            Some(Token::Word(fold @ ("reduce" | "foreach"))) => {
                let xs = self.term();
                assert!(matches!(self.i.next(), Some(Token::Word("as"))));
                let x = self.var();
                let args = self.args(|p| p.term());
                if args.is_empty() {
                    todo!()
                }
                Term::Fold(*fold, Box::new(xs), x, args)
            },
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
                Term::Obj(p.sep_by1(Punct::Comma, |p| p.obj_entry()))
            }),
            Some(Token::Str(parts)) => Term::Str(string::Str {
                fmt: None,
                parts: self.str_parts(parts),
            }),
            _ => todo!(),
        };

        let tm = match self.opt() {
            path::Opt::Optional => Term::TryCatch(Box::new(tm), None),
            path::Opt::Essential => tm,
        };

        let mut path: Vec<_> = core::iter::from_fn(|| self.path_part_opt()).collect();
        while self.punct(Punct::Dot).is_some() {
            use path::Opt;
            let key = self.i.next().and_then(ident_key).unwrap();
            let opt = self.punct(Punct::Question).is_some();
            let key = Term::Str(string::Str::from(key.to_string()));
            let opt = if opt { Opt::Optional } else { Opt::Essential };
            path.push((path::Part::Index(key), opt));
            path.extend(core::iter::from_fn(|| self.path_part_opt()));
        }
        if path.is_empty() {
            tm
        } else {
            Term::Path(Box::new(tm), path)
        }
    }

    fn obj_entry(&mut self) -> KeyVal<Term<&'a str>> {
        self.maybe(|p| match p.i.next() {
            Some(Token::Word(k)) if !k.starts_with(['$', '@']) => {
                let k = string::Str::from(k.to_string());
                let v = p.punct(Punct::Colon).map(|_| p.term_with_comma(false));
                Some(KeyVal::Str(k, v))
            }
            _ => None,
        })
        .unwrap_or_else(|| {
            let k = self.atom();
            if self.punct(Punct::Colon).is_none() {
                todo!()
            }
            KeyVal::Filter(k, self.term())
        })
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

    fn path_part(&mut self) -> path::Part<Term<&'a str>> {
        use path::Part::{Index, Range};
        if self.i.as_slice().is_empty() {
            Range(None, None)
        } else if self.punct(Punct::Colon).is_some() {
            Range(None, Some(self.term()))
        } else {
            let tm = self.term();
            if self.punct(Punct::Colon).is_some() {
                if self.i.as_slice().is_empty() {
                    Range(Some(tm), None)
                } else {
                    Range(Some(tm), Some(self.term()))
                }
            } else {
                Index(tm)
            }
        }
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
}

fn ident_key<'a>(token: &Token<&'a str>) -> Option<&'a str> {
    match token {
        Token::Word(id) if !id.starts_with(['$', '@']) => Some(*id),
        _ => None,
    }
}
