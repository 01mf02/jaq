use crate::lex::{self, Token};
use crate::parse::{self, Defs, Term};
use alloc::string::String;
use alloc::vec::Vec;

struct Arena(typed_arena::Arena<String>);
struct DefsMods<S>(Vec<(String, Result<Module<S, Defs<S>>, Error<S>>)>);

enum Error<S> {
    Io(String),
    Lex(Vec<lex::Error<S>>),
    Parse(Vec<parse::Error<S>>),
}

#[derive(Default)]
pub struct Module<S, B> {
    pub meta: Option<Term<S>>,
    pub mods: Vec<(usize, Option<S>)>,
    pub vars: Vec<(S, S)>,
    pub body: B,
}

impl<'s, B> Module<&'s str, B> {
    fn map_mods(m: parse::Module<&'s str, B>, mut f: impl FnMut(&str) -> usize) -> Self {
        let mut mods = Vec::new();
        let mut vars = Vec::new();
        for (path, as_) in m.deps {
            match as_ {
                Some(x) if x.starts_with('$') => vars.push((path, x)),
                as_ => mods.push((f(path), as_)),
            }
        }
        Module {
            meta: m.meta,
            mods,
            vars,
            body: m.body,
        }
    }
}

impl<'s> DefsMods<&'s str> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn main2<F>(
        &mut self,
        arena: &'s Arena,
        path: &str,
        code: Option<String>,
        f: &F,
    ) -> Module<&'s str, Term<&'s str>>
    where
        F: Fn(&str) -> Result<String, String>,
    {
        let code = match code {
            Some(code) => Ok(code),
            None => f(path).map_err(Error::Io),
        };
        let result = code
            .and_then(|code| parse_main(arena.0.alloc(code)))
            .map(|m| Module::map_mods(m, |path| self.find(arena, path, f)));

        match result {
            Ok(main) => main,
            Err(e) => {
                self.0.push((path.into(), Err(e)));
                Module::default()
            }
        }
    }

    fn find<F>(&mut self, arena: &'s Arena, path: &str, f: &F) -> usize
    where
        F: Fn(&str) -> Result<String, String>,
    {
        if let Some(id) = self.0.iter().position(|(p, _mod)| path == p) {
            return id;
        };

        let result = f(path)
            .map_err(Error::Io)
            .and_then(|file| parse_defs(arena.0.alloc(file)))
            .map(|m| Module::map_mods(m, |path| self.find(arena, path, f)));

        let id = self.0.len();
        self.0.push((path.into(), result));
        id
    }
}

fn parse_main(code: &str) -> Result<parse::Module<&str, Term<&str>>, Error<&str>> {
    let tokens = lex::Lexer::new(code).lex().map_err(Error::Lex)?;
    let conv_err = |(expected, found)| (expected, Token::opt_as_str(found, code));
    parse::Parser::new(&tokens)
        .parse(|p| p.module(|p| p.term()))
        .map_err(|e| Error::Parse(e.into_iter().map(conv_err).collect()))
}

fn parse_defs(code: &str) -> Result<parse::Module<&str, Defs<&str>>, Error<&str>> {
    let tokens = lex::Lexer::new(code).lex().map_err(Error::Lex)?;
    let conv_err = |(expected, found)| (expected, Token::opt_as_str(found, code));
    parse::Parser::new(&tokens)
        .parse(|p| p.module(|p| p.defs()))
        .map_err(|e| Error::Parse(e.into_iter().map(conv_err).collect()))
}
