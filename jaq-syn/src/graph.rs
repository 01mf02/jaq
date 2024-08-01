use crate::lex::{self, Token};
use crate::parse::{self, Defs, Term};
use alloc::string::String;
use alloc::vec::Vec;

struct Arena(typed_arena::Arena<String>);
// list of paths (`String`) and the result of loading the module there
struct Loader<S, R> {
    mods: Vec<(String, Result<Module<S, Defs<S>>, Error<S>>)>,
    read: R,
}

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

impl<'s> Loader<&'s str, fn(&str) -> Result<String, String>> {
    fn new() -> Self {
        Self {
            mods: Vec::from([("/prelude".into(), Ok(Module::default()))]),
            read: |path| Err("module loading not supported".into()),
        }
    }
}

// TODO: std_read, with_std_read
impl<S, R> Loader<S, R> {
    fn with_read<R2>(self, read: R2) -> Loader<S, R2> {
        let mods = self.mods;
        Loader { mods, read }
    }
}

impl<'s, R: Fn(&str) -> Result<String, String>> Loader<&'s str, R> {
    fn main2(
        &mut self,
        arena: &'s Arena,
        path: &str,
        code: Option<String>,
    ) -> Module<&'s str, Term<&'s str>> {
        let code = match code {
            Some(code) => Ok(code),
            None => (self.read)(path).map_err(Error::Io),
        };
        let result = code
            .and_then(|code| parse_main(arena.0.alloc(code)))
            .map(|m| Module::map_mods(m, |path| self.find(arena, path)));

        match result {
            Ok(main) => main,
            Err(e) => {
                self.mods.push((path.into(), Err(e)));
                Module::default()
            }
        }
    }

    fn find(&mut self, arena: &'s Arena, path: &str) -> usize {
        if let Some(id) = self.mods.iter().position(|(p, _mod)| path == p) {
            return id;
        };

        let result = (self.read)(path)
            .map_err(Error::Io)
            .and_then(|file| parse_defs(arena.0.alloc(file)))
            .map(|m| Module::map_mods(m, |path| self.find(arena, path)));

        let id = self.mods.len();
        self.mods.push((path.into(), result));
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
