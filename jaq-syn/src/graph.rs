use crate::lex::{self, Token};
use crate::parse::{self, Def, Defs, Term};
use alloc::string::String;
use alloc::vec::Vec;

#[cfg(feature = "std")]
extern crate std;

struct Arena(typed_arena::Arena<String>);
struct Loader<S, R> {
    mods: Vec<(File<S>, Result<Module<S, Defs<S>>, Error<S>>)>,
    read: R,
}

#[derive(Default)]
struct File<S> {
    path: S,
    code: S,
}

enum Error<S> {
    Io(Vec<(S, String)>),
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

type Modules<S> = Vec<(File<S>, Module<S, Defs<S>>)>;
type Errors<S> = Vec<(File<S>, Error<S>)>;

impl<'s, B> Module<&'s str, B> {
    fn map_mods(
        m: parse::Module<&'s str, B>,
        mut f: impl FnMut(&'s str) -> Result<usize, String>,
    ) -> Result<Self, Error<&'s str>> {
        let mut mods = Vec::new();
        let mut vars = Vec::new();
        let mut errs = Vec::new();
        for (path, as_) in m.deps {
            match as_ {
                Some(x) if x.starts_with('$') => vars.push((path, x)),
                as_ => match f(path) {
                    Ok(mid) => mods.push((mid, as_)),
                    Err(e) => errs.push((path, e)),
                },
            }
        }
        if errs.is_empty() {
            Ok(Module {
                meta: m.meta,
                mods,
                vars,
                body: m.body,
            })
        } else {
            Err(Error::Io(errs))
        }
    }
}

impl<S, B> Module<S, B> {
    fn map_body<B2>(self, f: impl FnOnce(B) -> B2) -> Module<S, B2> {
        Module {
            meta: self.meta,
            mods: self.mods,
            vars: self.vars,
            body: f(self.body),
        }
    }
}

impl<'s> Loader<&'s str, fn(&str) -> Result<String, String>> {
    fn new() -> Self {
        Self {
            // the first module is reserved for the prelude
            mods: Vec::from([(File::default(), Ok(Module::default()))]),
            read: |path| Err("module loading not supported".into()),
        }
    }
}

#[cfg(feature = "std")]
fn std_read(path: &str) -> Result<String, String> {
    use alloc::string::ToString;
    std::fs::read_to_string(path).map_err(|e| e.to_string())
}

impl<S, R> Loader<S, R> {
    fn with_read<R2>(self, read: R2) -> Loader<S, R2> {
        let mods = self.mods;
        Loader { mods, read }
    }

    #[cfg(feature = "std")]
    fn with_std_read(self) -> Loader<S, fn(&str) -> Result<String, String>> {
        self.with_read(std_read)
    }
}

impl<'s, R: Fn(&str) -> Result<String, String>> Loader<&'s str, R> {
    fn load(
        mut self,
        arena: &'s Arena,
        file: File<&'s str>,
    ) -> Result<Modules<&'s str>, Errors<&'s str>> {
        let result = parse_main(file.code)
            .and_then(|m| Module::map_mods(m, |path| self.find(arena, path)))
            .map(|m| m.map_body(|body| Defs::from([Def::new("main", Vec::new(), body)])));
        self.mods.push((file, result));

        let mut mods = Vec::new();
        let mut errs = Vec::new();
        for (file, result) in self.mods {
            match result {
                Ok(m) => mods.push((file, m)),
                Err(e) => errs.push((file, e)),
            }
        }
        if errs.is_empty() {
            Ok(mods)
        } else {
            Err(errs)
        }
    }

    fn find(&mut self, arena: &'s Arena, path: &'s str) -> Result<usize, String> {
        if let Some(id) = self.mods.iter().position(|(file, _)| path == file.path) {
            return Ok(id);
        };

        let code = &**arena.0.alloc((self.read)(path)?);
        let defs =
            parse_defs(code).and_then(|m| Module::map_mods(m, |path| self.find(arena, path)));

        let id = self.mods.len();
        self.mods.push((File { path, code }, defs));
        Ok(id)
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
