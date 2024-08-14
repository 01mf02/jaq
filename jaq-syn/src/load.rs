use crate::lex::{self, Token};
use crate::parse::{self, Def, Defs, Term};
use alloc::string::String;
use alloc::vec::Vec;

#[cfg(feature = "std")]
extern crate std;

#[derive(Default)]
pub struct Arena(typed_arena::Arena<String>);

pub struct Loader<S, R> {
    mods: Vec<(File<S>, Result<Module<S, Defs<S>>, Error<S>>)>,
    read: R,
    /// currently processed modules
    ///
    /// This is used to detect circular dependencies between modules.
    open: Vec<S>,
}

#[derive(Clone, Debug, Default)]
pub struct File<S> {
    pub path: S,
    pub code: S,
}

impl<S> File<S> {
    pub fn map<S2>(self, f: impl Fn(S) -> S2) -> File<S2> {
        File {
            path: f(self.path),
            code: f(self.code),
        }
    }
}

#[derive(Debug)]
pub enum Error<S> {
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

// TODO: Modules::imported_vars() -> Iter<&S>
pub type Modules<S> = Vec<(File<S>, Module<S, Defs<S>>)>;
pub type Errors<S> = Vec<(File<S>, Error<S>)>;

impl<'s, B> Module<&'s str, B> {
    fn map_mods(
        m: parse::Module<&'s str, B>,
        mut f: impl FnMut(&'s str) -> Result<usize, String>,
    ) -> Result<Self, Error<&'s str>> {
        // the prelude module is included implicitly in every module (except itself)
        let mut mods = Vec::from([(0, None)]);
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
    pub fn new(prelude: impl IntoIterator<Item = Def<&'s str, Term<&'s str>>>) -> Self {
        let defs = [
            Def::new("!recurse", Vec::new(), Term::recurse("!recurse")),
            Def::new("!empty", Vec::new(), Term::empty()),
        ];

        let prelude: Module<&'s str, Defs<&'s str>> = Module {
            body: defs.into_iter().chain(prelude).collect(),
            ..Module::default()
        };

        Self {
            // the first module is reserved for the prelude
            mods: Vec::from([(File::default(), Ok(prelude))]),
            read: |path| Err("module loading not supported".into()),
            open: Vec::new(),
        }
    }
}

#[cfg(feature = "std")]
fn std_read(path: &str) -> Result<String, String> {
    use alloc::string::ToString;
    let mut path = std::path::Path::new(path).to_path_buf();
    path.set_extension("jq");
    std::fs::read_to_string(path).map_err(|e| e.to_string())
}

pub fn map_imports<S: Copy, V, F>(modules: &Modules<S>, f: F) -> Result<Vec<V>, Errors<S>>
where
    F: Fn(S) -> Result<V, String>,
{
    let mut errs = Vec::new();
    let mut vals = Vec::new();
    for (file, module) in modules {
        let mut mod_errs = Vec::new();
        for (file, _name) in &module.vars {
            match f(*file) {
                Ok(v) => vals.push(v),
                Err(e) => mod_errs.push((*file, e)),
            }
        }
        if !mod_errs.is_empty() {
            errs.push((file.clone(), Error::Io(mod_errs)));
        }
    }
    if errs.is_empty() {
        Ok(vals)
    } else {
        Err(errs)
    }
}

impl<S, R> Loader<S, R> {
    pub fn with_read<R2>(self, read: R2) -> Loader<S, R2> {
        let Self { mods, open, .. } = self;
        Loader { mods, read, open }
    }

    #[cfg(feature = "std")]
    pub fn with_std_read(self) -> Loader<S, fn(&str) -> Result<String, String>> {
        self.with_read(std_read)
    }
}

impl<'s, R: Fn(&str) -> Result<String, String>> Loader<&'s str, R> {
    pub fn load(
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
        if self.open.contains(&path) {
            return Err("circular include/import detected".into());
        }

        let code = &**arena.0.alloc((self.read)(path)?);
        self.open.push(path);
        let defs =
            parse_defs(code).and_then(|m| Module::map_mods(m, |path| self.find(arena, path)));
        assert_eq!(self.open.pop(), Some(path));

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
