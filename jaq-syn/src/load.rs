//! Combined file loading, lexing, and parsing for multiple modules.

use crate::lex::{self, Token};
use crate::parse::{self, Def, Defs, Term};
use alloc::string::String;
use alloc::vec::Vec;

#[cfg(feature = "std")]
extern crate std;

/// Storage for loaded modules.
///
/// Once Rust has [internal references](https://smallcultfollowing.com/babysteps/blog/2024/06/02/the-borrow-checker-within/#step-4-internal-references),
/// this should become unnecessary.
/// I can't wait for it to happen!
#[derive(Default)]
pub struct Arena(typed_arena::Arena<String>);

/// Combined file loader, lexer, and parser for multiple modules.
pub struct Loader<S, R> {
    mods: Vec<(File<S>, Result<Module<S, Defs<S>>, Error<S>>)>,
    /// function to read module file contents from a path
    read: R,
    /// currently processed modules
    ///
    /// This is used to detect circular dependencies between modules.
    open: Vec<S>,
}

/// Path and contents of a (module) file, both represented by `S`.
///
/// This is useful for creating precise error messages.
#[derive(Clone, Debug, Default)]
pub struct File<S> {
    /// path of the file
    pub path: S,
    /// contents of the file
    pub code: S,
}

impl<S> File<S> {
    /// Apply a function to both path and contents of file.
    ///
    /// This is useful to go from a reference `&str` to an owned `String`,
    /// in order to save the `File` without its corresponding [`Arena`].
    pub fn map<S2>(self, f: impl Fn(S) -> S2) -> File<S2> {
        File {
            path: f(self.path),
            code: f(self.code),
        }
    }
}

/// Error occurring during loading of a single module.
#[derive(Debug)]
pub enum Error<S> {
    /// input/output errors, for example when trying to load a module that does not exist
    Io(Vec<(S, String)>),
    /// lex   errors, for example when loading a module `($) (`
    Lex(Vec<lex::Error<S>>),
    /// parse errors, for example when loading a module `(+) *`
    Parse(Vec<parse::Error<S>>),
}

/// Module containing strings `S` and a body `B`.
#[derive(Default)]
pub struct Module<S, B> {
    /// metadata (optional)
    pub meta: Option<Term<S>>,
    /// included and imported modules
    ///
    /// Suppose that we have [`Modules`] `mods` and the current [`Module`] is `mods[id]`.
    /// Then for every `(id_, name)` in `mods[id].1.mods`, we have that
    /// the included/imported module is stored in `mods[id_]` (`id_ < id`), and
    /// the module is included if `name` is `None` and imported if `name` is `Some(name)`.
    pub mods: Vec<(usize, Option<S>)>,
    /// imported variables, storing path and name (always starts with `$`)
    pub vars: Vec<(S, S)>,
    /// everything that comes after metadata and includes/imports
    pub body: B,
}

// TODO: Modules::imported_vars() -> Iter<&S>
pub type Modules<S> = Vec<(File<S>, Module<S, Defs<S>>)>;
/// Errors occurring during loading of multiple modules.
///
/// For example, suppose that we have
/// a file `l.jq` that yields a lex error,
/// a file `p.jq` that yields a parse error, and
/// a file `i.jq` that includes a non-existing module.
/// If we then include all these files in our main program,
/// [`Errors`] will contain each file with a different [`Error`].
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
    /// Initialise the loader with prelude definitions.
    ///
    /// The prelude is a special module that is implicitly included by all other modules
    /// (including the main module).
    /// That means that all filters defined in the prelude can be called from any module.
    ///
    /// The prelude is normally initialised with filters like `map` or `true`.
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
            read: |_path| Err("module loading not supported".into()),
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

/// Apply function to path of every imported data file, accumulating errors.
pub fn import<S: Copy>(
    mods: &Modules<S>,
    mut f: impl FnMut(S) -> Result<(), String>,
) -> Result<(), Errors<S>> {
    let mut errs = Vec::new();
    let mut vals = Vec::new();
    for (file, module) in mods {
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
        Ok(())
    } else {
        Err(errs)
    }
}

impl<S, R> Loader<S, R> {
    /// Provide a function to return the contents of included/imported module files.
    ///
    /// For every included/imported module, the loader will call this function to
    /// obtain the contents of the module.
    /// For example, if we have `include "foo"`, the loader calls `read("foo")`.
    pub fn with_read<R2>(self, read: R2) -> Loader<S, R2> {
        let Self { mods, open, .. } = self;
        Loader { mods, read, open }
    }

    /// Read the contents of included/imported module files by performing file I/O.
    #[cfg(feature = "std")]
    pub fn with_std_read(self) -> Loader<S, fn(&str) -> Result<String, String>> {
        self.with_read(std_read)
    }
}

impl<'s, R: FnMut(&str) -> Result<String, String>> Loader<&'s str, R> {
    /// Load a set of modules, starting from a given file.
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
