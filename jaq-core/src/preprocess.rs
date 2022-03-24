use crate::functions::Builtin;
use crate::Filter;
use alloc::{boxed::Box, string::String, vec::Vec};
use core::convert::{TryFrom, TryInto};
use core::fmt::{self, Display};

pub type PreFilter = Filter<Call>;
pub type OpenFilter = Filter<Open>;
pub type ClosedFilter = Filter<Closed>;

#[derive(Debug)]
pub struct UndefinedError(String, usize);

impl Display for UndefinedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}/{} is not defined", self.0, self.1)
    }
}

pub struct Call {
    name: String,
    args: Vec<PreFilter>,
}

impl Call {
    pub fn new(name: String, args: Vec<PreFilter>) -> Self {
        Self { name, args }
    }
}

#[derive(Clone, Debug)]
pub enum Open {
    C(Builtin<Box<OpenFilter>>),
    V(usize),
}

#[derive(Debug)]
pub struct Closed(Builtin<Box<ClosedFilter>>);

impl PreFilter {
    pub fn open<F>(self, args: &[String], fns: &F) -> Result<OpenFilter, UndefinedError>
    where
        F: Fn(&(String, usize)) -> Option<OpenFilter>,
    {
        self.try_map(&|call| call.open(args, fns))
    }
}

impl Call {
    fn open<F>(self, args: &[String], fns: &F) -> Result<OpenFilter, UndefinedError>
    where
        F: Fn(&(String, usize)) -> Option<OpenFilter>,
    {
        let name = self.name;
        match args.iter().position(|arg| *arg == name) {
            Some(pos) if self.args.is_empty() => Ok(Filter::Named(Open::V(pos))),
            _ => {
                let fun = (name, self.args.len());
                let fun = fns(&fun).ok_or(UndefinedError(fun.0, fun.1))?;
                let cargs: Result<Vec<_>, _> =
                    self.args.into_iter().map(|a| a.open(args, fns)).collect();
                let cargs = cargs?;
                Ok(fun.try_map(&|cv| cv.subst(&cargs)).unwrap())
            }
        }
    }
}

impl Open {
    fn subst(self, args: &[OpenFilter]) -> Result<OpenFilter, ()> {
        use Open::*;
        match self {
            C(builtin) => Ok(Filter::Named(C(builtin.try_map(&|cv| cv.subst(args))?))),
            V(v) => Ok(args[v].clone()),
        }
    }
}

impl TryFrom<OpenFilter> for ClosedFilter {
    type Error = ();
    fn try_from(f: OpenFilter) -> Result<Self, Self::Error> {
        f.try_map(&|open| Ok(Filter::Named(open.try_into()?)))
    }
}

impl TryFrom<Open> for Closed {
    type Error = ();
    fn try_from(open: Open) -> Result<Self, Self::Error> {
        use Filter::Named;
        use Open::*;
        match open {
            C(builtin) => Ok(Self(builtin.try_map(&|open| Ok(Named(open.try_into()?)))?)),
            V(_v) => Err(()),
        }
    }
}

impl core::ops::Deref for Closed {
    type Target = Builtin<Box<ClosedFilter>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Builtin<usize>> for OpenFilter {
    fn from(f: Builtin<usize>) -> Self {
        Filter::Named(Open::C(f.map(&|i| Box::new(Filter::Named(Open::V(i))))))
    }
}
