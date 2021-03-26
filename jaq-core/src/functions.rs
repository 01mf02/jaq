use crate::{Filter, Val, ValR};
use alloc::{boxed::Box, rc::Rc, string::ToString};

#[derive(Debug)]
pub enum NewFunc {
    Not,
    All,
    Any,
    Add,
    Length,
    Type,
    Map(Box<Filter>),
}

impl NewFunc {
    pub fn run(&self, v: Rc<Val>) -> ValR {
        use NewFunc::*;
        match self {
            Any => Ok(Val::Bool(v.iter()?.any(|v| v.as_bool()))),
            All => Ok(Val::Bool(v.iter()?.all(|v| v.as_bool()))),
            Not => Ok(Val::Bool(!v.as_bool())),
            Add => v
                .iter()?
                .map(|x| (*x).clone())
                .try_fold(Val::Null, |acc, x| acc + x),
            Length => Ok(Val::Num(v.len()?)),
            Type => Ok(Val::Str(v.typ().to_string())),
            Map(f) => Ok(Val::Arr(
                v.iter()?.flat_map(|x| f.run(x)).collect::<Result<_, _>>()?,
            )),
        }
    }
}
