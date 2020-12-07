use crate::val::{Val, Vals};
use std::rc::Rc;

#[derive(Debug)]
pub enum Function {
    Empty,
    Not,
    All,
    Any,
    Add,
    Length,
}

impl Function {
    pub fn run(&self, v: Rc<Val>) -> Vals {
        use Function::*;
        match self {
            Empty => Box::new(core::iter::empty()),
            Any => Val::Bool(v.iter().unwrap().any(|v| v.as_bool())).into(),
            All => Val::Bool(v.iter().unwrap().all(|v| v.as_bool())).into(),
            Not => Val::Bool(!v.as_bool()).into(),
            Add => {
                let iter = v.iter().unwrap().map(|x| (*x).clone());
                iter.fold(Val::Null, |acc, x| (acc + x).unwrap()).into()
            }
            Length => Val::Num(v.len().unwrap()).into(),
        }
    }
}
