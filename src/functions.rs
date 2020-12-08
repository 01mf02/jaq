use crate::filter::Filter;
use crate::val::{Val, Vals};
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Debug)]
pub enum Function {
    Empty,
    Not,
    All,
    Any,
    Add,
    Length,
    Map(Box<Filter>),
    Select(Box<Filter>),
    Recurse(Box<Filter>),
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
            Map(f) => {
                let iter = v.iter().unwrap().flat_map(move |x| f.run(x));
                Box::new(iter.collect::<Vec<_>>().into_iter())
            }
            Select(f) => {
                let iter = f.run(Rc::clone(&v)).flat_map(|y| {
                    if y.as_bool() {
                        Box::new(core::iter::once(Rc::clone(&v)))
                    } else {
                        Box::new(core::iter::empty()) as Box<dyn Iterator<Item = _>>
                    }
                });
                Box::new(iter.collect::<Vec<_>>().into_iter())
            }
            Recurse(f) => Box::new(crate::functions::Recurse::new(f, v)),
        }
    }
}

struct Recurse<'f> {
    filter: &'f Filter,
    input: VecDeque<Rc<Val>>,
    output: VecDeque<Rc<Val>>,
}

impl<'f> Recurse<'f> {
    pub fn new(filter: &'f Filter, val: Rc<Val>) -> Self {
        let mut output = VecDeque::new();
        output.push_back(val);
        Recurse {
            filter,
            input: VecDeque::new(),
            output,
        }
    }
}

impl<'f> Iterator for Recurse<'f> {
    type Item = Rc<Val>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.output.pop_front() {
            Some(o) => {
                self.input.push_back(Rc::clone(&o));
                Some(o)
            }
            None => match self.input.pop_front() {
                None => None,
                Some(i) => {
                    self.output = self.filter.run(i).collect();
                    self.next()
                }
            },
        }
    }
}
