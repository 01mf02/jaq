use crate::val::{Val, ValR};
use crate::Filter;
use alloc::collections::VecDeque;

pub struct Recurse<F> {
    filter: F,
    input: VecDeque<Val>,
    output: VecDeque<ValR>,
}

impl<F> Recurse<F> {
    pub fn new(filter: F, val: Val) -> Self {
        let mut output = VecDeque::new();
        output.push_back(Ok(val));
        Self {
            filter,
            input: VecDeque::new(),
            output,
        }
    }
}

impl<F: core::ops::Deref<Target = Filter>> Iterator for Recurse<&F> {
    type Item = ValR;

    fn next(&mut self) -> Option<Self::Item> {
        match self.output.pop_front() {
            Some(o) => {
                if let Ok(ref o) = o {
                    self.input.push_back(o.clone());
                };
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
