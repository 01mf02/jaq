use crate::{ClosedFilter, RValR, Val};
use alloc::collections::VecDeque;
use alloc::rc::Rc;

pub struct Recurse<F> {
    filter: F,
    input: VecDeque<Rc<Val>>,
    output: VecDeque<RValR>,
}

impl<F> Recurse<F> {
    pub fn new(filter: F, val: Rc<Val>) -> Self {
        let mut output = VecDeque::new();
        output.push_back(Ok(val));
        Self {
            filter,
            input: VecDeque::new(),
            output,
        }
    }
}

impl<F: core::ops::Deref<Target = ClosedFilter>> Iterator for Recurse<&F> {
    type Item = RValR;

    fn next(&mut self) -> Option<Self::Item> {
        match self.output.pop_front() {
            Some(o) => {
                if let Ok(ref o) = o {
                    self.input.push_back(Rc::clone(&o));
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
