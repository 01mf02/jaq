use crate::filter::FilterT;
use crate::val::Val;
use std::collections::VecDeque;
use std::rc::Rc;

pub struct Recurse<F> {
    filter: F,
    input: VecDeque<Rc<Val>>,
    output: VecDeque<Rc<Val>>,
}

impl<F> Recurse<F> {
    pub fn new(filter: F, val: Rc<Val>) -> Self {
        let mut output = VecDeque::new();
        output.push_back(val);
        Recurse {
            filter,
            input: VecDeque::new(),
            output,
        }
    }
}

impl<F: FilterT> Iterator for Recurse<F> {
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
