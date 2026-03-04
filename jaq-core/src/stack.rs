use alloc::vec::Vec;
use core::ops::ControlFlow;

pub struct Stack<I, F>(Vec<I>, F);

impl<I, F> Stack<I, F> {
    pub fn new(v: Vec<I>, f: F) -> Self {
        Self(v, f)
    }
}

/// If `F` returns `Break(x)`, then `x` is returned
/// If `F` returns `Continue(iter)`, then the iterator is pushed onto the stack
impl<I: Iterator, F: Fn(I::Item) -> ControlFlow<I::Item, I>> Iterator for Stack<I, F> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        // uncomment this to verify that the stack does not grow
        //println!("stack size: {}", self.0.len());
        loop {
            let mut top = self.0.pop()?;
            if let Some(next) = top.next() {
                // try not to grow the stack with empty iterators left behind
                if top.size_hint() != (0, Some(0)) {
                    self.0.push(top);
                }
                match self.1(next) {
                    ControlFlow::Break(next) => return Some(next),
                    ControlFlow::Continue(iter) => self.0.push(iter),
                }
            }
        }
    }
}

/// Efficient if-then-else execution.
pub struct Ite<'a, C, PF, I, P, T> {
    stack: Vec<(C, I, T, core::slice::Iter<'a, (P, T)>)>,
    else_: T,
    pred: PF,
}

impl<'a, C: Clone, PF: Fn(P, C) -> I, I, P, T> Ite<'a, C, PF, I, P, T> {
    pub fn new(ctx: C, (if_, then): (P, T), elifs: &'a [(P, T)], else_: T, pred: PF) -> Self {
        Self {
            stack: Vec::from([(ctx.clone(), pred(if_, ctx), then, elifs.iter())]),
            else_,
            pred,
        }
    }
}

impl<'a, C: Clone, PF: Fn(P, C) -> I, I: Iterator<Item = Result<bool, E>>, P: Copy, T: Copy, E>
    Iterator for Ite<'a, C, PF, I, P, T>
{
    type Item = Result<(T, C), E>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (ctx, mut if_, then_, mut if_thens) = self.stack.pop()?;
            match if_.next() {
                None => continue,
                Some(Err(e)) => return Some(Err(e)),
                Some(Ok(true)) => {
                    if if_.size_hint() != (0, Some(0)) {
                        self.stack.push((ctx.clone(), if_, then_, if_thens));
                    }
                    return Some(Ok((then_, ctx)));
                }
                Some(Ok(false)) => match if_thens.next() {
                    Some((if_, then_)) => {
                        self.stack
                            .push((ctx.clone(), (self.pred)(*if_, ctx), *then_, if_thens))
                    }
                    None => return Some(Ok((self.else_, ctx))),
                },
            }
        }
    }
}
