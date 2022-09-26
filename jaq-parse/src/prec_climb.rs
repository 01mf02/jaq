pub trait Op {
    fn prec(&self) -> usize;
    fn right_assoc(&self) -> bool;

    fn climb(&self, next: &Self) -> bool {
        next.prec() > self.prec() || (next.right_assoc() && next.prec() == self.prec())
    }
}

pub trait Output<A, O: Op>
where
    Self: Sized,
{
    fn from_atom(atom: A) -> Self;
    fn from_op(lhs: Self, op: O, rhs: Self) -> Self;

    fn parse(first: A, iter: impl IntoIterator<Item = (O, A)>) -> Self {
        let mut iter = iter.into_iter();
        Self::parse1(Self::from_atom(first), &mut iter.next(), &mut iter, 0)
    }

    fn parse1<I>(mut self, next: &mut Option<(O, A)>, iter: &mut I, min_prec: usize) -> Self
    where
        I: Iterator<Item = (O, A)>,
    {
        while let Some((op, rhs_atom)) = next.take() {
            if op.prec() < min_prec {
                *next = Some((op, rhs_atom));
                return self;
            }

            let mut rhs = Self::from_atom(rhs_atom);
            *next = iter.next();

            while let Some(peek) = next.take() {
                let prec = peek.0.prec();
                let climb = op.climb(&peek.0);
                *next = Some(peek);

                if !climb {
                    break;
                }
                rhs = Self::parse1(rhs, next, iter, prec);
            }

            self = Self::from_op(self, op, rhs);
        }

        self
    }
}
