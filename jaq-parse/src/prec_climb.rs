pub trait Op {
    fn prec(&self) -> usize;
    fn right_assoc(&self) -> bool;

    fn climb(&self, next: &Self) -> bool {
        next.prec() > self.prec() || (next.right_assoc() && next.prec() == self.prec())
    }
}

pub trait Output<O: Op>
where
    Self: Sized,
{
    fn from_op(lhs: Self, op: O, rhs: Self) -> Self;

    fn parse(self, iter: impl IntoIterator<Item = (O, Self)>) -> Self {
        let mut iter = iter.into_iter();
        self.parse1(&mut iter.next(), &mut iter, 0)
    }

    fn parse1<I>(mut self, next: &mut Option<(O, Self)>, iter: &mut I, min_prec: usize) -> Self
    where
        I: Iterator<Item = (O, Self)>,
    {
        while let Some((op, mut rhs)) = next.take() {
            if op.prec() < min_prec {
                *next = Some((op, rhs));
                return self;
            }

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
