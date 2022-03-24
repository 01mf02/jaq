#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug)]
pub enum Opt {
    Optional,
    Essential,
}

impl Opt {
    /// If the value is optional, return `x`, else fail with `f(x)`.
    pub fn fail<T, E>(self, x: T, f: impl FnOnce(T) -> E) -> Result<T, E> {
        match self {
            Self::Optional => Ok(x),
            Self::Essential => Err(f(x)),
        }
    }

    pub fn collect<T, E>(self, iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
        match self {
            Self::Optional => Ok(iter.filter_map(|x| x.ok()).collect()),
            Self::Essential => iter.collect(),
        }
    }
}
