//! Native filters.

use crate::filter::{Args, Cv, Update};
use crate::val::ValRs;
use crate::Error;
use alloc::boxed::Box;

/// A filter which is implemented using function pointers.
#[derive(Clone)]
pub struct Native {
    pub run: RunPtr,
    pub update: UpdatePtr,
}

pub type RunPtr = for<'a> fn(Args<'a>, Cv<'a>) -> ValRs<'a>;
pub type UpdatePtr = for<'a> fn(Args<'a>, Cv<'a>, Box<dyn Update<'a> + 'a>) -> ValRs<'a>;

impl Native {
    /// Create a native filter from a run function.
    pub const fn new(run: RunPtr) -> Self {
        Self::with_update(run, |_, _, _| crate::results::box_once(Err(Error::PathExp)))
    }

    /// Create a native filter from a run function and an update function (used for `filter |= ...`).
    pub const fn with_update(run: RunPtr, update: UpdatePtr) -> Self {
        Self { run, update }
    }
}

impl core::fmt::Debug for Native {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Native").finish()
    }
}
