macro_rules! math {
    // Build a 0-ary filter from a 1-ary math function.
    ($f: ident, $domain: expr, $codomain: expr) => {
        #[allow(clippy::redundant_closure_call)]
        (stringify!($f), v(0), |_, cv| {
            bome((|| Ok($codomain(libm::$f($domain(&cv.1)?))))())
        })
    };
    // Build a 2-ary filter that ignores '.' from a 2-ary math function.
    ($f: ident, $domain1: expr, $domain2: expr, $codomain: expr) => {
        (stringify!($f), v(2), |_, mut cv| {
            bome((|| {
                let y = cv.0.pop_var();
                let x = cv.0.pop_var();
                Ok($codomain(libm::$f($domain1(&x)?, $domain2(&y)?)))
            })())
        })
    };
    // Build a 3-ary filter that ignores '.' from a 3-ary math function.
    ($f: ident, $domain1: expr, $domain2: expr, $domain3: expr, $codomain: expr) => {
        (stringify!($f), v(3), |_, mut cv| {
            bome((|| {
                let z = cv.0.pop_var();
                let y = cv.0.pop_var();
                let x = cv.0.pop_var();
                Ok($codomain(libm::$f(
                    $domain1(&x)?,
                    $domain2(&y)?,
                    $domain3(&z)?,
                )))
            })())
        })
    };
}

pub(crate) use math;

/// Build a filter from float to float
macro_rules! f_f {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, V::from)
    };
}

/// Build a filter from float to int
macro_rules! f_i {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, |x| V::from(x as isize))
    };
}

/// Build a filter from float to (float, int)
macro_rules! f_fi {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, |(x, y)| [V::from(x), V::from(y as isize)]
            .into_iter()
            .collect())
    };
}

/// Build a filter from float to (float, float)
macro_rules! f_ff {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, |(x, y)| [V::from(x), V::from(y)]
            .into_iter()
            .collect())
    };
}

/// Build a filter from (float, float) to float
macro_rules! ff_f {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, V::as_f64, V::from)
    };
}

/// Build a filter from (int, float) to float
macro_rules! if_f {
    ($f: ident) => {
        crate::math::math!($f, V::try_as_i32, V::as_f64, V::from)
    };
}

/// Build a filter from (float, int) to float
macro_rules! fi_f {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, V::try_as_i32, V::from)
    };
}

/// Build a filter from (float, float, float) to float
macro_rules! fff_f {
    ($f: ident) => {
        crate::math::math!($f, V::as_f64, V::as_f64, V::as_f64, V::from)
    };
}

pub(crate) use f_f;
pub(crate) use f_ff;
pub(crate) use f_fi;
pub(crate) use f_i;
pub(crate) use ff_f;
pub(crate) use fff_f;
pub(crate) use fi_f;
pub(crate) use if_f;
