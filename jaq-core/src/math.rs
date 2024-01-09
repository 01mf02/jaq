use jaq_interpret::{Error, Val};

/// Use a value as an i32 to be given as an argument to a libm
/// function.
pub fn as_i32(v: &Val) -> Result<i32, Error> {
    v.as_int()?.try_into().map_err(Error::str)
}

/// Build a 0-ary filter from a 1-ary math function.
macro_rules! math_0_ary {
    ($name: expr, $f: ident, $domain: expr, $codomain: expr) => {
        ($name, 0, |_, cv| {
            once_with(move || $domain(&cv.1).map(libm::$f).map($codomain))
        })
    };
}

pub(crate) use math_0_ary;

/// Build a filter from float to float
macro_rules! f_f {
    ($name: expr, $f: ident) => {
        crate::math::math_0_ary!($name, $f, Val::as_float, Val::Float)
    };
    ($f: ident) => {
        crate::math::f_f!(stringify!($f), $f)
    };
}

pub(crate) use f_f;

/// Build a filter from float to int
macro_rules! f_i {
    ($name: expr, $f: ident) => {
        crate::math::math_0_ary!($name, $f, Val::as_float, |x| Val::Int(x as isize))
    };
    ($f: ident) => {
        crate::math::f_i!(stringify!($f), $f)
    };
}

pub(crate) use f_i;

/// Build a filter from float to (float, int)
macro_rules! f_fi {
    ($name: expr, $f: ident) => {
        crate::math::math_0_ary!($name, $f, Val::as_float, |(x, y)| Val::arr(Vec::from([
            Val::Float(x),
            Val::Int(y as isize)
        ])))
    };
    ($f: ident) => {
        crate::math::f_fi!(stringify!($f), $f)
    };
}

pub(crate) use f_fi;

/// Build a filter from float to (float, float)
macro_rules! f_ff {
    ($name: expr, $f: ident) => {
        crate::math::math_0_ary!($name, $f, Val::as_float, |(x, y)| Val::arr(Vec::from([
            Val::Float(x),
            Val::Float(y)
        ])))
    };
    ($f: ident) => {
        crate::math::f_ff!(stringify!($f), $f)
    };
}

pub(crate) use f_ff;

/// Build a 2-ary filter that ignores '.' from a 2-ary math function.
macro_rules! math_2_ary {
    ($name: expr, $f: ident, $domain1: expr, $domain2: expr, $codomain: expr) => {
        ($name, 2, |args, cv| {
            let xs = args.get(0);
            let ys = args.get(1);
            Box::new(xs.cartesian(ys, cv).map(|(x, y)| {
                let x = $domain1(&x?)?;
                let y = $domain2(&y?)?;
                Ok($codomain(libm::$f(x, y)))
            }))
        })
    };
}

pub(crate) use math_2_ary;

/// Build a filter from (float, float) to float
macro_rules! ff_f {
    ($name: expr, $f: ident) => {
        crate::math::math_2_ary!($name, $f, Val::as_float, Val::as_float, Val::Float)
    };
    ($f: ident) => {
        crate::math::ff_f!(stringify!($f), $f)
    };
}

pub(crate) use ff_f;

/// Build a filter from (float, int) to float
macro_rules! fi_f {
    ($name: expr, $f: ident) => {
        crate::math::math_2_ary!($name, $f, Val::as_float, math::as_i32, Val::Float)
    };
    ($f: ident) => {
        crate::math::fi_f!(stringify!($f), $f)
    };
}

pub(crate) use fi_f;

/// Build a filter from (int, float) to float
macro_rules! if_f {
    ($name: expr, $f: ident) => {
        crate::math::math_2_ary!($name, $f, math::as_i32, Val::as_float, Val::Float)
    };
    ($f: ident) => {
        crate::math::if_f!(stringify!($f), $f)
    };
}

pub(crate) use if_f;

/// Build a 3-ary filter that ignores '.' from a 3-ary math function.
macro_rules! math_3_ary {
    ($name: expr, $f: ident, $domain1: expr, $domain2: expr, $domain3: expr, $codomain: expr) => {
        ($name, 3, |args, cv| {
            let (xs, ys, zs) = (args.get(0), args.get(1), args.get(2));
            Box::new(xs.cartesian3(ys, zs, cv).map(|(x, y, z)| {
                Ok($codomain(libm::$f(
                    $domain1(&x?)?,
                    $domain2(&y?)?,
                    $domain3(&z?)?,
                )))
            }))
        })
    };
}

pub(crate) use math_3_ary;

/// Build a filter from (float, float, float) to float
macro_rules! fff_f {
    ($name: expr, $f: ident) => {
        crate::math::math_3_ary!(
            $name,
            $f,
            Val::as_float,
            Val::as_float,
            Val::as_float,
            Val::Float
        )
    };
    ($f: ident) => {
        crate::math::fff_f!(stringify!($f), $f)
    };
}

pub(crate) use fff_f;
