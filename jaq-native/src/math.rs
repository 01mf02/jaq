use alloc::string::String;
use jaq_core::{Error, Val};

/// Use a value as a float to be given as an argument to a libm
/// function.
pub fn as_float(v: Val, fun: &str) -> Result<f64, Error> {
    match v {
        Val::Int(n) => Ok(n as f64),
        Val::Float(n) => Ok(n),
        Val::Num(ref n) => n.parse().or(Err(Error::MathFn(String::from(fun), v))),
        _ => Err(Error::MathFn(String::from(fun), v)),
    }
}

/// Use a value as an integer to be given as an argument to a libm
/// function.
pub fn as_int(v: Val, fun: &str) -> Result<i32, Error> {
    match v {
        Val::Int(n) => Ok(n as i32),
        Val::Num(ref n) => n.parse().or(Err(Error::MathFn(String::from(fun), v))),
        _ => Err(Error::MathFn(String::from(fun), v)),
    }
}

/// Build a 0-ary filter from a 1-ary math function.
macro_rules! math_0_ary {
    ($f: ident, $domain: expr, $codomain: expr) => {
        (stringify!($f), 0, |_, cv| {
            box_once($domain(cv.1, stringify!($f)).map(libm::$f).map($codomain))
        })
    };
}

pub(crate) use math_0_ary;

/// Build a 2-ary filter that ignores '.' from a 2-ary math function.
macro_rules! math_2_ary {
    ($f: ident, $domain1: expr, $domain2: expr, $codomain: expr) => {
        (stringify!($f), 2, |args, cv| {
            let xs = args.get(0);
            let ys = args.get(1);
            Box::new(xs.cartesian(ys, cv).map(|(x, y)| {
                let x = $domain1(x.clone()?, stringify!($f))?;
                let y = $domain2(y.clone()?, stringify!($f))?;
                Ok($codomain(libm::$f(x, y)))
            }))
        })
    };
}

pub(crate) use math_2_ary;

/// Build a 3-ary filter that ignores '.' from a 3-ary math function.
macro_rules! math_3_ary {
    ($f: ident, $domain1: expr, $domain2: expr, $domain3: expr, $codomain: expr) => {
        (stringify!($f), 3, |args, cv| {
            use itertools::Itertools;
            let xs = args.get(0).run(cv.clone());
            let ys: Vec<_> = args.get(1).run(cv.clone()).collect();
            let zs: Vec<_> = args.get(2).run(cv).collect();
            Box::new(
                xs.into_iter()
                    .cartesian_product(ys)
                    .cartesian_product(zs)
                    .map(|((x, y), z)| {
                        let x = $domain1(x.clone()?, stringify!($f))?;
                        let y = $domain2(y.clone()?, stringify!($f))?;
                        let z = $domain3(z.clone()?, stringify!($f))?;
                        Ok($codomain(libm::$f(x, y, z)))
                    }),
            )
        })
    };
}

pub(crate) use math_3_ary;
