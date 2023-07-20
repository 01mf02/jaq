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
    ($name: expr, $f: expr, $domain: expr, $codomain: expr) => {
        ($name, 0, |_, cv| {
            box_once($domain(cv.1, $name).map($f).map($codomain))
        })
    };
}

pub(crate) use math_0_ary;

/// Build a 2-ary filter that ignores '.' from a 2-ary math function.
macro_rules! math_2_ary {
    ($name: expr, $f: expr, $domain1: expr, $domain2: expr, $codomain: expr) => {
        ($name, 2, |args, cv| {
            let xs = args.get(0);
            let ys = args.get(1);
            Box::new(xs.cartesian(ys, cv).map(|(x, y)| {
                let x = $domain1(x.clone()?, $name)?;
                let y = $domain2(y.clone()?, $name)?;
                Ok($codomain($f(x, y)))
            }))
        })
    };
}

pub(crate) use math_2_ary;

/// Build a 3-ary filter that ignores '.' from a 3-ary math function.
macro_rules! math_3_ary {
    ($name: expr, $f: expr, $domain1: expr, $domain2: expr, $domain3: expr, $codomain: expr) => {
        ($name, 3, |args, cv| {
            use itertools::Itertools;
            let xs = args.get(0).run(cv.clone());
            let ys: Vec<_> = args.get(1).run(cv.clone()).collect();
            let zs: Vec<_> = args.get(2).run(cv).collect();
            Box::new(
                xs.into_iter()
                    .cartesian_product(ys)
                    .cartesian_product(zs)
                    .map(|((x, y), z)| {
                        let x = $domain1(x.clone()?, $name)?;
                        let y = $domain2(y.clone()?, $name)?;
                        let z = $domain3(z.clone()?, $name)?;
                        Ok($codomain($f(x, y, z)))
                    }),
            )
        })
    };
}

pub(crate) use math_3_ary;

/// Source: https://github.com/itchyny/gojq/blob/main/func.go#L1374
pub fn drem(l: f64, r: f64) -> f64 {
    let x = libm::remainder(l, r);
    if x == 0.0 {
        libm::copysign(x, l)
    } else {
        x
    }
}

/// Source: https://github.com/golang/go/blob/master/src/math/logb.go
pub fn logb(x: f64) -> f64 {
    if x == 0.0 {
        f64::NEG_INFINITY
    } else if x.is_infinite() {
        f64::INFINITY
    } else if x.is_nan() {
        x
    } else {
        libm::ilogb(x) as f64
    }
}

pub fn pow10(x: f64) -> f64 {
    libm::pow(10.0, x)
}

/// Source: https://github.com/jqlang/jq/blob/master/src/builtin.c#L132
pub fn significand(x: f64) -> f64 {
    libm::scalbn(x, -libm::ilogb(x))
}
