//! JSON values with reference-counted sharing.

use crate::Error;
use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::fmt;
use hifijson::{LexAlloc, Token};
use jaq_parse::MathOp;

/// JSON value with sharing.
///
/// The speciality of this type is that numbers are distinguished into
/// machine-sized integers and 64-bit floating-point numbers.
/// This allows using integers to index arrays,
/// while using floating-point numbers to do general math.
///
/// Operations on numbers follow a few principles:
/// * The sum, difference, product, and remainder of two integers is integer.
/// * Any other operation between two numbers yields a float.
#[derive(Clone, Debug)]
pub enum Val {
    /// Null
    Null,
    /// Boolean
    Bool(bool),
    /// Integer
    Int(isize),
    /// Floating-point number
    Float(f64),
    /// Floating-point number or integer not fitting into `Int`
    Num(Rc<String>),
    /// String
    Str(Rc<String>),
    /// Array
    Arr(Rc<Vec<Val>>),
    /// Object
    Obj(Rc<Map<Rc<String>, Val>>),
}

/// Order-preserving map
type Map<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;

/// A value result.
pub type ValR = Result<Val, Error>;

/// A stream of value results.
pub type ValRs<'a> = Box<dyn Iterator<Item = ValR> + 'a>;

// This might be included in the Rust standard library:
// <https://github.com/rust-lang/rust/issues/93610>
fn rc_unwrap_or_clone<T: Clone>(a: Rc<T>) -> T {
    Rc::try_unwrap(a).unwrap_or_else(|a| (*a).clone())
}

impl Val {
    /// Construct a string value.
    pub fn str(s: String) -> Self {
        Self::Str(s.into())
    }

    /// Construct an array value.
    pub fn arr(v: Vec<Val>) -> Self {
        Self::Arr(v.into())
    }

    /// Construct an object value.
    pub fn obj(m: Map<Rc<String>, Val>) -> Self {
        Self::Obj(m.into())
    }

    /// True if the value is neither null nor false.
    pub fn as_bool(&self) -> bool {
        !matches!(self, Val::Null | Val::Bool(false))
    }

    /// If the value is integer, return it, else fail.
    pub fn as_int(&self) -> Result<isize, Error> {
        match self {
            Self::Int(i) => Ok(*i),
            _ => Err(Error::Int(self.clone())),
        }
    }

    /// If the value is a string, return it, else fail.
    pub fn to_str(self) -> Result<Rc<String>, Error> {
        match self {
            Self::Str(s) => Ok(s),
            _ => Err(Error::Str(self)),
        }
    }

    /// If the value is a string, return it, else fail.
    fn as_str(&self) -> Result<&Rc<String>, Error> {
        match self {
            Self::Str(s) => Ok(s),
            _ => Err(Error::Str(self.clone())),
        }
    }

    /// If the value is an array, return it, else fail.
    fn to_arr(self) -> Result<Rc<Vec<Val>>, Error> {
        match self {
            Self::Arr(a) => Ok(a),
            _ => Err(Error::Arr(self)),
        }
    }

    /// If the value is an array, return it, else fail.
    fn as_arr(&self) -> Result<&Rc<Vec<Val>>, Error> {
        match self {
            Self::Arr(a) => Ok(a),
            _ => Err(Error::Arr(self.clone())),
        }
    }

    fn from_dec_str(n: &str) -> Self {
        n.parse().map_or(Self::Null, Self::Float)
    }

    /// If the value is an integer representing a valid Unicode codepoint, return it, else fail.
    fn as_codepoint(&self) -> Result<char, Error> {
        let i = self.as_int()?;
        // conversion from isize to u32 may fail on 64-bit systems for high values of c
        let u = u32::try_from(i).map_err(|_| Error::Char(i))?;
        char::from_u32(u).ok_or(Error::Char(i))
    }

    pub(crate) fn debug(self) -> Self {
        log::debug!("{}", self);
        self
    }

    /// Return 0 for null, the absolute value for numbers, and
    /// the length for strings, arrays, and objects.
    ///
    /// Fail on booleans.
    pub fn len(&self) -> Result<Self, Error> {
        match self {
            Self::Null => Ok(Self::Int(0)),
            Self::Bool(_) => Err(Error::Length(self.clone())),
            Self::Int(i) => Ok(Self::Int(i.abs())),
            Self::Num(n) => Self::from_dec_str(n).len(),
            Self::Float(f) => Ok(Self::Float(f.abs())),
            Self::Str(s) => Ok(Self::Int(s.chars().count() as isize)),
            Self::Arr(a) => Ok(Self::Int(a.len() as isize)),
            Self::Obj(o) => Ok(Self::Int(o.len() as isize)),
        }
    }

    /// Apply a rounding function to floating-point numbers, then convert them to integers.
    ///
    /// Return integers unchanged, and fail on any other input.
    pub fn round(&self, f: impl FnOnce(f64) -> f64) -> Result<Self, Error> {
        match self {
            Self::Int(_) => Ok(self.clone()),
            Self::Float(x) => Ok(Self::Int(f(*x) as isize)),
            Self::Num(n) => Self::from_dec_str(n).round(f),
            _ => Err(Error::Round(self.clone())),
        }
    }

    /// Return true if `value | .[key]` is defined.
    ///
    /// Fail on values that are neither null, arrays, nor objects.
    pub fn has(&self, key: &Self) -> Result<bool, Error> {
        match (self, key) {
            (Self::Null, _) => Ok(false),
            (Self::Arr(a), Self::Int(i)) if *i >= 0 => Ok((*i as usize) < a.len()),
            (Self::Obj(o), Self::Str(s)) => Ok(o.contains_key(&**s)),
            _ => Err(Error::Has(self.clone(), key.clone())),
        }
    }

    /// Return any `key` for which `value | .[key]` is defined.
    ///
    /// Fail on values that are neither arrays nor objects.
    pub fn keys(&self) -> Result<Vec<Val>, Error> {
        match self {
            Self::Arr(a) => Ok((0..a.len() as isize).map(Self::Int).collect()),
            Self::Obj(o) => Ok(o.keys().map(|k| Self::Str(Rc::clone(k))).collect()),
            _ => Err(Error::Keys(self.clone())),
        }
    }

    /// Return the elements of an array or the values of an object (omitting its keys).
    ///
    /// Fail on any other value.
    pub fn into_iter(self) -> Result<Box<dyn Iterator<Item = Val>>, Error> {
        match self {
            Self::Arr(a) => Ok(Box::new(rc_unwrap_or_clone(a).into_iter())),
            Self::Obj(o) => Ok(Box::new(rc_unwrap_or_clone(o).into_iter().map(|(_k, v)| v))),
            _ => Err(Error::Iter(self)),
        }
    }

    pub(crate) fn try_map<I: Iterator<Item = ValR>>(self, f: impl Fn(Val) -> I) -> ValR {
        Ok(match self {
            Self::Arr(a) => {
                let iter = rc_unwrap_or_clone(a).into_iter().flat_map(f);
                Val::arr(iter.collect::<Result<_, _>>()?)
            }
            Self::Obj(o) => {
                let iter = rc_unwrap_or_clone(o).into_iter();
                let iter = iter.filter_map(|(k, v)| f(v).next().map(|v| Ok((k, v?))));
                Val::obj(iter.collect::<Result<_, _>>()?)
            }
            v => v,
        })
    }

    /// `a` contains `b` iff either
    /// * the string `b` is a substring of `a`,
    /// * every element in the array `b` is contained in some element of the array `a`,
    /// * for every key-value pair `k, v` in `b`,
    ///   there is a key-value pair `k, v'` in `a` such that `v'` contains `v`, or
    /// * `a equals `b`.
    pub fn contains(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l), Self::Str(r)) => l.contains(&**r),
            (Self::Arr(l), Self::Arr(r)) => r.iter().all(|r| l.iter().any(|l| l.contains(r))),
            (Self::Obj(l), Self::Obj(r)) => r
                .iter()
                .all(|(k, r)| l.get(k).map(|l| l.contains(r)).unwrap_or(false)),
            _ => self == other,
        }
    }

    /// Convert string to a single JSON value.
    ///
    /// Fail on any other value.
    pub fn from_json(&self) -> ValR {
        let mut lexer = hifijson::SliceLexer::new(self.as_str()?.as_bytes());
        use hifijson::token::Lex;
        lexer
            .exactly_one(Self::parse)
            .map_err(|e| Error::FromJson(self.clone(), e.to_string()))
    }

    /// Convert a string into an array of its Unicode codepoints.
    pub fn explode(&self) -> Result<Vec<Val>, Error> {
        // conversion from u32 to isize may fail on 32-bit systems for high values of c
        let conv = |c: char| Val::Int(isize::try_from(c as u32).unwrap());
        Ok(self.as_str()?.chars().map(conv).collect())
    }

    /// Convert an array of Unicode codepoints into a string.
    pub fn implode(&self) -> Result<String, Error> {
        self.as_arr()?.iter().map(|v| v.as_codepoint()).collect()
    }

    /// Apply a function to a string.
    pub fn mutate_str(self, f: impl Fn(&mut String)) -> ValR {
        let mut s = self.to_str()?;
        f(Rc::make_mut(&mut s));
        Ok(Self::Str(s))
    }

    /// Apply a function to an array.
    pub fn mutate_arr(self, f: impl Fn(&mut Vec<Val>)) -> ValR {
        let mut a = self.to_arr()?;
        f(Rc::make_mut(&mut a));
        Ok(Self::Arr(a))
    }

    /// Apply the function to the value if there is no error,
    /// set error if the function application yielded an error.
    ///
    /// This is useful if we have to run a function in a context
    /// where we cannot fail immediately.
    fn run_if_ok<'a>(self, err: &mut Option<Error>, f: &impl Fn(Val) -> ValRs<'a>) -> Vec<Val> {
        if err.is_some() {
            return Vec::new();
        };
        match f(self).collect() {
            Ok(y) => y,
            Err(e) => {
                *err = Some(e);
                Vec::new()
            }
        }
    }

    /// Sort array by the given function.
    ///
    /// Fail on any other value.
    pub fn sort_by<'a>(self, f: impl Fn(Val) -> ValRs<'a>) -> ValR {
        let mut a = self.to_arr()?;
        // Some(e) iff an error has previously occurred
        let mut err = None;
        Rc::make_mut(&mut a).sort_by_cached_key(|x| x.clone().run_if_ok(&mut err, &f));
        err.map_or(Ok(Val::Arr(a)), Err)
    }

    /// Group an array by the given function.
    ///
    /// Fail on any other value.
    pub fn group_by<'a>(self, f: impl Fn(Val) -> ValRs<'a>) -> ValR {
        let mut err = None;
        let mut yx = rc_unwrap_or_clone(self.to_arr()?)
            .into_iter()
            .map(|x| (x.clone().run_if_ok(&mut err, &f), x))
            .collect::<Vec<(Vec<Val>, Val)>>();
        if let Some(err) = err {
            return Err(err);
        }

        yx.sort_by(|(y1, _), (y2, _)| y1.cmp(y2));

        use itertools::Itertools;
        let grouped = yx
            .into_iter()
            .group_by(|(y, _)| y.clone())
            .into_iter()
            .map(|(_y, yxs)| Val::arr(yxs.map(|(_y, x)| x).collect()))
            .collect();
        Ok(Val::arr(grouped))
    }

    /// Split a string by a given separator string.
    ///
    /// Fail if any of the two given values is not a string.
    pub fn split(&self, sep: &Self) -> Result<Vec<Val>, Error> {
        let s = self.as_str()?;
        let sep = sep.as_str()?;
        Ok(if sep.is_empty() {
            // Rust's `split` function with an empty separator ("")
            // yields an empty string as first and last result
            // to prevent this, we are using `chars` instead
            s.chars().map(|s| Self::str(s.to_string())).collect()
        } else {
            s.split(&**sep).map(|s| Self::str(s.to_string())).collect()
        })
    }

    /// Apply a regular expression to the given input value.
    ///
    /// `sm` indicates whether to
    /// 1. output strings that do *not* match the regex, and
    /// 2. output the matches.
    pub fn regex(&self, re: &Self, flags: &Self, sm: (bool, bool)) -> Result<Vec<Val>, Error> {
        use crate::regex::{ByteChar, Flags, Match};

        let s = self.as_str()?;
        let flags = Flags::new(flags.as_str()?).map_err(Error::RegexFlag)?;
        let re = flags
            .regex(re.as_str()?)
            .map_err(|e| Error::Regex(e.to_string()))?;
        let (split, matches) = sm;

        let mut last_byte = 0;
        let mut bc = ByteChar::new(s);
        let mut out = Vec::new();

        for c in re.captures_iter(s) {
            let whole = c.get(0).unwrap();
            if whole.start() >= s.len() || (flags.ignore_empty() && whole.as_str().is_empty()) {
                continue;
            }
            let vs = c
                .iter()
                .zip(re.capture_names())
                .filter_map(|(match_, name)| Some(Match::new(&mut bc, match_?, name)))
                .map(Val::from);
            if split {
                out.push(Val::str(s[last_byte..whole.start()].to_string()));
                last_byte = whole.end();
            }
            if matches {
                out.push(Val::arr(vs.collect()));
            }
            if !flags.global() {
                break;
            }
        }
        if split {
            out.push(Val::str(s[last_byte..].to_string()));
        }
        Ok(out)
    }

    /// Parse at least one JSON value, given an initial token and a lexer.
    ///
    /// If the underlying lexer reads input fallibly (for example `IterLexer`),
    /// the error returned by this function might be misleading.
    /// In that case, always check whether the lexer contains an error.
    pub fn parse(token: Token, lexer: &mut impl LexAlloc) -> Result<Self, hifijson::Error> {
        use hifijson::{token, Error};
        match token {
            Token::Null => Ok(Val::Null),
            Token::True => Ok(Val::Bool(true)),
            Token::False => Ok(Val::Bool(false)),
            Token::DigitOrMinus => {
                let (num, parts) = lexer.num_string()?;
                // if we are dealing with an integer ...
                if parts.dot.is_none() && parts.exp.is_none() {
                    // ... that fits into an isize
                    if let Ok(i) = num.parse() {
                        return Ok(Self::Int(i));
                    }
                }
                Ok(Val::Num(Rc::new(num.to_string())))
            }
            Token::Quote => Ok(Val::str(lexer.str_string()?.to_string())),
            Token::LSquare => Ok(Val::arr({
                let mut arr = Vec::new();
                lexer.seq(Token::RSquare, |token, lexer| {
                    arr.push(Self::parse(token, lexer)?);
                    Ok::<_, hifijson::Error>(())
                })?;
                arr
            })),
            Token::LCurly => Ok(Val::obj({
                let mut obj: Map<_, _> = Default::default();
                lexer.seq(Token::RCurly, |token, lexer| {
                    let key =
                        lexer.str_colon(token, |lexer| lexer.str_string().map_err(Error::Str))?;

                    let token = lexer.ws_token().ok_or(token::Expect::Value)?;
                    let value = Self::parse(token, lexer)?;
                    obj.insert(Rc::new(key.to_string()), value);
                    Ok::<_, Error>(())
                })?;
                obj
            })),
            _ => Err(token::Expect::Value)?,
        }
    }
}

#[cfg(feature = "serde_json")]
impl From<serde_json::Value> for Val {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match v {
            Null => Self::Null,
            Bool(b) => Self::Bool(b),
            Number(n) => n
                .to_string()
                .parse()
                .map_or_else(|_| Self::Num(Rc::new(n.to_string())), Self::Int),
            String(s) => Self::str(s),
            Array(a) => Self::arr(a.into_iter().map(|x| x.into()).collect()),
            Object(o) => Self::obj(o.into_iter().map(|(k, v)| (Rc::new(k), v.into())).collect()),
        }
    }
}

#[cfg(feature = "serde_json")]
impl From<Val> for serde_json::Value {
    fn from(v: Val) -> serde_json::Value {
        use core::str::FromStr;
        use serde_json::Value::*;
        match v {
            Val::Null => Null,
            Val::Bool(b) => Bool(b),
            Val::Int(i) => Number(i.into()),
            Val::Float(f) => serde_json::Number::from_f64(f).map_or(Null, Number),
            Val::Num(n) => Number(serde_json::Number::from_str(&n).unwrap()),
            Val::Str(s) => String((*s).clone()),
            Val::Arr(a) => Array(a.iter().map(|x| x.clone().into()).collect()),
            Val::Obj(o) => Object(
                o.iter()
                    .map(|(k, v)| ((**k).clone(), v.clone().into()))
                    .collect(),
            ),
        }
    }
}

impl From<crate::regex::Match> for Val {
    fn from(m: crate::regex::Match) -> Self {
        let obj = [
            ("offset", Val::Int(m.offset as isize)),
            ("length", Val::Int(m.length as isize)),
            ("string", Val::str(m.string)),
            ("name", m.name.map(Val::str).unwrap_or(Val::Null)),
        ];
        let obj = obj.into_iter().filter(|(_, v)| *v != Val::Null);
        Val::obj(obj.map(|(k, v)| (Rc::new(k.to_string()), v)).collect())
    }
}

impl core::ops::Add for Val {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Ok(x),
            (Int(x), Int(y)) => Ok(Int(x + y)),
            (Int(i), Float(f)) | (Float(f), Int(i)) => Ok(Float(f + i as f64)),
            (Float(x), Float(y)) => Ok(Float(x + y)),
            (Num(n), r) => Self::from_dec_str(&n) + r,
            (l, Num(n)) => l + Self::from_dec_str(&n),
            (Str(mut l), Str(r)) => {
                Rc::make_mut(&mut l).push_str(&r);
                Ok(Str(l))
            }
            (Arr(mut l), Arr(r)) => {
                //std::dbg!(Rc::strong_count(&l));
                Rc::make_mut(&mut l).extend(r.iter().cloned());
                Ok(Arr(l))
            }
            (Obj(mut l), Obj(r)) => {
                Rc::make_mut(&mut l).extend(r.iter().map(|(k, v)| (k.clone(), v.clone())));
                Ok(Obj(l))
            }
            (l, r) => Err(Error::MathOp(l, MathOp::Add, r)),
        }
    }
}

impl core::ops::Sub for Val {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x - y)),
            (Float(f), Int(i)) => Ok(Float(f - i as f64)),
            (Int(i), Float(f)) => Ok(Float(i as f64 - f)),
            (Float(x), Float(y)) => Ok(Float(x - y)),
            (Num(n), r) => Self::from_dec_str(&n) - r,
            (l, Num(n)) => l - Self::from_dec_str(&n),
            (l, r) => Err(Error::MathOp(l, MathOp::Sub, r)),
        }
    }
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x * y)),
            (Float(f), Int(i)) | (Int(i), Float(f)) => Ok(Float(f * i as f64)),
            (Float(x), Float(y)) => Ok(Float(x * y)),
            (Num(n), r) => Self::from_dec_str(&n) * r,
            (l, Num(n)) => l * Self::from_dec_str(&n),
            (l, r) => Err(Error::MathOp(l, MathOp::Mul, r)),
        }
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Float(x as f64 / y as f64)),
            (Float(f), Int(i)) => Ok(Float(f / i as f64)),
            (Int(i), Float(f)) => Ok(Float(i as f64 / f)),
            (Float(x), Float(y)) => Ok(Float(x / y)),
            (Num(n), r) => Self::from_dec_str(&n) / r,
            (l, Num(n)) => l / Self::from_dec_str(&n),
            (l, r) => Err(Error::MathOp(l, MathOp::Div, r)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) if y != 0 => Ok(Int(x % y)),
            (l, r) => Err(Error::MathOp(l, MathOp::Rem, r)),
        }
    }
}

impl core::ops::Neg for Val {
    type Output = ValR;
    fn neg(self) -> Self::Output {
        use Val::*;
        match self {
            Int(x) => Ok(Int(-x)),
            Float(x) => Ok(Float(-x)),
            Num(n) => -Self::from_dec_str(&n),
            x => Err(Error::Neg(x)),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Int(i), Self::Float(f)) | (Self::Float(f), Self::Int(i)) => {
                float_eq(&(*i as f64), f)
            }
            (Self::Float(x), Self::Float(y)) => float_eq(x, y),
            (Self::Num(x), Self::Num(y)) if Rc::ptr_eq(x, y) => true,
            (Self::Num(n), y) => &Self::from_dec_str(n) == y,
            (x, Self::Num(n)) => x == &Self::from_dec_str(n),
            (Self::Str(x), Self::Str(y)) => x == y,
            (Self::Arr(x), Self::Arr(y)) => x == y,
            (Self::Obj(x), Self::Obj(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for Val {}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Val {
    fn cmp(&self, other: &Self) -> Ordering {
        use Ordering::*;
        match (self, other) {
            (Self::Null, Self::Null) => Equal,
            (Self::Bool(x), Self::Bool(y)) => x.cmp(y),
            (Self::Int(x), Self::Int(y)) => x.cmp(y),
            (Self::Int(i), Self::Float(f)) => float_cmp(&(*i as f64), f),
            (Self::Float(f), Self::Int(i)) => float_cmp(f, &(*i as f64)),
            (Self::Float(x), Self::Float(y)) => float_cmp(x, y),
            (Self::Num(x), Self::Num(y)) if Rc::ptr_eq(x, y) => Equal,
            (Self::Num(n), y) => Self::from_dec_str(n).cmp(y),
            (x, Self::Num(n)) => x.cmp(&Self::from_dec_str(n)),
            (Self::Str(x), Self::Str(y)) => x.cmp(y),
            (Self::Arr(x), Self::Arr(y)) => x.cmp(y),
            (Self::Obj(x), Self::Obj(y)) => match (x.len(), y.len()) {
                (0, 0) => Equal,
                (0, _) => Less,
                (_, 0) => Greater,
                _ => {
                    let mut l: Vec<_> = x.iter().collect();
                    let mut r: Vec<_> = y.iter().collect();
                    l.sort_by_key(|(k, _v)| *k);
                    r.sort_by_key(|(k, _v)| *k);
                    // TODO: make this nicer
                    let kl = l.iter().map(|(k, _v)| k);
                    let kr = r.iter().map(|(k, _v)| k);
                    let vl = l.iter().map(|(_k, v)| v);
                    let vr = r.iter().map(|(_k, v)| v);
                    kl.cmp(kr).then_with(|| vl.cmp(vr))
                }
            },

            // nulls are smaller than anything else
            (Self::Null, _) => Less,
            (_, Self::Null) => Greater,
            // bools are smaller than anything else, except for nulls
            (Self::Bool(_), _) => Less,
            (_, Self::Bool(_)) => Greater,
            // numbers are smaller than anything else, except for nulls and bools
            (Self::Int(_) | Self::Float(_), _) => Less,
            (_, Self::Int(_) | Self::Float(_)) => Greater,
            // etc.
            (Self::Str(_), _) => Less,
            (_, Self::Str(_)) => Greater,
            (Self::Arr(_), _) => Less,
            (_, Self::Arr(_)) => Greater,
        }
    }
}

fn float_eq(left: &f64, right: &f64) -> bool {
    float_cmp(left, right) == Ordering::Equal
}

fn float_cmp(left: &f64, right: &f64) -> Ordering {
    if *left == 0. && *right == 0. {
        Ordering::Equal
    } else {
        f64::total_cmp(left, right)
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Null => "null".fmt(f),
            Self::Bool(b) => b.fmt(f),
            Self::Int(i) => i.fmt(f),
            Self::Float(x) if x.is_finite() => write!(f, "{:?}", x),
            Self::Float(_) => "null".fmt(f),
            Self::Num(n) => n.fmt(f),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Arr(a) => {
                write!(f, "[")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    first.fmt(f)?
                };
                iter.try_for_each(|x| write!(f, ",{}", x))?;
                write!(f, "]")
            }
            Self::Obj(o) => {
                write!(f, "{{")?;
                let mut iter = o.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "\"{}\":{}", k, v)?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",\"{}\":{}", k, v))?;
                write!(f, "}}")
            }
        }
    }
}
