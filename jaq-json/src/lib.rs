//! JSON values with reference-counted sharing.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

mod num;

use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use jaq_core::box_iter::{box_once, BoxIter};
use jaq_core::{load, ops, path, val, DataT, Exn, Native, RunPtr};
use jaq_std::{run, unary, v, Filter};
pub use num::Num;
use num_bigint::BigInt;
use num_traits::{cast::ToPrimitive, Signed};

#[cfg(feature = "hifijson")]
use hifijson::{LexAlloc, Token};

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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Val {
    #[default]
    /// Null
    Null,
    /// Boolean
    Bool(bool),
    /// Number
    Num(Num),
    /// String
    Str(Rc<String>),
    /// Array
    Arr(Rc<Vec<Val>>),
    /// Object
    Obj(Rc<Map<Val, Val>>),
}

/// Types and sets of types.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    /// `[] | .["a"]` or `limit("a"; 0)` or `range(0; "a")`
    Int,
    /// `"1" | sin` or `pow(2; "3")` or `fma(2; 3; "4")`
    Float,
    /// `-"a"`, `"a" | round`
    Num,
    /// `{(0): 1}` or `0 | fromjson` or `0 | explode` or `"a b c" | split(0)`
    Str,
    /// `0 | sort` or `0 | implode` or `[] | .[0:] = 0`
    Arr,
    /// `0 | .[]` or `0 | .[0]` or `0 | keys` (array or object)
    Iter,
    /// `{}[0:1]` (string or array)
    Range,
}

impl Type {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Int => "integer",
            Self::Float => "floating-point number",
            Self::Num => "number",
            Self::Str => "string",
            Self::Arr => "array",
            Self::Iter => "iterable (array or object)",
            Self::Range => "rangeable (array or string)",
        }
    }
}

/// Order-preserving map
type Map<K = Val, V = K> = indexmap::IndexMap<K, V, foldhash::fast::RandomState>;

/// Error that can occur during filter execution.
pub type Error = jaq_core::Error<Val>;
/// A value or an eRror.
pub type ValR = jaq_core::ValR<Val>;
/// A value or an eXception.
pub type ValX = jaq_core::ValX<Val>;

// This is part of the Rust standard library since 1.76:
// <https://doc.rust-lang.org/std/rc/struct.Rc.html#method.unwrap_or_clone>.
// However, to keep MSRV low, we reimplement it here.
fn rc_unwrap_or_clone<T: Clone>(a: Rc<T>) -> T {
    Rc::try_unwrap(a).unwrap_or_else(|a| (*a).clone())
}

impl jaq_core::ValT for Val {
    fn from_num(n: &str) -> ValR {
        Ok(Self::Num(Num::from_str(n)))
    }

    fn from_map<I: IntoIterator<Item = (Self, Self)>>(iter: I) -> ValR {
        Ok(Self::obj(iter.into_iter().collect()))
    }

    fn key_values(self) -> Box<dyn Iterator<Item = Result<(Val, Val), Error>>> {
        let arr_idx = |(i, x)| Ok((Self::from(i as isize), x));
        match self {
            Self::Arr(a) => Box::new(rc_unwrap_or_clone(a).into_iter().enumerate().map(arr_idx)),
            Self::Obj(o) => Box::new(rc_unwrap_or_clone(o).into_iter().map(Ok)),
            _ => box_once(Err(Error::typ(self, Type::Iter.as_str()))),
        }
    }

    fn values(self) -> Box<dyn Iterator<Item = ValR>> {
        match self {
            Self::Arr(a) => Box::new(rc_unwrap_or_clone(a).into_iter().map(Ok)),
            Self::Obj(o) => Box::new(rc_unwrap_or_clone(o).into_iter().map(|(_k, v)| Ok(v))),
            _ => box_once(Err(Error::typ(self, Type::Iter.as_str()))),
        }
    }

    fn index(self, index: &Self) -> ValR {
        match (self, index) {
            (Val::Arr(a), Val::Num(Num::Int(i))) => {
                Ok(abs_index(*i, a.len()).map_or(Val::Null, |i| a[i].clone()))
            }
            (a @ Val::Arr(_), Val::Num(Num::BigInt(i))) => {
                a.index(&Val::Num(Num::Int(bigint_to_int_saturated(i))))
            }
            (Val::Obj(o), i) => Ok(o.get(i).cloned().unwrap_or(Val::Null)),
            (s @ Val::Arr(_), _) => Err(Error::index(s, index.clone())),
            (s, _) => Err(Error::typ(s, Type::Iter.as_str())),
        }
    }

    fn range(self, range: jaq_core::val::Range<&Self>) -> ValR {
        let (from, upto) = (range.start, range.end);
        match self {
            Val::Arr(a) => {
                let len = a.len();
                let from = from.as_ref().map(|i| i.as_isize()).transpose();
                let upto = upto.as_ref().map(|i| i.as_isize()).transpose();
                from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
                    let from = abs_bound(from, len, 0);
                    let upto = abs_bound(upto, len, len);
                    let (skip, take) = skip_take(from, upto);
                    a.iter().skip(skip).take(take).cloned().collect()
                })
            }
            Val::Str(s) => {
                let len = s.chars().count();
                let from = from.as_ref().map(|i| i.as_isize()).transpose();
                let upto = upto.as_ref().map(|i| i.as_isize()).transpose();
                from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
                    let from = abs_bound(from, len, 0);
                    let upto = abs_bound(upto, len, len);
                    let (skip, take) = skip_take(from, upto);
                    Val::from(s.chars().skip(skip).take(take).collect::<String>())
                })
            }
            _ => Err(Error::typ(self, Type::Range.as_str())),
        }
    }

    fn map_values<I: Iterator<Item = ValX>>(self, opt: path::Opt, f: impl Fn(Self) -> I) -> ValX {
        match self {
            Self::Arr(a) => {
                let iter = rc_unwrap_or_clone(a).into_iter().flat_map(f);
                Ok(iter.collect::<Result<_, _>>()?)
            }
            Self::Obj(o) => {
                let iter = rc_unwrap_or_clone(o).into_iter();
                let iter = iter.filter_map(|(k, v)| f(v).next().map(|v| Ok((k, v?))));
                Ok(Self::obj(iter.collect::<Result<_, Exn<_>>>()?))
            }
            v => opt.fail(v, |v| Exn::from(Error::typ(v, Type::Iter.as_str()))),
        }
    }

    fn map_index<I: Iterator<Item = ValX>>(
        mut self,
        index: &Self,
        opt: path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX {
        match self {
            Val::Obj(ref mut o) => {
                use indexmap::map::Entry::{Occupied, Vacant};
                let o = Rc::make_mut(o);
                match o.entry(index.clone()) {
                    Occupied(mut e) => {
                        let v = core::mem::take(e.get_mut());
                        match f(v).next().transpose()? {
                            Some(y) => e.insert(y),
                            // this runs in constant time, at the price of
                            // changing the order of the elements
                            None => e.swap_remove(),
                        };
                    }
                    Vacant(e) => {
                        if let Some(y) = f(Val::Null).next().transpose()? {
                            e.insert(y);
                        }
                    }
                }
                Ok(self)
            }
            Val::Arr(ref mut a) => {
                let a = Rc::make_mut(a);
                let abs_or = |i| {
                    abs_index(i, a.len()).ok_or(Error::str(format_args!("index {i} out of bounds")))
                };
                let i = match index.as_isize().and_then(abs_or) {
                    Ok(i) => i,
                    Err(e) => return opt.fail(self, |_| Exn::from(e)),
                };

                let x = core::mem::take(&mut a[i]);
                if let Some(y) = f(x).next().transpose()? {
                    a[i] = y;
                } else {
                    a.remove(i);
                }
                Ok(self)
            }
            _ => opt.fail(self, |v| Exn::from(Error::typ(v, Type::Iter.as_str()))),
        }
    }

    fn map_range<I: Iterator<Item = ValX>>(
        mut self,
        range: jaq_core::val::Range<&Self>,
        opt: path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX {
        if let Val::Arr(ref mut a) = self {
            let a = Rc::make_mut(a);
            let from = range.start.as_ref().map(|i| i.as_isize()).transpose();
            let upto = range.end.as_ref().map(|i| i.as_isize()).transpose();
            let (from, upto) = match from.and_then(|from| Ok((from, upto?))) {
                Ok(from_upto) => from_upto,
                Err(e) => return opt.fail(self, |_| Exn::from(e)),
            };
            let len = a.len();
            let from = abs_bound(from, len, 0);
            let upto = abs_bound(upto, len, len);
            let (skip, take) = skip_take(from, upto);
            let arr = a.iter().skip(skip).take(take).cloned().collect();
            let y = f(arr).map(|y| y?.into_arr().map_err(Exn::from)).next();
            let y = y.transpose()?.unwrap_or_default();
            a.splice(skip..skip + take, (*y).clone());
            Ok(self)
        } else {
            opt.fail(self, |v| Exn::from(Error::typ(v, Type::Arr.as_str())))
        }
    }

    /// True if the value is neither null nor false.
    fn as_bool(&self) -> bool {
        !matches!(self, Self::Null | Self::Bool(false))
    }

    /// If the value is a string, return it, else fail.
    fn as_str(&self) -> Option<&str> {
        if let Self::Str(s) = self {
            Some(s)
        } else {
            None
        }
    }
}

impl jaq_std::ValT for Val {
    fn into_seq<S: FromIterator<Self>>(self) -> Result<S, Self> {
        match self {
            Self::Arr(a) => match Rc::try_unwrap(a) {
                Ok(a) => Ok(a.into_iter().collect()),
                Err(a) => Ok(a.iter().cloned().collect()),
            },
            _ => Err(self),
        }
    }

    fn is_int(&self) -> bool {
        self.as_num().map_or(false, Num::is_int)
    }

    fn as_isize(&self) -> Option<isize> {
        self.as_num().and_then(Num::as_isize)
    }

    fn as_f64(&self) -> Result<f64, Error> {
        let fail = || Error::typ(self.clone(), Type::Float.as_str());
        self.as_num().and_then(Num::as_f64).ok_or_else(fail)
    }
}

/// Definitions of the standard library.
pub fn defs() -> impl Iterator<Item = load::parse::Def<&'static str>> {
    load::parse(include_str!("defs.jq"), |p| p.defs())
        .unwrap()
        .into_iter()
}

impl Val {
    /// Return 0 for null, the absolute value for numbers, and
    /// the length for strings, arrays, and objects.
    ///
    /// Fail on booleans.
    fn length(&self) -> ValR {
        match self {
            Val::Null => Ok(Val::from(0)),
            Val::Bool(_) => Err(Error::str(format_args!("{self} has no length"))),
            Val::Num(n) => Ok(Val::Num(n.length())),
            Val::Str(s) => Ok(Val::from(s.chars().count() as isize)),
            Val::Arr(a) => Ok(Val::from(a.len() as isize)),
            Val::Obj(o) => Ok(Val::from(o.len() as isize)),
        }
    }

    /// Return the indices of `y` in `self`.
    fn indices<'a>(&'a self, y: &'a Val) -> Result<Box<dyn Iterator<Item = usize> + 'a>, Error> {
        match (self, y) {
            (Val::Str(_), Val::Str(y)) if y.is_empty() => Ok(Box::new(core::iter::empty())),
            (Val::Arr(_), Val::Arr(y)) if y.is_empty() => Ok(Box::new(core::iter::empty())),
            (Val::Str(x), Val::Str(y)) => {
                let iw = str_windows(x, y.chars().count()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == **y).then_some(i))))
            }
            (Val::Arr(x), Val::Arr(y)) => {
                let iw = x.windows(y.len()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == **y).then_some(i))))
            }
            (Val::Arr(x), y) => {
                let ix = x.iter().enumerate();
                Ok(Box::new(ix.filter_map(move |(i, x)| (x == y).then_some(i))))
            }
            (x, y) => Err(Error::index(x.clone(), y.clone())),
        }
    }
}

/// Return the string windows having `n` characters, where `n` > 0.
///
/// Taken from <https://users.rust-lang.org/t/iterator-over-windows-of-chars/17841/3>.
fn str_windows(line: &str, n: usize) -> impl Iterator<Item = &str> {
    line.char_indices()
        .zip(line.char_indices().skip(n).chain(Some((line.len(), ' '))))
        .map(move |((i, _), (j, _))| &line[i..j])
}

/// Functions of the standard library.
#[cfg(feature = "parse")]
pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Filter<Native<D>>> {
    base_funs().chain([run(parse_fun())])
}

/// Minimal set of filters for JSON values.
pub fn base_funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Filter<Native<D>>> {
    base().into_vec().into_iter().map(run)
}

fn box_once_err<'a>(r: ValR) -> BoxIter<'a, ValX> {
    box_once(r.map_err(Exn::from))
}

fn base<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        ("tojson", v(0), |cv| box_once(Ok(cv.1.to_string().into()))),
        ("length", v(0), |cv| box_once_err(cv.1.length())),
        ("contains", v(1), |cv| {
            unary(cv, |x, y| Ok(Val::from(x.contains(&y))))
        }),
        ("has", v(1), |cv| unary(cv, |v, k| v.has(&k).map(Val::from))),
        ("indices", v(1), |cv| {
            let to_int = |i: usize| Val::from(i as isize);
            unary(cv, move |x, v| {
                x.indices(&v).map(|idxs| idxs.map(to_int).collect())
            })
        }),
        ("bsearch", v(1), |cv| {
            let to_idx = |r: Result<_, _>| r.map_or_else(|i| -1 - i as isize, |i| i as isize);
            unary(cv, move |a, x| {
                a.as_arr().map(|a| Val::from(to_idx(a.binary_search(&x))))
            })
        }),
    ])
}

#[cfg(feature = "parse")]
/// Convert string to a single JSON value.
fn from_json(s: &str) -> ValR {
    use hifijson::token::Lex;
    let mut lexer = hifijson::SliceLexer::new(s.as_bytes());
    lexer
        .exactly_one(Val::parse)
        .map_err(|e| Error::str(format_args!("cannot parse {s} as JSON: {e}")))
}

#[cfg(feature = "parse")]
fn parse_fun<D: for<'a> DataT<V<'a> = Val>>() -> Filter<RunPtr<D>> {
    ("fromjson", v(0), |cv| {
        use jaq_core::ValT;
        let fail = || Error::typ(cv.1.clone(), Type::Str.as_str());
        box_once_err(cv.1.as_str().ok_or_else(fail).and_then(from_json))
    })
}

fn skip_take(from: usize, until: usize) -> (usize, usize) {
    (from, until.saturating_sub(from))
}

/// If a range bound is given, absolutise and clip it between 0 and `len`,
/// else return `default`.
fn abs_bound(i: Option<isize>, len: usize, default: usize) -> usize {
    i.map_or(default, |i| core::cmp::min(wrap(i, len).unwrap_or(0), len))
}

/// Absolutise an index and return result if it is inside [0, len).
fn abs_index(i: isize, len: usize) -> Option<usize> {
    wrap(i, len).filter(|i| *i < len)
}

fn wrap(i: isize, len: usize) -> Option<usize> {
    if i >= 0 {
        Some(i as usize)
    } else if len < -i as usize {
        None
    } else {
        Some(len - (-i as usize))
    }
}

#[test]
fn wrap_test() {
    let len = 4;
    assert_eq!(wrap(0, len), Some(0));
    assert_eq!(wrap(8, len), Some(8));
    assert_eq!(wrap(-1, len), Some(3));
    assert_eq!(wrap(-4, len), Some(0));
    assert_eq!(wrap(-8, len), None);
}

impl Val {
    /// Construct an object value.
    pub fn obj(m: Map) -> Self {
        Self::Obj(m.into())
    }

    fn as_num(&self) -> Option<&Num> {
        match self {
            Self::Num(n) => Some(n),
            _ => None,
        }
    }

    /// If the value is a machine-sized integer, return it, else fail.
    fn as_isize(&self) -> Result<isize, Error> {
        let fail = || Error::typ(self.clone(), Type::Int.as_str());
        self.as_num().and_then(Num::as_isize).ok_or_else(fail)
    }

    /// If the value is an array, return it, else fail.
    fn into_arr(self) -> Result<Rc<Vec<Self>>, Error> {
        match self {
            Self::Arr(a) => Ok(a),
            _ => Err(Error::typ(self, Type::Arr.as_str())),
        }
    }

    fn as_arr(&self) -> Result<&Rc<Vec<Self>>, Error> {
        match self {
            Self::Arr(a) => Ok(a),
            _ => Err(Error::typ(self.clone(), Type::Arr.as_str())),
        }
    }

    /// Return true if `value | .[key]` is defined.
    ///
    /// Fail on values that are neither arrays nor objects.
    fn has(&self, key: &Self) -> Result<bool, Error> {
        match (self, key) {
            (Self::Arr(a), Self::Num(Num::Int(i))) if *i >= 0 => Ok((*i as usize) < a.len()),
            (Self::Arr(a), Self::Num(Num::BigInt(i))) if !i.is_negative() => {
                Ok(i.to_usize().map_or(false, |i| i < a.len()))
            }
            (Self::Obj(o), k) => Ok(o.contains_key(k)),
            _ => Err(Error::index(self.clone(), key.clone())),
        }
    }

    /// `a` contains `b` iff either
    /// * the string `b` is a substring of `a`,
    /// * every element in the array `b` is contained in some element of the array `a`,
    /// * for every key-value pair `k, v` in `b`,
    ///   there is a key-value pair `k, v'` in `a` such that `v'` contains `v`, or
    /// * `a` equals `b`.
    fn contains(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l), Self::Str(r)) => l.contains(&**r),
            (Self::Arr(l), Self::Arr(r)) => r.iter().all(|r| l.iter().any(|l| l.contains(r))),
            (Self::Obj(l), Self::Obj(r)) => r
                .iter()
                .all(|(k, r)| l.get(k).map_or(false, |l| l.contains(r))),
            _ => self == other,
        }
    }

    /// Parse at least one JSON value, given an initial token and a lexer.
    ///
    /// If the underlying lexer reads input fallibly (for example `IterLexer`),
    /// the error returned by this function might be misleading.
    /// In that case, always check whether the lexer contains an error.
    #[cfg(feature = "hifijson")]
    pub fn parse(token: Token, lexer: &mut impl LexAlloc) -> Result<Self, hifijson::Error> {
        use hifijson::{token, Error};
        match token {
            Token::Null => Ok(Self::Null),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            Token::DigitOrMinus => Ok(Self::Num({
                let (num, parts) = lexer.num_string()?;
                // if we are dealing with an integer ...
                if parts.dot.is_none() && parts.exp.is_none() {
                    Num::try_from_int_str(&num).unwrap()
                } else {
                    Num::Dec(Rc::new(num.to_string()))
                }
            })),
            Token::Quote => Ok(Self::from(lexer.str_string()?.to_string())),
            Token::LSquare => Ok(Self::Arr({
                let mut arr = Vec::new();
                lexer.seq(Token::RSquare, |token, lexer| {
                    arr.push(Self::parse(token, lexer)?);
                    Ok::<_, hifijson::Error>(())
                })?;
                arr.into()
            })),
            Token::LCurly => Ok(Self::obj({
                let mut obj = Map::default();
                lexer.seq(Token::RCurly, |token, lexer| {
                    let key =
                        lexer.str_colon(token, |lexer| lexer.str_string().map_err(Error::Str))?;

                    let token = lexer.ws_token().ok_or(token::Expect::Value)?;
                    let value = Self::parse(token, lexer)?;
                    obj.insert(Val::from(key.to_string()), value);
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
            Number(n) => Self::Num(Num::from_str(&n.to_string())),
            String(s) => Self::from(s),
            Array(a) => a.into_iter().map(Self::from).collect(),
            Object(o) => Self::obj(o.into_iter().map(|(k, v)| (Rc::new(k), v.into())).collect()),
        }
    }
}

#[cfg(feature = "serde_json")]
impl From<Val> for serde_json::Value {
    fn from(v: Val) -> Self {
        use core::str::FromStr;
        use serde_json::Value::*;
        match v {
            Val::Null => Null,
            Val::Bool(b) => Bool(b),
            /*
            Val::Int(i) => Number(i.into()),
            Val::BigInt(i) => Number(serde_json::Number::from_str(&i.to_string()).unwrap()),
            Val::Float(f) => serde_json::Number::from_f64(f).map_or(Null, Number),
            */
            Val::Num(n) => Number(serde_json::Number::from_str(&n.to_string()).unwrap()),
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

impl From<bool> for Val {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<isize> for Val {
    fn from(i: isize) -> Self {
        Self::Num(Num::Int(i))
    }
}

impl From<f64> for Val {
    fn from(f: f64) -> Self {
        Self::Num(Num::Float(f))
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Self::Str(Rc::new(s))
    }
}

impl From<val::Range<Val>> for Val {
    fn from(r: val::Range<Val>) -> Self {
        let kv = |(k, v): (&str, Option<_>)| v.map(|v| (k.to_string().into(), v));
        let kvs = [("start", r.start), ("end", r.end)];
        Val::obj(kvs.into_iter().flat_map(kv).collect())
    }
}

impl FromIterator<Self> for Val {
    fn from_iter<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        Self::Arr(Rc::new(iter.into_iter().collect()))
    }
}

fn bigint_to_int_saturated(i: &BigInt) -> isize {
    let (min, max) = (isize::MIN, isize::MAX);
    i.to_isize()
        .unwrap_or_else(|| if i.is_negative() { min } else { max })
}

impl core::ops::Add for Val {
    type Output = ValR;
    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            // `null` is a neutral element for addition
            (Null, x) | (x, Null) => Ok(x),
            (Num(x), Num(y)) => Ok(Num(x + y)),
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
            (l, r) => Err(Error::math(l, ops::Math::Add, r)),
        }
    }
}

impl core::ops::Sub for Val {
    type Output = ValR;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(x), Self::Num(y)) => Ok(Self::Num(x - y)),
            (Self::Arr(mut l), Self::Arr(r)) => {
                let r = r.iter().collect::<alloc::collections::BTreeSet<_>>();
                Rc::make_mut(&mut l).retain(|x| !r.contains(x));
                Ok(Self::Arr(l))
            }
            (l, r) => Err(Error::math(l, ops::Math::Sub, r)),
        }
    }
}

fn obj_merge(l: &mut Rc<Map>, r: Rc<Map>) {
    let l = Rc::make_mut(l);
    let r = rc_unwrap_or_clone(r).into_iter();
    r.for_each(|(k, v)| match (l.get_mut(&k), v) {
        (Some(Val::Obj(l)), Val::Obj(r)) => obj_merge(l, r),
        (Some(l), r) => *l = r,
        (None, r) => {
            l.insert(k, r);
        }
    });
}

impl core::ops::Mul for Val {
    type Output = ValR;
    fn mul(self, rhs: Self) -> Self::Output {
        use crate::Num::{BigInt, Int};
        use Val::*;
        match (self, rhs) {
            (Num(x), Num(y)) => Ok(Num(x * y)),
            (s @ Str(_), Num(BigInt(i))) | (Num(BigInt(i)), s @ Str(_)) => {
                s * Num(Int(bigint_to_int_saturated(&i)))
            }
            (Str(s), Num(Int(i))) | (Num(Int(i)), Str(s)) if i > 0 => {
                Ok(Self::from(s.repeat(i as usize)))
            }
            // string multiplication with negatives or 0 results in null
            // <https://jqlang.github.io/jq/manual/#Builtinoperatorsandfunctions>
            (Str(_), Num(Int(_))) | (Num(Int(_)), Str(_)) => Ok(Null),
            (Obj(mut l), Obj(r)) => {
                obj_merge(&mut l, r);
                Ok(Obj(l))
            }
            (l, r) => Err(Error::math(l, ops::Math::Mul, r)),
        }
    }
}

/// Split a string by a given separator string.
fn split<'a>(s: &'a str, sep: &'a str) -> Box<dyn Iterator<Item = String> + 'a> {
    if s.is_empty() {
        Box::new(core::iter::empty())
    } else if sep.is_empty() {
        // Rust's `split` function with an empty separator ("")
        // yields an empty string as first and last result
        // to prevent this, we are using `chars` instead
        Box::new(s.chars().map(|s| s.to_string()))
    } else {
        Box::new(s.split(sep).map(|s| s.to_string()))
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        use Val::{Num, Str};
        match (self, rhs) {
            (Num(x), Num(y)) => Ok(Num(x / y)),
            (Str(x), Str(y)) => Ok(split(&x, &y).map(Val::from).collect()),
            (l, r) => Err(Error::math(l, ops::Math::Div, r)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(x), Self::Num(y)) if !(x.is_int() && y.is_int() && y == Num::Int(0)) => {
                Ok(Self::Num(x % y))
            }
            (l, r) => Err(Error::math(l, ops::Math::Rem, r)),
        }
    }
}

impl core::ops::Neg for Val {
    type Output = ValR;
    fn neg(self) -> Self::Output {
        match self {
            Self::Num(n) => Ok(Self::Num(-n)),
            x => Err(Error::typ(x, Type::Num.as_str())),
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Val {
    fn cmp(&self, other: &Self) -> Ordering {
        use Ordering::{Equal, Greater, Less};
        match (self, other) {
            (Self::Null, Self::Null) => Equal,
            (Self::Bool(x), Self::Bool(y)) => x.cmp(y),
            (Self::Num(x), Self::Num(y)) => x.cmp(y),
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
            (Self::Num(_), _) => Less,
            (_, Self::Num(_)) => Greater,
            // etc.
            (Self::Str(_), _) => Less,
            (_, Self::Str(_)) => Greater,
            (Self::Arr(_), _) => Less,
            (_, Self::Arr(_)) => Greater,
        }
    }
}

impl Hash for Val {
    fn hash<H: Hasher>(&self, state: &mut H) {
        fn hash_with(u: u8, x: impl Hash, state: &mut impl Hasher) {
            state.write_u8(u);
            x.hash(state)
        }
        match self {
            Self::Num(n) => n.hash(state),
            // Num::hash() starts its hash with a 0 or 1, so we start with 2 here
            Self::Null => state.write_u8(2),
            Self::Bool(b) => state.write_u8(if *b { 3 } else { 4 }),
            Self::Str(s) => hash_with(5, s, state),
            Self::Arr(a) => hash_with(6, a, state),
            Self::Obj(o) => {
                state.write_u8(7);
                // this is similar to what happens in `Val::cmp`
                let mut kvs: Vec<_> = o.iter().collect();
                kvs.sort_by_key(|(k, _v)| *k);
                kvs.iter().for_each(|(k, v)| (k, v).hash(state));
            }
        }
    }
}

/// Format a string as valid JSON string, including leading and trailing quotes.
pub fn fmt_str(f: &mut fmt::Formatter, s: &str) -> fmt::Result {
    write!(f, "\"")?;
    for s in s.split_inclusive(|c| c < ' ' || c == '\\' || c == '"') {
        // split s into last character and everything before (init)
        let mut chars = s.chars();
        let last = chars.next_back();
        let init = chars.as_str();

        match last {
            Some(last @ ('\t' | '\n' | '\r' | '\\' | '"')) => {
                write!(f, "{init}{}", last.escape_default())
            }
            Some(last) if last < ' ' => write!(f, "{init}\\u{:04x}", last as u8),
            _ => write!(f, "{s}"),
        }?;
    }
    write!(f, "\"")
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Str(s) => fmt_str(f, s),
            Self::Arr(a) => {
                write!(f, "[")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{first}")?;
                };
                iter.try_for_each(|x| write!(f, ",{x}"))?;
                write!(f, "]")
            }
            Self::Obj(o) => {
                write!(f, "{{")?;
                let mut iter = o.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{k}:{v}")?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",{k}:{v}"))?;
                write!(f, "}}")
            }
        }
    }
}
