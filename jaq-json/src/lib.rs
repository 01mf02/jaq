//! JSON values with reference-counted sharing.
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

use alloc::string::{String, ToString};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use jaq_core::box_iter::{box_once, BoxIter};
use jaq_core::{load, ops, path, Exn, Native, RunPtr};
use jaq_std::{run, unary, v, Filter};

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
#[derive(Clone, Debug, Default)]
pub enum Val {
    #[default]
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
type Map<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;

/// Error that can occur during filter execution.
pub type Error = jaq_core::Error<Val>;
/// A value or an eRror.
pub type ValR = jaq_core::ValR<Val>;
/// A value or an eXception.
pub type ValX<'a> = jaq_core::ValX<'a, Val>;

// This is part of the Rust standard library since 1.76:
// <https://doc.rust-lang.org/std/rc/struct.Rc.html#method.unwrap_or_clone>.
// However, to keep MSRV low, we reimplement it here.
fn rc_unwrap_or_clone<T: Clone>(a: Rc<T>) -> T {
    Rc::try_unwrap(a).unwrap_or_else(|a| (*a).clone())
}

impl jaq_core::ValT for Val {
    fn from_num(n: &str) -> ValR {
        Ok(Val::Num(Rc::new(n.to_string())))
    }

    fn from_map<I: IntoIterator<Item = (Self, Self)>>(iter: I) -> ValR {
        let iter = iter.into_iter().map(|(k, v)| Ok((k.into_str()?, v)));
        Ok(Self::obj(iter.collect::<Result<_, _>>()?))
    }

    fn values(self) -> Box<dyn Iterator<Item = ValR>> {
        match self {
            Self::Arr(a) => Box::new(rc_unwrap_or_clone(a).into_iter().map(Ok)),
            Self::Obj(o) => Box::new(rc_unwrap_or_clone(o).into_iter().map(|(_k, v)| Ok(v))),
            _ => Box::new(core::iter::once(Err(Error::typ(self, Type::Iter.as_str())))),
        }
    }

    fn index(self, index: &Self) -> ValR {
        match (self, index) {
            (Val::Arr(a), Val::Int(i)) => {
                Ok(abs_index(*i, a.len()).map_or(Val::Null, |i| a[i].clone()))
            }
            (Val::Obj(o), Val::Str(s)) => Ok(o.get(s).cloned().unwrap_or(Val::Null)),
            (s @ (Val::Arr(_) | Val::Obj(_)), _) => Err(Error::index(s, index.clone())),
            (s, _) => Err(Error::typ(s, Type::Iter.as_str())),
        }
    }

    fn range(self, range: jaq_core::val::Range<&Self>) -> ValR {
        let (from, upto) = (range.start, range.end);
        match self {
            Val::Arr(a) => {
                let len = a.len();
                let from = from.as_ref().map(|i| i.as_int()).transpose();
                let upto = upto.as_ref().map(|i| i.as_int()).transpose();
                from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
                    let from = abs_bound(from, len, 0);
                    let upto = abs_bound(upto, len, len);
                    let (skip, take) = skip_take(from, upto);
                    a.iter().skip(skip).take(take).cloned().collect()
                })
            }
            Val::Str(s) => {
                let len = s.chars().count();
                let from = from.as_ref().map(|i| i.as_int()).transpose();
                let upto = upto.as_ref().map(|i| i.as_int()).transpose();
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

    fn map_values<'a, I: Iterator<Item = ValX<'a>>>(
        self,
        opt: path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
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

    fn map_index<'a, I: Iterator<Item = ValX<'a>>>(
        mut self,
        index: &Self,
        opt: path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
        match self {
            Val::Obj(ref mut o) => {
                use indexmap::map::Entry::{Occupied, Vacant};
                let o = Rc::make_mut(o);
                let i = match index {
                    Val::Str(s) => s,
                    i => return opt.fail(self, |v| Exn::from(Error::index(v, i.clone()))),
                };
                match o.entry(Rc::clone(i)) {
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
                let i = match index.as_int().and_then(abs_or) {
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

    fn map_range<'a, I: Iterator<Item = ValX<'a>>>(
        mut self,
        range: jaq_core::val::Range<&Self>,
        opt: path::Opt,
        f: impl Fn(Self) -> I,
    ) -> ValX<'a> {
        if let Val::Arr(ref mut a) = self {
            let a = Rc::make_mut(a);
            let from = range.start.as_ref().map(|i| i.as_int()).transpose();
            let upto = range.end.as_ref().map(|i| i.as_int()).transpose();
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

    fn as_isize(&self) -> Option<isize> {
        match self {
            Self::Int(i) => Some(*i),
            _ => None,
        }
    }

    fn as_f64(&self) -> Result<f64, Error> {
        Self::as_float(self)
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
            Val::Null => Ok(Val::Int(0)),
            Val::Bool(_) => Err(Error::str(format_args!("{self} has no length"))),
            Val::Int(i) => Ok(Val::Int(i.abs())),
            Val::Num(n) => Val::from_dec_str(n).length(),
            Val::Float(f) => Ok(Val::Float(f.abs())),
            Val::Str(s) => Ok(Val::Int(s.chars().count() as isize)),
            Val::Arr(a) => Ok(Val::Int(a.len() as isize)),
            Val::Obj(o) => Ok(Val::Int(o.len() as isize)),
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
pub fn funs() -> impl Iterator<Item = Filter<Native<Val>>> {
    let base_run = base_funs().into_vec().into_iter().map(run);
    base_run.chain([run(parse_fun())])
}

fn box_once_err<'a>(r: ValR) -> BoxIter<'a, ValX<'a>> {
    box_once(r.map_err(Exn::from))
}

fn base_funs() -> Box<[Filter<RunPtr<Val>>]> {
    Box::new([
        ("tojson", v(0), |_, cv| {
            box_once(Ok(cv.1.to_string().into()))
        }),
        ("length", v(0), |_, cv| box_once_err(cv.1.length())),
        ("keys_unsorted", v(0), |_, cv| {
            box_once_err(cv.1.keys_unsorted().map(|v| Val::Arr(v.into())))
        }),
        ("contains", v(1), |_, cv| {
            unary(cv, |x, y| Ok(Val::from(x.contains(&y))))
        }),
        ("has", v(1), |_, cv| {
            unary(cv, |v, k| v.has(&k).map(Val::from))
        }),
        ("indices", v(1), |_, cv| {
            let to_int = |i: usize| Val::Int(i.try_into().unwrap());
            unary(cv, move |x, v| {
                x.indices(&v).map(|idxs| idxs.map(to_int).collect())
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
fn parse_fun() -> Filter<RunPtr<Val>> {
    ("fromjson", v(0), |_, cv| {
        box_once_err(cv.1.as_str().and_then(|s| from_json(s)))
    })
}

fn skip_take(from: usize, until: usize) -> (usize, usize) {
    (from, if until > from { until - from } else { 0 })
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
    pub fn obj(m: Map<Rc<String>, Self>) -> Self {
        Self::Obj(m.into())
    }

    /// If the value is integer, return it, else fail.
    fn as_int(&self) -> Result<isize, Error> {
        match self {
            Self::Int(i) => Ok(*i),
            _ => Err(Error::typ(self.clone(), Type::Int.as_str())),
        }
    }

    /// If the value is or can be converted to float, return it, else
    /// fail.
    fn as_float(&self) -> Result<f64, Error> {
        match self {
            Self::Int(n) => Ok(*n as f64),
            Self::Float(n) => Ok(*n),
            Self::Num(n) => n
                .parse()
                .or(Err(Error::typ(self.clone(), Type::Float.as_str()))),
            _ => Err(Error::typ(self.clone(), Type::Float.as_str())),
        }
    }

    /// If the value is a string, return it, else fail.
    fn into_str(self) -> Result<Rc<String>, Error> {
        match self {
            Self::Str(s) => Ok(s),
            _ => Err(Error::typ(self, Type::Str.as_str())),
        }
    }

    /// If the value is a string, return it, else fail.
    fn as_str(&self) -> Result<&Rc<String>, Error> {
        match self {
            Self::Str(s) => Ok(s),
            _ => Err(Error::typ(self.clone(), Type::Str.as_str())),
        }
    }

    /// If the value is an array, return it, else fail.
    fn into_arr(self) -> Result<Rc<Vec<Self>>, Error> {
        match self {
            Self::Arr(a) => Ok(a),
            _ => Err(Error::typ(self, Type::Arr.as_str())),
        }
    }

    /// Try to parse a string to a [`Self::Float`], else return [`Self::Null`].
    fn from_dec_str(n: &str) -> Self {
        n.parse().map_or(Self::Null, Self::Float)
    }

    /// Return true if `value | .[key]` is defined.
    ///
    /// Fail on values that are neither arrays nor objects.
    fn has(&self, key: &Self) -> Result<bool, Error> {
        match (self, key) {
            (Self::Arr(a), Self::Int(i)) if *i >= 0 => Ok((*i as usize) < a.len()),
            (Self::Obj(o), Self::Str(s)) => Ok(o.contains_key(&**s)),
            _ => Err(Error::index(self.clone(), key.clone())),
        }
    }

    /// Return any `key` for which `value | .[key]` is defined.
    ///
    /// Fail on values that are neither arrays nor objects.
    fn keys_unsorted(&self) -> Result<Vec<Self>, Error> {
        match self {
            Self::Arr(a) => Ok((0..a.len() as isize).map(Self::Int).collect()),
            Self::Obj(o) => Ok(o.keys().map(|k| Self::Str(Rc::clone(k))).collect()),
            _ => Err(Error::typ(self.clone(), Type::Iter.as_str())),
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
            Token::DigitOrMinus => {
                let (num, parts) = lexer.num_string()?;
                // if we are dealing with an integer ...
                if parts.dot.is_none() && parts.exp.is_none() {
                    // ... that fits into an isize
                    if let Ok(i) = num.parse() {
                        return Ok(Self::Int(i));
                    }
                }
                Ok(Self::Num(Rc::new(num.to_string())))
            }
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

impl From<bool> for Val {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<isize> for Val {
    fn from(i: isize) -> Self {
        Self::Int(i)
    }
}

impl From<f64> for Val {
    fn from(f: f64) -> Self {
        Self::Float(f)
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Self::Str(Rc::new(s))
    }
}

impl FromIterator<Self> for Val {
    fn from_iter<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        Self::Arr(Rc::new(iter.into_iter().collect()))
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
            (l, r) => Err(Error::math(l, ops::Math::Add, r)),
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
            (Arr(mut l), Arr(r)) => {
                let r = r.iter().collect::<alloc::collections::BTreeSet<_>>();
                Rc::make_mut(&mut l).retain(|x| !r.contains(x));
                Ok(Arr(l))
            }
            (l, r) => Err(Error::math(l, ops::Math::Sub, r)),
        }
    }
}

fn obj_merge(l: &mut Rc<Map<Rc<String>, Val>>, r: Rc<Map<Rc<String>, Val>>) {
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
        use Val::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x * y)),
            (Float(f), Int(i)) | (Int(i), Float(f)) => Ok(Float(f * i as f64)),
            (Float(x), Float(y)) => Ok(Float(x * y)),
            (Str(s), Int(i)) | (Int(i), Str(s)) if i > 0 => Ok(Self::from(s.repeat(i as usize))),
            // string multiplication with negatives or 0 results in null
            // <https://jqlang.github.io/jq/manual/#Builtinoperatorsandfunctions>
            (Str(_), Int(_)) | (Int(_), Str(_)) => Ok(Null),
            (Num(n), r) => Self::from_dec_str(&n) * r,
            (l, Num(n)) => l * Self::from_dec_str(&n),
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
        use Val::{Float, Int, Num, Str};
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Float(x as f64 / y as f64)),
            (Float(f), Int(i)) => Ok(Float(f / i as f64)),
            (Int(i), Float(f)) => Ok(Float(i as f64 / f)),
            (Float(x), Float(y)) => Ok(Float(x / y)),
            (Num(n), r) => Self::from_dec_str(&n) / r,
            (l, Num(n)) => l / Self::from_dec_str(&n),
            (Str(x), Str(y)) => Ok(split(&x, &y).map(Val::from).collect()),
            (l, r) => Err(Error::math(l, ops::Math::Div, r)),
        }
    }
}

impl core::ops::Rem for Val {
    type Output = ValR;
    fn rem(self, rhs: Self) -> Self::Output {
        use Val::Int;
        match (self, rhs) {
            (Int(x), Int(y)) if y != 0 => Ok(Int(x % y)),
            (l, r) => Err(Error::math(l, ops::Math::Rem, r)),
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
            x => Err(Error::typ(x, Type::Num.as_str())),
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
                float_eq(*i as f64, *f)
            }
            (Self::Float(x), Self::Float(y)) => float_eq(*x, *y),
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
        use Ordering::{Equal, Greater, Less};
        match (self, other) {
            (Self::Null, Self::Null) => Equal,
            (Self::Bool(x), Self::Bool(y)) => x.cmp(y),
            (Self::Int(x), Self::Int(y)) => x.cmp(y),
            (Self::Int(i), Self::Float(f)) => float_cmp(*i as f64, *f),
            (Self::Float(f), Self::Int(i)) => float_cmp(*f, *i as f64),
            (Self::Float(x), Self::Float(y)) => float_cmp(*x, *y),
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

fn float_eq(left: f64, right: f64) -> bool {
    float_cmp(left, right) == Ordering::Equal
}

fn float_cmp(left: f64, right: f64) -> Ordering {
    if left == 0. && right == 0. {
        Ordering::Equal
    } else {
        f64::total_cmp(&left, &right)
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
            Self::Int(i) => write!(f, "{i}"),
            Self::Float(x) if x.is_finite() => write!(f, "{x:?}"),
            Self::Float(_) => write!(f, "null"),
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
                    write!(f, "{k:?}:{v}")?;
                }
                iter.try_for_each(|(k, v)| write!(f, ",{k:?}:{v}"))?;
                write!(f, "}}")
            }
        }
    }
}
