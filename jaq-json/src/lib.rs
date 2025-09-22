//! JSON superset with binary data and non-string object keys.
//!
//! This crate provides a few macros for formatting / writing;
//! this is done in order to function with both
//! [`core::fmt::Write`] and [`std::io::Write`].
#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;
extern crate std;

#[cfg(feature = "formats")]
mod formats;
mod funs;
mod num;
#[macro_use]
mod write;

#[cfg(feature = "cbor")]
pub mod cbor;
#[cfg(feature = "json")]
pub mod json;
#[cfg(feature = "toml")]
pub mod toml;
#[cfg(feature = "xml")]
pub mod xml;
#[cfg(feature = "yaml")]
pub mod yaml;

#[cfg(feature = "serde_json")]
mod serde_json;

use alloc::{borrow::ToOwned, boxed::Box, rc::Rc, string::String, vec::Vec};
use bstr::{BStr, ByteSlice};
use bytes::{BufMut, Bytes, BytesMut};
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use jaq_core::box_iter::box_once;
use jaq_core::{load, ops, path, val, Exn};
use num_bigint::BigInt;
use num_traits::{cast::ToPrimitive, Signed};

pub use funs::base_funs;
#[cfg(feature = "formats")]
pub use funs::funs;
pub use num::Num;

/// JSON superset with binary data and non-string object keys.
///
/// This is the default value type for jaq.
#[derive(Clone, Debug, Default)]
pub enum Val {
    #[default]
    /// Null
    Null,
    /// Boolean
    Bool(bool),
    /// Number
    Num(Num),
    /// String
    Str(Bytes, Tag),
    /// Array
    Arr(Rc<Vec<Val>>),
    /// Object
    Obj(Rc<Map<Val, Val>>),
}

/// Interpretation of a string.
///
/// This influences the outcome of a few operations (e.g. slicing)
/// as well as how a string is printed.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Tag {
    /// Sequence of bytes, to be escaped
    Bytes,
    /// Sequence of UTF-8 code points
    ///
    /// Note that this does not require the actual bytes to be all valid UTF-8;
    /// this just means that the bytes are interpreted as UTF-8.
    /// An effort is made to preserve invalid UTF-8 as is, else
    /// replace invalid UTF-8 by the Unicode replacement character.
    Utf8,
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
    /*
    /// `{(0): 1}` or `0 | fromjson` or `0 | explode` or `"a b c" | split(0)`
    Str,
    */
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
            //Self::Str => "string",
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
            (Val::Str(a, Tag::Bytes), Val::Num(i @ (Num::Int(_) | Num::BigInt(_)))) => Ok(i
                .as_pos_usize()
                .and_then(|i| abs_index(i, a.len()))
                .map_or(Val::Null, |i| Val::from(a[i] as usize))),
            (Val::Arr(a), Val::Num(i @ (Num::Int(_) | Num::BigInt(_)))) => Ok(i
                .as_pos_usize()
                .and_then(|i| abs_index(i, a.len()))
                .map_or(Val::Null, |i| a[i].clone())),
            (Val::Obj(o), i) => Ok(o.get(i).cloned().unwrap_or(Val::Null)),
            (s @ (Val::Str(_, Tag::Bytes) | Val::Arr(_)), _) => Err(Error::index(s, index.clone())),
            (s, _) => Err(Error::typ(s, Type::Iter.as_str())),
        }
    }

    fn range(self, range: jaq_core::val::Range<&Self>) -> ValR {
        use bstr::Chars;
        match self {
            Val::Str(b, t @ Tag::Bytes) => Self::skip_take(range, b.len())
                .map(|(skip, take)| Val::Str(b.slice(skip..skip + take), t)),
            // TODO: This is very inefficient, because
            // we traverse the whole string regardless of the requested range.
            // It would be better to create a special case for when
            // all range bounds are positive.
            Val::Str(s, Tag::Utf8) => {
                Self::skip_take(range, s.chars().count()).map(|(skip, take)| {
                    let advance = |chars: &mut Chars, n| {
                        for _ in 0..n {
                            chars.next();
                        }
                    };
                    let mut sk = s.chars();
                    advance(&mut sk, skip);
                    let mut tk = sk.clone();
                    advance(&mut tk, take);
                    let slice = &sk.as_bytes()[..sk.as_bytes().len() - tk.as_bytes().len()];
                    Val::Str(s.slice_ref(slice), Tag::Utf8)
                })
            }
            Val::Arr(a) => Self::skip_take(range, a.len())
                .map(|(skip, take)| a.iter().skip(skip).take(take).cloned().collect()),
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
                let oob = || Error::str(format_args!("index {index} out of bounds"));
                let abs_or = |i| abs_index(i, a.len()).ok_or_else(oob);
                let i = match index.as_pos_usize().and_then(abs_or) {
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
            let from = range.start.as_ref().map(|i| i.as_pos_usize()).transpose();
            let upto = range.end.as_ref().map(|i| i.as_pos_usize()).transpose();
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

    fn into_string(self) -> Self {
        if let Self::Str(b, _tag) = self {
            Self::utf8_str(b)
        } else {
            Self::utf8_str(self.to_json())
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
        self.as_num().is_some_and(Num::is_int)
    }

    fn as_isize(&self) -> Option<isize> {
        self.as_num().and_then(Num::as_isize)
    }

    fn as_f64(&self) -> Result<f64, Error> {
        let fail = || Error::typ(self.clone(), Type::Float.as_str());
        self.as_num().and_then(Num::as_f64).ok_or_else(fail)
    }

    fn is_utf8_str(&self) -> bool {
        matches!(self, Self::Str(_, Tag::Utf8))
    }

    fn as_bytes(&self) -> Option<&[u8]> {
        if let Self::Str(b, _) = self {
            Some(b)
        } else {
            None
        }
    }

    fn as_sub_str(&self, sub: &[u8]) -> Self {
        match self {
            Self::Str(b, tag) => Self::Str(b.slice_ref(sub), *tag),
            _ => panic!(),
        }
    }

    fn from_utf8_bytes(b: impl AsRef<[u8]> + Send + 'static) -> Self {
        Self::Str(Bytes::from_owner(b), Tag::Utf8)
    }
}

/// Definitions of the standard library.
pub fn defs() -> impl Iterator<Item = load::parse::Def<&'static str>> {
    load::parse(include_str!("defs.jq"), |p| p.defs())
        .unwrap()
        .into_iter()
}

fn skip_take(from: usize, until: usize) -> (usize, usize) {
    (from, until.saturating_sub(from))
}

/// If a range bound is given, absolutise and clip it between 0 and `len`,
/// else return `default`.
fn abs_bound(i: Option<num::PosUsize>, len: usize, default: usize) -> usize {
    i.map_or(default, |i| core::cmp::min(i.wrap(len).unwrap_or(0), len))
}

/// Absolutise an index and return result if it is inside [0, len).
fn abs_index(i: num::PosUsize, len: usize) -> Option<usize> {
    i.wrap(len).filter(|i| *i < len)
}

impl Val {
    /// Construct an object value.
    pub fn obj(m: Map) -> Self {
        Self::Obj(m.into())
    }

    /// Construct a string that is interpreted as UTF-8.
    pub fn utf8_str(s: impl Into<Bytes>) -> Self {
        Self::Str(s.into(), Tag::Utf8)
    }

    /// Construct a string that is interpreted as bytes.
    pub fn byte_str(s: impl Into<Bytes>) -> Self {
        Self::Str(s.into(), Tag::Bytes)
    }

    fn as_num(&self) -> Option<&Num> {
        match self {
            Self::Num(n) => Some(n),
            _ => None,
        }
    }

    /// If the value is an integer in [-usize::MAX, +usize::MAX], return it, else fail.
    fn as_pos_usize(&self) -> Result<num::PosUsize, Error> {
        let fail = || Error::typ(self.clone(), Type::Int.as_str());
        self.as_num().and_then(Num::as_pos_usize).ok_or_else(fail)
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

    fn skip_take(range: jaq_core::val::Range<&Self>, len: usize) -> Result<(usize, usize), Error> {
        let (from, upto) = (range.start, range.end);
        let from = from.as_ref().map(|i| i.as_pos_usize()).transpose();
        let upto = upto.as_ref().map(|i| i.as_pos_usize()).transpose();
        from.and_then(|from| Ok((from, upto?))).map(|(from, upto)| {
            let from = abs_bound(from, len, 0);
            let upto = abs_bound(upto, len, len);
            skip_take(from, upto)
        })
    }

    fn to_json(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        write::write(&mut buf, self).unwrap();
        buf
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

impl From<usize> for Val {
    fn from(i: usize) -> Self {
        Self::Num(Num::from_integral(i))
    }
}

impl From<f64> for Val {
    fn from(f: f64) -> Self {
        Self::Num(Num::Float(f))
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Self::Str(Bytes::from_owner(s), Tag::Utf8)
    }
}

impl From<val::Range<Val>> for Val {
    fn from(r: val::Range<Val>) -> Self {
        let kv = |(k, v): (&str, Option<_>)| v.map(|v| (k.to_owned().into(), v));
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
            (Str(l, tag), Str(r, tag_)) if tag == tag_ => {
                let mut buf = BytesMut::from(l);
                buf.put(r);
                Ok(Str(buf.into(), tag))
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
            (s @ Str(..), Num(BigInt(i))) | (Num(BigInt(i)), s @ Str(..)) => {
                s * Num(Int(bigint_to_int_saturated(&i)))
            }
            (Str(s, tag), Num(Int(i))) | (Num(Int(i)), Str(s, tag)) if i > 0 => {
                Ok(Self::Str(s.repeat(i as usize).into(), tag))
            }
            // string multiplication with negatives or 0 results in null
            // <https://jqlang.github.io/jq/manual/#Builtinoperatorsandfunctions>
            (Str(..), Num(Int(_))) | (Num(Int(_)), Str(..)) => Ok(Null),
            (Obj(mut l), Obj(r)) => {
                obj_merge(&mut l, r);
                Ok(Obj(l))
            }
            (l, r) => Err(Error::math(l, ops::Math::Mul, r)),
        }
    }
}

/// Split a string by a given separator string.
fn split<'a>(s: &'a [u8], sep: &'a [u8]) -> Box<dyn Iterator<Item = &'a [u8]> + 'a> {
    if s.is_empty() {
        Box::new(core::iter::empty())
    } else if sep.is_empty() {
        // Rust's `split` function with an empty separator ("")
        // yields an empty string as first and last result
        // to prevent this, we are using `chars` instead
        Box::new(s.char_indices().map(|(start, end, _)| &s[start..end]))
    } else {
        Box::new(s.split_str(sep))
    }
}

impl core::ops::Div for Val {
    type Output = ValR;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(x), Self::Num(y)) => Ok(Self::Num(x / y)),
            (Self::Str(x, tag), Self::Str(y, tag_)) if tag == tag_ => Ok(split(&x, &y)
                .map(|s| Val::Str(x.slice_ref(s), tag))
                .collect()),
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
            (Self::Str(x, _), Self::Str(y, _)) => x.cmp(y),
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
            (Self::Str(..), _) => Less,
            (_, Self::Str(..)) => Greater,
            (Self::Arr(_), _) => Less,
            (_, Self::Arr(_)) => Greater,
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Num(x), Self::Num(y)) => x == y,
            (Self::Str(x, _tag), Self::Str(y, _)) => x == y,
            (Self::Arr(x), Self::Arr(y)) => x == y,
            (Self::Obj(x), Self::Obj(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for Val {}

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
            Self::Str(b, _) => hash_with(5, b, state),
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

/// Display bytes as valid UTF-8 string.
///
/// This maps invalid UTF-8 to the Unicode replacement character.
pub fn bstr(s: &(impl core::convert::AsRef<[u8]> + ?Sized)) -> impl fmt::Display + '_ {
    BStr::new(s)
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_val!(f, self, |v: &Val| v.fmt(f))
    }
}

#[cfg(feature = "formats")]
type BoxError = Box<dyn std::error::Error + Send + Sync>;

#[cfg(feature = "formats")]
fn invalid_data(e: impl Into<BoxError>) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, e)
}
