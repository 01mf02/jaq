use crate::{Num, Val};
use alloc::{fmt, string::String};
use serde::de::{Error, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};

struct ValueVisitor;

impl<'de> Visitor<'de> for ValueVisitor {
    type Value = Val;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_bool<E>(self, value: bool) -> Result<Val, E> {
        Ok(Val::Bool(value))
    }

    #[inline]
    fn visit_i64<E>(self, value: i64) -> Result<Val, E> {
        Ok(Val::Num(Num::from_integral(value)))
    }

    #[inline]
    fn visit_u64<E>(self, value: u64) -> Result<Val, E> {
        Ok(Val::Num(Num::from_integral(value)))
    }

    fn visit_i128<E: Error>(self, value: i128) -> Result<Val, E> {
        Ok(Val::Num(Num::from_integral(value)))
    }

    fn visit_u128<E: Error>(self, value: u128) -> Result<Val, E> {
        Ok(Val::Num(Num::from_integral(value)))
    }

    #[inline]
    fn visit_f64<E>(self, value: f64) -> Result<Val, E> {
        Ok(Val::Num(Num::Float(value)))
    }

    #[inline]
    fn visit_str<E: Error>(self, value: &str) -> Result<Val, E> {
        self.visit_string(String::from(value))
    }

    #[inline]
    fn visit_string<E>(self, value: String) -> Result<Val, E> {
        Ok(Val::utf8_str(value.into_bytes()))
    }

    #[inline]
    fn visit_none<E>(self) -> Result<Val, E> {
        Ok(Val::Null)
    }

    #[inline]
    fn visit_some<D: Deserializer<'de>>(self, deserializer: D) -> Result<Val, D::Error> {
        Deserialize::deserialize(deserializer)
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Val, E> {
        Ok(Val::Null)
    }

    #[inline]
    fn visit_seq<V: SeqAccess<'de>>(self, mut visitor: V) -> Result<Val, V::Error> {
        core::iter::from_fn(|| visitor.next_element().transpose()).collect()
    }

    fn visit_map<V: MapAccess<'de>>(self, mut visitor: V) -> Result<Val, V::Error> {
        core::iter::from_fn(|| visitor.next_entry().transpose())
            .collect::<Result<_, _>>()
            .map(Val::obj)
    }
}

impl<'de> Deserialize<'de> for Val {
    #[inline]
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Val, D::Error> {
        deserializer.deserialize_any(ValueVisitor)
    }
}
