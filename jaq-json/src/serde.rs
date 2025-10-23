use super::Rc;
use alloc::fmt;
use alloc::string::String;
use alloc::vec::Vec;
use serde::de::{MapAccess, SeqAccess, Visitor};
use serde::Deserialize;

use crate::Num;
use crate::{Map, Val as Value};

impl<'de> serde::Deserialize<'de> for Value {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("any valid JSON value")
            }

            #[inline]
            fn visit_bool<E>(self, value: bool) -> Result<Value, E> {
                Ok(Value::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
                Ok(Value::Num(Num::from_integral(value)))
            }

            fn visit_i128<E>(self, value: i128) -> Result<Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Num(Num::from_integral(value)))
            }

            #[inline]
            fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
                Ok(Value::Num(Num::from_integral(value)))
            }

            fn visit_u128<E>(self, value: u128) -> Result<Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Num(Num::from_integral(value)))
            }

            #[inline]
            fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
                Ok(Value::Num(Num::Float(value)))
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(self, value: String) -> Result<Value, E> {
                Ok(Value::utf8_str(value.into_bytes()))
            }

            #[inline]
            fn visit_none<E>(self) -> Result<Value, E> {
                Ok(Value::Null)
            }

            #[inline]
            fn visit_some<D>(self, deserializer: D) -> Result<Value, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                Deserialize::deserialize(deserializer)
            }

            #[inline]
            fn visit_unit<E>(self) -> Result<Value, E> {
                Ok(Value::Null)
            }

            #[inline]
            fn visit_seq<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let mut vec = Vec::new();

                while let Some(elem) = visitor.next_element()? {
                    vec.push(elem);
                }

                Ok(Value::Arr(Rc::new(vec)))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut values = Map::default();

                while let Some((key, value)) = visitor.next_entry::<Value, Value>()? {
                    values.insert(key, value);
                }

                Ok(Value::Obj(Rc::new(values)))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}
