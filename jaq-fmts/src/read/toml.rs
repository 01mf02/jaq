//! TOML support.
use jaq_json::{Num, Val};
use toml_span::{Error, Value};

/// Parse a TOML document from a string.
pub fn parse(s: &str) -> Result<Val, Error> {
    toml_span::parse(s).map(from)
}

fn from(mut v: Value) -> Val {
    use toml_span::value::ValueInner::*;
    match v.take() {
        String(s) => Val::from(s.into_owned()),
        Integer(i) => Val::Num(Num::from_integral(i)),
        Float(f) => Val::from(f),
        Boolean(b) => Val::from(b),
        Array(a) => a.into_iter().map(from).collect(),
        Table(t) => {
            let mut kvs: Vec<_> = t.into_iter().collect();
            kvs.sort_by_key(|(k, _v)| k.span.start);
            let kvs = kvs
                .into_iter()
                .map(|(k, v)| (Val::from(k.name.into_owned()), from(v)));
            Val::obj(kvs.collect())
        }
    }
}
