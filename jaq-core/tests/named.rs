//! Tests for named core filters, sorted by name.

pub mod common;

use common::{fail, give, gives};
use jaq_core::{Error, Val};
use serde_json::json;

#[test]
fn first_last() {
    gives(json!([]), "first(.[])", []);
    gives(json!([]), "last(.[])", []);
    give(json!([1, 2, 3]), "first(.[])", json!(1));
    give(json!([1, 2, 3]), "last(.[])", json!(3));
}

#[test]
fn keys() {
    give(json!([0, null, "a"]), "[keys]", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "[keys]", json!(["a", "b"]));

    fail(json!(0), "keys", Error::Keys(Val::Int(0.into())));
    fail(json!(null), "keys", Error::Keys(Val::Null));
}

#[test]
fn length() {
    give(json!("ƒoo"), "length", json!(3));
    give(json!("नमस्ते"), "length", json!(6));
    give(json!({"a": 5, "b": 3}), "length", json!(2));
    give(json!(2), "length", json!(2));
    give(json!(-2), "length", json!(2));
    give(json!(2.5), "length", json!(2.5));
    give(json!(-2.5), "length", json!(2.5));
}

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(json!(null), "limit(0; 1,2)", []);
    give(json!(null), "[limit(1, 0, 3; 0, 1)]", json!([0, 0, 1]));

    // here, jaq diverges from jq, which returns `[0, 1]`
    give(json!(null), "[limit(-1; 0, 1)]", json!([]));
}

#[test]
fn range() {
    let y = json!([-1, -1, 0, 1, 1]);
    give(json!(null), "[range(-1, 1; 0, 2)]", y);
}

#[test]
fn recurse() {
    let y = [json!(1), json!(2), json!(3)];
    gives(json!(1), "recurse(if . < 3 then .+1 else [] | .[] end)", y);
}

#[test]
fn round() {
    give(json!(1), "round", json!(1));
    give(json!(1.0), "round", json!(1));
    give(json!(-1.0), "round", json!(-1));
    give(json!(-1), "round", json!(-1));

    give(json!(-1.5), "round", json!(-2));
    give(json!(-1.5), "floor", json!(-2));
    give(json!(-1.5), "ceil", json!(-1));

    give(json!(-1.4), "round", json!(-1));
    give(json!(-1.4), "floor", json!(-2));
    give(json!(-1.4), "ceil", json!(-1));

    fail(json!([]), "round", Error::Round(Val::from(json!([]))));
    fail(json!({}), "round", Error::Round(Val::from(json!({}))));
}
