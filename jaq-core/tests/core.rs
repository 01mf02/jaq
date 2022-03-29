//! Tests for core filters, sorted by name.

pub mod common;

use jaq_core::{Error, Val};
use common::{give, gives, fail};
use serde_json::json;

#[test]
fn any() {
    give(json!({"a": false, "b": true}), "any", json!(true));
}

#[test]
fn all() {
    give(json!({"a": false, "b": true}), "all", json!(false));
    give(json!({"a": 1, "b": 2}), "all", json!(true))
}

#[test]
fn first_last() {
    gives(json!([]), "first(.[])", []);
    gives(json!([]), "last(.[])", []);
    give(json!([1, 2, 3]), "first(.[])", json!(1));
    give(json!([1, 2, 3]), "last(.[])", json!(3));
}

#[test]
fn fold() {
    // the corresponding jq command is: 'reduce range(1000) as $x (0; . + $x)'
    give(json!(0), "fold(0; range(1000); .[0] + .[1])", json!(499500));
}

#[test]
fn keys() {
    give(json!([0, null, "a"]), "[keys]", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "[keys]", json!(["a", "b"]));

    fail(json!(0), "keys", Error::Keys(Val::Pos(0)));
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
    // jaq does not support negative indices in limit
    give(json!(null), "[limit(1, 0, 3; 0, 1)]", json!([0, 0, 1]));
}

#[test]
fn recurse() {
    let x = json!({"a":0,"b":[1]});
    gives(x.clone(), "recurse", [x, json!(0), json!([1]), json!(1)]);

    let y = [json!(1), json!(2), json!(3)];
    gives(json!(1), "recurse(.+1; . < 4)", y.clone());
    gives(json!(1), "recurse(if . < 3 then .+1 else empty end)", y);

    let y = [json!(2), json!(4), json!(16)];
    gives(json!(2), "recurse(. * .; . < 20)", y);
}

#[test]
fn typ() {
    give(json!({"a": 1, "b": 2}), "type", json!("object"));
    give(json!("Hello"), "type", json!("string"));
    give(json!(1), "type", json!("number"));
    give(json!(1.0), "type", json!("number"));
    give(json!(true), "type", json!("boolean"));
    give(json!(null), "type", json!("null"));
}
