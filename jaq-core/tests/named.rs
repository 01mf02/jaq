//! Tests for named core filters, sorted by name.

pub mod common;

use common::{fail, give, gives};
use jaq_core::{Error, Val};
use serde_json::json;

#[test]
fn ascii() {
    give(json!("aAaAäの"), "ascii_upcase", json!("AAAAäの"));
    give(json!("aAaAäの"), "ascii_downcase", json!("aaaaäの"));
}

#[test]
fn explode_implode() {
    give(json!("❤ の"), "explode", json!([10084, 32, 12398]));
    give(json!("y̆"), "explode", json!([121, 774]));

    give(json!("❤ の"), "explode | implode", json!("❤ の"));
    give(json!("y̆"), "explode | implode", json!("y̆"));

    fail(json!([1114112]), "implode", Error::Char(1114112));
}

#[test]
fn first_last() {
    gives(json!([]), "first(.[])", []);
    gives(json!([]), "last(.[])", []);
    give(json!([1, 2, 3]), "first(.[])", json!(1));
    give(json!([1, 2, 3]), "last(.[])", json!(3));
}

#[test]
fn has() {
    give(json!(null), "has(0)", json!(false));

    let err = Error::Has(Val::Int(0), Val::Null);
    fail(json!(0), "has([] | .[0])", err);
    let err = Error::Has(Val::Int(0), Val::Int(1));
    fail(json!(0), "has(1)", err);
    let err = Error::Has(Val::Str("a".to_string().into()), Val::Int(0));
    fail(json!("a"), "has(0)", err);

    give(json!([0, null]), "has(0)", json!(true));
    give(json!([0, null]), "has(1)", json!(true));
    give(json!([0, null]), "has(2)", json!(false));

    give(json!({"a": 1, "b": null}), r#"has("a")"#, json!(true));
    give(json!({"a": 1, "b": null}), r#"has("b")"#, json!(true));
    give(json!({"a": 1, "b": null}), r#"has("c")"#, json!(false));
}

#[test]
fn json() {
    give(json!(1.0), "tojson", json!("1.0"));
    give(json!(0), "1.0 | tojson", json!("1.0"));
    give(json!(0), "1.1 | tojson", json!("1.1"));
    give(json!(0), "0.0 / 0.0 | tojson", json!("null"));
    give(json!(0), "1.0 / 0.0 | tojson", json!("null"));
}

#[test]
fn keys() {
    give(json!([0, null, "a"]), "keys", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "keys", json!(["a", "b"]));

    fail(json!(0), "keys", Error::Keys(Val::Int(0)));
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

#[test]
fn split() {
    give(json!("aöß"), r#"split("")"#, json!(["a", "ö", "ß"]));
    give(json!("abcabcdab"), r#"split("ab")"#, json!(["", "c", "cd", ""]));
}
