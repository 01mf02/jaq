//! Tests for filters in the standard library, sorted by name.

pub mod common;

use common::{give, gives};
use serde_json::json;

#[test]
fn add() {
    give(json!({"a": 1, "b": 2}), "add", json!(3));
    give(json!([[0, 1], [2, 3]]), "add", json!([0, 1, 2, 3]));
}

#[test]
fn all() {
    give(json!({"a": false, "b": true}), "all", json!(false));
    give(json!({"a": 1, "b": 2}), "all", json!(true));

    let f = "def positive(f): all(f; . > 0); positive(.[])";
    give(json!([1, 2]), f, json!(true));
}

#[test]
fn any() {
    give(json!({"a": false, "b": true}), "any", json!(true));
}

#[test]
fn entries() {
    let obj = json!({"a": 1, "b": 2});
    let entries = json!([{"key": "a", "value": 1}, {"key": "b", "value": 2}]);
    let objk = json!({"ak": 1, "bk": 2});

    give(obj.clone(), "to_entries", entries.clone());
    give(entries, "from_entries", obj.clone());
    give(obj, r#"with_entries(.key += "k")"#, objk);

    let arr = json!([null, 0]);
    let entries = json!([{"key": 0, "value": null}, {"key": 1, "value": 0}]);
    give(arr, "to_entries", entries);
}

#[test]
fn map() {
    give(json!([1, 2]), "map(.+1)", json!([2, 3]));
}

#[test]
fn min_max() {
    give(json!([]), "min", json!(null));
    give(json!([]), "max", json!(null));
    give(json!([1, 4, 2]), "min", json!(1));
    give(json!([1, 4, 2]), "max", json!(4));
}

#[test]
fn nth() {
    let fib = "[0,1] | recurse([.[1], add]) | .[0]";
    give(json!(10), &format!("nth(.; {})", fib), json!(55));
}

#[test]
fn range_reverse() {
    give(json!(null), "[range(1, 2)]", json!([0, 0, 1]));

    let y = json!([-1, -1, 0, 1, 1]);
    give(json!(null), "[range(-1, 1; 0, 2)]", y);

    give(json!(3), "[range(.)] | reverse", json!([2, 1, 0]));
}

#[test]
fn recurse() {
    let x = json!({"a":0,"b":[1]});
    gives(x.clone(), "recurse", [x, json!(0), json!([1]), json!(1)]);

    let y = [json!(1), json!(2), json!(3)];
    gives(json!(1), "recurse(.+1; . < 4)", y);

    let y = [json!(2), json!(4), json!(16)];
    gives(json!(2), "recurse(. * .; . < 20)", y);
}

#[test]
fn repeat() {
    let y = json!([0, 1, 0, 1]);
    give(json!([0, 1]), "[limit(4; repeat(.[]))]", y);
}

#[test]
fn select() {
    give(json!([1, 2]), ".[] | select(.>1)", json!(2));
    give(json!([0, 1, 2]), "map(select(.<1, 1<.))", json!([0, 2]));
}

#[test]
fn transpose() {
    let y = json!([[1, 2], [3, null]]);
    give(json!([[1, 3], [2]]), "transpose", y);

    let y = json!([[1, 2], [3, 4]]);
    give(json!([[1, 3], [2, 4]]), "transpose", y);
}
