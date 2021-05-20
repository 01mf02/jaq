//! Tests for filters in the standard library, sorted by name.

pub mod common;

use common::give;
use serde_json::json;

#[test]
fn add() {
    give(json!({"a": 1, "b": 2}), "add", json!(3));
    give(json!([[0, 1], [2, 3]]), "add", json!([0, 1, 2, 3]));
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
fn range_reverse() {
    give(json!(null), "[range(1, 2)]", json!([0, 0, 1]));

    let y = json!([-1, -1, 0, 1, 1]);
    give(json!(null), "[range(-1, 1; 0, 2)]", y);

    give(json!(3), "[range(.)] | reverse", json!([2, 1, 0]));
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
