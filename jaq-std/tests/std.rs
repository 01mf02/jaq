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
fn flatten() {
    let a0 = || json!([1, [{"a": 2}, [3]]]);
    let a1 = || json!([1, {"a": 2}, [3]]);
    let a2 = || json!([1, {"a": 2}, 3]);
    give(a0(), "flatten", json!(a2()));
    let f = "[flatten(0, 1, 2, 3)]";
    give(a0(), f, json!([a0(), a1(), a2(), a2()]));

    give(
        json!([[[0], 1], 2, [3, [4]]]),
        "flatten",
        json!([0, 1, 2, 3, 4]),
    );

    // here, we diverge from jq, which returns just 1
    give(json!({"a": 1}), "flatten", json!([{"a": 1}]));
    // jq gives an error here
    give(json!(0), "flatten", json!([0]));
}

#[test]
fn inside() {
    give(
        json!(["foo", "bar"]),
        r#"map(in({"foo": 42}))"#,
        json!([true, false]),
    );
    give(json!([2, 0]), r#"map(in([0,1]))"#, json!([false, true]));

    give(json!("bar"), r#"inside("foobar")"#, json!(true));

    let f = r#"inside(["foobar", "foobaz", "blarp"])"#;
    give(json!(["baz", "bar"]), f, json!(true));
    give(json!(["bazzzz", "bar"]), f, json!(false));

    let f = r#"inside({"foo": 12, "bar":[1,2,{"barp":12, "blip":13}]})"#;
    give(json!({"foo": 12, "bar": [{"barp": 12}]}), f, json!(true));
    give(json!({"foo": 12, "bar": [{"barp": 15}]}), f, json!(false));
}

#[test]
fn isfinite() {
    give(json!(0), "isfinite", json!(true));
    give(json!(1), "isfinite", json!(true));
    give(json!(0), "nan | isfinite", json!(true));
    give(json!(0), "infinite | isfinite", json!(false));
    give(json!(0), "-infinite | isfinite", json!(false));
    give(json!(""), "isfinite", json!(false));
}

#[test]
fn isnormal() {
    give(json!(0), "isnormal", json!(false));
    give(json!(1), "isnormal", json!(true));
    give(json!(0), "nan | isnormal", json!(false));
    give(json!(0), "infinite | isnormal", json!(false));
    give(json!(0), "-infinite | isnormal", json!(false));
    give(json!(""), "isnormal", json!(false));
}

#[test]
fn join() {
    give(json!([]), r#"join(" ")"#, json!(null));
    give(
        json!(["Hello", "world"]),
        r#"join(" ")"#,
        json!("Hello world"),
    );

    // 2 + 1 + 3 + 1 + 4 + 1 + 5
    give(json!([2, 3, 4, 5]), "join(1)", json!(17));
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

    let v = json!([null, false, true, 1, 1.0, "", "a", [], [0], {}, {"a": 1}]);
    let iterables = json!([[], [0], {}, {"a": 1}]);
    let scalars = json!([null, false, true, 1, 1.0, "", "a"]);
    let values = json!([false, true, 1, 1.0, "", "a", [], [0], {}, {"a": 1}]);
    give(v.clone(), ".[] | nulls", json!(null));
    give(v.clone(), "[.[] | booleans]", json!([false, true]));
    give(v.clone(), "[.[] | numbers]", json!([1, 1.0]));
    give(v.clone(), "[.[] | strings]", json!(["", "a"]));
    give(v.clone(), "[.[] | arrays]", json!([[], [0]]));
    give(v.clone(), "[.[] | objects]", json!([{}, {"a": 1}]));
    give(v.clone(), "[.[] | iterables]", iterables);
    give(v.clone(), "[.[] | scalars]", scalars);
    give(v.clone(), "[.[] | values]", values);
}

#[test]
fn transpose() {
    let y = json!([[1, 2], [3, null]]);
    give(json!([[1, 3], [2]]), "transpose", y);

    let y = json!([[1, 2], [3, 4]]);
    give(json!([[1, 3], [2, 4]]), "transpose", y);
}

#[test]
fn typ() {
    give(json!({"a": 1, "b": 2}), "type", json!("object"));
    give(json!([0, 1]), "type", json!("array"));
    give(json!("Hello"), "type", json!("string"));
    give(json!(1), "type", json!("number"));
    give(json!(1.0), "type", json!("number"));
    give(json!(true), "type", json!("boolean"));
    give(json!(null), "type", json!("null"));
}
