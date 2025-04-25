//! Tests for filters in the standard library, sorted by name.

pub mod common;

use common::give;
use serde_json::json;

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

    give(json!([]), "from_entries", json!({}));
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

yields!(
    keys,
    r#"{"foo":null,"abc":null,"fax":null,"az":null} | keys"#,
    ["abc", "az", "fax", "foo"]
);

yields!(paths_num, "1 | [paths]", json!([]));
yields!(paths_null, "null | [paths]", json!([]));
yields!(paths_arr, "[1, 2] | [paths]", [[0], [1]]);
yields!(
    paths_arr_obj,
    "{a: [1, [2]], b: {c: 3}} | [paths]",
    json!([["a"], ["a", 0], ["a", 1], ["a", 1, 0], ["b"], ["b", "c"]])
);

yields!(
    paths_filter,
    "[{a: 1, b: [2, 3]} | paths(. < [])]",
    json!([["a"], ["b", 0], ["b", 1]])
);

const RECURSE_PATHS: &str = "def paths:
  { x: ., p: [] } |
  recurse((.x | keys_unsorted?)[] as $k | .x |= .[$k] | .p += [$k]) |
  .p | if . == [] then empty else . end;";

yields!(
    recurse_paths,
    &(RECURSE_PATHS.to_owned() + "{a: [1, [2]], b: {c: 3}} | [paths]"),
    json!([["a"], ["a", 0], ["a", 1], ["a", 1, 0], ["b"], ["b", "c"]])
);

#[test]
fn transpose() {
    let y = json!([[1, 2], [3, null]]);
    give(json!([[1, 3], [2]]), "transpose", y);

    let y = json!([[1, 2], [3, 4]]);
    give(json!([[1, 3], [2, 4]]), "transpose", y);
}

#[test]
fn walk() {
    give(
        json!([[4, 1, 7], [8, 5, 2], [3, 6, 9]]),
        r#"walk(if . < [] then . else sort end)"#,
        json!([[1, 4, 7], [2, 5, 8], [3, 6, 9]]),
    );

    give(
        json!({"a": {"b": 1, "c": 2}}),
        r#"walk(if . < {} then . + 1 else . + {"l": length} end)"#,
        json!({"a": {"b": 2, "c": 3, "l": 2}, "l": 1}),
    );
}

#[test]
fn while_until() {
    give(
        json!(1),
        "[while(. < 100; . * 2)]",
        json!([1, 2, 4, 8, 16, 32, 64]),
    );
    give(
        json!("a"),
        "[while(length < 4; . + \"a\")]",
        json!(["a", "aa", "aaa"]),
    );
    give(
        json!([1, 2, 3]),
        "[while(length > 0; .[1:])]",
        json!([[1, 2, 3], [2, 3], [3]]),
    );

    give(json!(50), "until(. > 100; . * 2)", json!(200));
    give(
        json!([1, 2, 3]),
        "until(length == 1; .[1:]) | .[0]",
        json!(3),
    );
    give(
        json!(5),
        "[.,1] | until(.[0] < 1; [.[0] - 1, .[1] * .[0]]) | .[1]",
        json!(120),
    );
}

yields!(
    format_json,
    r#"[0, 0 == 0, {}.a, "hello", {}, [] | @json]"#,
    ["0", "true", "null", "\"hello\"", "{}", "[]"]
);
