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

    give(json!([]), "from_entries", json!({}));
}

#[test]
fn flatten() {
    let a0 = || json!([1, [{"a": 2}, [3]]]);
    let a1 = || json!([1, {"a": 2}, [3]]);
    let a2 = || json!([1, {"a": 2}, 3]);
    give(a0(), "flatten", json!(a2()));
    let f = "[flatten(0, 1, 2, 3)]";
    give(a0(), f, json!([a0(), a1(), a2(), a2()]));
}

yields!(
    flatten_deep,
    "[[[0], 1], 2, [3, [4]]] | flatten",
    [0, 1, 2, 3, 4]
);

// here, we diverge from jq, which returns just 1
yields!(flatten_obj, "{a: 1} | flatten", json!([{"a": 1}]));
// jq gives an error here
yields!(flatten_num, "0 | flatten", [0]);

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

yields!(isfinite_true, "all((0, 1, nan); isfinite)", true);
yields!(
    isfinite_false,
    "any((infinite, -infinite, []); isfinite)",
    false
);

yields!(isnormal_true, "1 | isnormal", true);
yields!(
    isnormal_false,
    "any(0, nan, infinite, -infinite, []; isnormal)",
    false
);

yields!(join_empty, r#"[] | join(" ")"#, json!(null));
yields!(
    join_strs,
    r#"["Hello", "world"] | join(" ")"#,
    "Hello world"
);
// 2 + 1 + 3 + 1 + 4 + 1 + 5
yields!(join_nums, r#"[2, 3, 4, 5] | join(1)"#, 17);

yields!(map, "[1, 2] | map(.+1)", [2, 3]);

yields!(
    keys,
    r#"{"foo":null,"abc":null,"fax":null,"az":null} | keys"#,
    ["abc", "az", "fax", "foo"]
);

#[test]
fn min_max() {
    give(json!([]), "min", json!(null));
    give(json!([]), "max", json!(null));
    give(json!([1, 4, 2]), "min", json!(1));
    give(json!([1, 4, 2]), "max", json!(4));
    give(
        json!([{"a": {"b": {"c": 1}}}, {"a": {"b": {"c": 4}}}, {"a": {"b": {"c": 2}}}]),
        "min_by(.a.b.c)",
        json!({"a": {"b": {"c": 1}}}),
    );
    give(
        json!([{"a": {"b": {"c": 1}}}, {"a": {"b": {"c": 4}}}, {"a": {"b": {"c": 2}}}]),
        "max_by(.a.b.c)",
        json!({"a": {"b": {"c": 4}}}),
    );
}

#[test]
fn nth() {
    let fib = "[0,1] | recurse([.[1], add]) | .[0]";
    give(json!(10), &format!("nth(.; {})", fib), json!(55));

    let fib = "[0,1] | recurse([.[1], add])[0]";
    give(json!(10), &format!("nth(.; {})", fib), json!(55));
}

yields!(paths_num, "1 | [paths]", json!([]));
yields!(paths_null, "null | [paths]", json!([]));
yields!(paths_arr, "[1, 2] | [paths]", [[0], [1]]);
yields!(
    paths_arr_obj,
    "{a: [1, [2]], b: {c: 3}} | [paths]",
    json!([["a"], ["a", 0], ["a", 1], ["a", 1, 0], ["b"], ["b", "c"]])
);

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

    let x = json!([[[0], 1], 2, [3, [4]]]);

    let y = json!([[[1], 2], 3, [4, [5]]]);
    give(x.clone(), "(.. | scalars) |= .+1", y);

    let f = ".. |= if . < [] then .+1 else . + [42] end";
    let y = json!([[[1, 43], 2, 43], 3, [4, [5, 43], 43], 43]);
    // jq gives: `[[[1, 42], 2, 42], 3, [4, [5, 42], 42], 42]`
    give(x.clone(), f, y);

    let f = ".. |= if . < [] then .+1 else [42] + . end";
    let y = json!([43, [43, [43, 1], 2], 3, [43, 4, [43, 5]]]);
    // jq fails here with: "Cannot index number with number"
    give(x.clone(), f, y);
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
    sub,
    r#""XYZxyzXYZxyz" | sub("x";"Q")"#,
    json!("XYZQyzXYZxyz")
);
yields!(
    sub_flags,
    r#""XYZxyzXYZxyz" | sub("x";"Q";"i")"#,
    json!("QYZxyzXYZxyz")
);
yields!(
    gsub,
    r#""XYZxyzXYZxyz" | gsub("x";"Q")"#,
    json!("XYZQyzXYZQyz")
);
yields!(
    gsub_flags,
    r#""XYZxyzXYZxyz" | gsub("x";"Q";"i")"#,
    json!("QYZQyzQYZQyz")
);
