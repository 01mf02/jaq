//! Tests for named core filters, sorted by name.

pub mod common;

use common::give;
use serde_json::json;

yields!(bsearch_absent1, "[1, 3] | bsearch(0)", -1);
yields!(bsearch_absent2, "[1, 3] | bsearch(2)", -2);
yields!(bsearch_absent3, "[1, 3] | bsearch(4)", -3);
yields!(bsearch_present, "[1, 3] | [bsearch(1, 3)]", [0, 1]);

#[test]
fn has() {
    /* TODO: reenable these tests
    let err = Error::Index(Val::Null, Val::Int(0));
    fail(json!(null), "has(0)", err);
    let err = Error::Index(Val::Int(0), Val::Null);
    fail(json!(0), "has([][0])", err);
    let err = Error::Index(Val::Int(0), Val::Int(1));
    fail(json!(0), "has(1)", err);
    let err = Error::Index(Val::Str("a".to_string().into()), Val::Int(0));
    fail(json!("a"), "has(0)", err);
    */

    give(json!([0, null]), "has(0)", json!(true));
    give(json!([0, null]), "has(1)", json!(true));
    give(json!([0, null]), "has(2)", json!(false));

    give(json!({"a": 1, "b": null}), r#"has("a")"#, json!(true));
    give(json!({"a": 1, "b": null}), r#"has("b")"#, json!(true));
    give(json!({"a": 1, "b": null}), r#"has("c")"#, json!(false));
}

yields!(indices_str, r#""a,b, cd, efg" | indices(", ")"#, [3, 7]);
yields!(
    indices_arr_num,
    "[0, 1, 2, 1, 3, 1, 4] | indices(1)",
    [1, 3, 5]
);
yields!(
    indices_arr_arr,
    "[0, 1, 2, 3, 1, 4, 2, 5, 1, 2, 6, 7] | indices([1, 2])",
    [1, 8]
);
yields!(indices_arr_str, r#"["a", "b", "c"] | indices("b")"#, [1]);

yields!(indices_arr_empty, "[0, 1] | indices([])", json!([]));
yields!(indices_arr_larger, "[1, 2] | indices([1, 2, 3])", json!([]));

yields!(indices_arr_overlap, "[0, 0, 0] | indices([0, 0])", [0, 1]);
yields!(indices_str_overlap, r#""aaa" | indices("aa")"#, [0, 1]);
yields!(indices_str_gb1, r#""🇬🇧!" | indices("!")"#, [2]);
yields!(indices_str_gb2, r#""🇬🇧🇬🇧" | indices("🇬🇧")"#, [0, 2]);

yields!(length_str_foo, r#""ƒoo" | length"#, 3);
yields!(length_str_namaste, r#""नमस्ते" | length"#, 6);
yields!(length_obj, r#"{"a": 5, "b": 3} | length"#, 2);
yields!(length_int_pos, " 2 | length", 2);
yields!(length_int_neg, "-2 | length", 2);
yields!(length_float_pos, " 2.5 | length", 2.5);
yields!(length_float_neg, "-2.5 | length", 2.5);

#[test]
fn tojson() {
    // TODO: correct this
    give(json!(1.0), "tojson", json!("1.0"));
    give(json!(0), "1.0 | tojson", json!("1.0"));
    give(json!(0), "1.1 | tojson", json!("1.1"));
    give(json!(0), "0.0 / 0.0 | tojson", json!("null"));
    give(json!(0), "1.0 / 0.0 | tojson", json!("null"));
}

#[test]
fn tonumber() {
    give(json!(1.0), "tonumber", json!(1.0));
    give(json!("1.0"), "tonumber", json!(1.0));
    give(json!("42"), "tonumber", json!(42));
    give(json!("null"), "try tonumber catch -7", json!(-7));
    give(json!("true"), "try tonumber catch -7", json!(-7));
    give(json!("str"), "try tonumber catch -7", json!(-7));
    give(json!("\"str\""), "try tonumber catch -7", json!(-7));
    give(json!("[3, 4]"), "try tonumber catch -7", json!(-7));
    give(json!("{\"a\": 1}"), "try tonumber catch -7", json!(-7));
}

#[test]
fn toboolean() {
    give(json!(false), "toboolean", json!(false));
    give(json!("true"), "toboolean", json!(true));
    give(json!("false"), "toboolean", json!(false));
    give(json!("null"), "try toboolean catch -7", json!(-7));
    give(json!("3"), "try toboolean catch -7", json!(-7));
    give(json!("str"), "try toboolean catch -7", json!(-7));
    give(json!("\"str\""), "try toboolean catch -7", json!(-7));
    give(json!("[3, 4]"), "try toboolean catch -7", json!(-7));
    give(json!("{\"a\": 1}"), "try toboolean catch -7", json!(-7));
}

#[test]
fn math_rem() {
    // generated with this command with modification for errors and float rounding
    // cargo run -- -rn 'def f: -2, -1, 0, 2.1, 3, 2000000001; f as $a | f as $b | "give!(json!(null), \"\($a) / \($b)\", \(try ($a % $b) catch tojson));"'
    // TODO: use fail!()?
    give(json!(null), "-2 % -2", json!(0));
    give(json!(null), "-2 % -1", json!(0));
    give(
        json!(null),
        "try (-2 % 0) catch .",
        json!("cannot calculate -2 % 0"),
    );
    give(json!(null), "-2 % 2.1", json!(-2.0));
    give(json!(null), "-2 % 3", json!(-2));
    give(json!(null), "-2 % 2000000001", json!(-2));
    give(json!(null), "-1 % -2", json!(-1));
    give(json!(null), "-1 % -1", json!(0));
    give(
        json!(null),
        "try (-1 % 0) catch .",
        json!("cannot calculate -1 % 0"),
    );
    give(json!(null), "-1 % 2.1", json!(-1.0));
    give(json!(null), "-1 % 3", json!(-1));
    give(json!(null), "-1 % 2000000001", json!(-1));
    give(json!(null), "0 % -2", json!(0));
    give(json!(null), "0 % -1", json!(0));
    give(
        json!(null),
        "try (0 % 0) catch .",
        json!("cannot calculate 0 % 0"),
    );
    give(json!(null), "0 % 2.1", json!(0.0));
    give(json!(null), "0 % 3", json!(0));
    give(json!(null), "0 % 2000000001", json!(0));
    give(json!(null), "2.1 % -2 | . * 1000 | round", json!(100));
    give(json!(null), "2.1 % -1 | . * 1000 | round", json!(100));
    give(json!(null), "2.1 % 0 | isnan", json!(true));
    give(json!(null), "2.1 % 2.1", json!(0.0));
    give(json!(null), "2.1 % 3", json!(2.1));
    give(json!(null), "2.1 % 2000000001", json!(2.1));
    give(json!(null), "3 % -2", json!(1));
    give(json!(null), "3 % -1", json!(0));
    give(
        json!(null),
        "try (3 % 0) catch .",
        json!("cannot calculate 3 % 0"),
    );
    give(json!(null), "3 % 2.1 | . * 1000 | round", json!(900));
    give(json!(null), "3 % 3", json!(0));
    give(json!(null), "3 % 2000000001", json!(3));
    give(json!(null), "2000000001 % -2", json!(1));
    give(json!(null), "2000000001 % -1", json!(0));
    give(
        json!(null),
        "try (2000000001 % 0) catch .",
        json!("cannot calculate 2000000001 % 0"),
    );
    give(
        json!(null),
        "2000000001 % 2.1 | . * 1000 | round",
        json!(1800), // 1000 in jq
    );
    give(json!(null), "2000000001 % 3", json!(0));
    give(json!(null), "2000000001 % 2000000001", json!(0));
}
