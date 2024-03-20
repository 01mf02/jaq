//! Tests for named core filters, sorted by name.

pub mod common;

use common::{fail, give, gives};
use jaq_interpret::error::{Error, Type};
use jaq_interpret::Val;
use serde_json::json;

yields!(repeat, "def r(f): f, r(f); [limit(3; r(1, 2))]", [1, 2, 1]);

yields!(lazy_array, "def f: 1, [f]; limit(1; f)", 1);

yields!(
    lazy_foreach,
    "def f: f; limit(1; foreach (1, f) as $x (0; .))",
    0
);

yields!(nested_rec, "def f: def g: 0, g; g; def h: h; first(f)", 0);

yields!(
    rec_two_var_args,
    "def f($a; $b): [$a, $b], f($a+1; $b+1); [limit(3; f(0; 1))]",
    [[0, 1], [1, 2], [2, 3]]
);

yields!(
    rec_update,
    "def upto($x): .[$x], (if $x > 0 then upto($x-1) else empty end); [1, 2, 3, 4] | upto(1) |= .+1",
    [2, 3, 3, 4]
);

#[test]
fn ascii() {
    give(json!("aAaAäの"), "ascii_upcase", json!("AAAAäの"));
    give(json!("aAaAäの"), "ascii_downcase", json!("aaaaäの"));
}

#[test]
fn dateiso8601() {
    give(
        json!("1970-01-02T00:00:00Z"),
        "fromdateiso8601",
        json!(86400),
    );
    give(
        json!("1970-01-02T00:00:00.123456789Z"),
        "fromdateiso8601",
        json!(86400.123456789),
    );
    give(json!(86400), "todateiso8601", json!("1970-01-02T00:00:00Z"));
    give(
        json!(86400.123456789),
        "todateiso8601",
        json!("1970-01-02T00:00:00.123456789Z"),
    );
}

#[test]
fn explode_implode() {
    give(json!("❤ の"), "explode", json!([10084, 32, 12398]));
    give(json!("y̆"), "explode", json!([121, 774]));

    give(json!("❤ の"), "explode | implode", json!("❤ の"));
    give(json!("y̆"), "explode | implode", json!("y̆"));

    give(json!([1114112]), "try implode catch -1", json!(-1));
}

yields!(first_empty, "[first({}[])]", json!([]));
yields!(first_some, "first(1, 2, 3)", 1);

yields!(
    format_text,
    r#"[0, 0 == 0, {}.a, "hello", {}, [] | @text]"#,
    ["0", "true", "null", "hello", "{}", "[]"]
);
yields!(
    format_json,
    r#"[0, 0 == 0, {}.a, "hello", {}, [] | @json]"#,
    ["0", "true", "null", "\"hello\"", "{}", "[]"]
);
yields!(
    format_html,
    r#""<p style='visibility: hidden'>sneaky</p>" | @html"#,
    "&lt;p style=&apos;visibility: hidden&apos;&gt;sneaky&lt;/p&gt;"
);
yields!(
    format_uri,
    r#""abc123 ?#+&[]" | @uri"#,
    "abc123%20%3F%23%2B%26%5B%5D"
);
yields!(
    format_csv,
    r#"[0, 0 == 0, {}.a, "hello \"quotes\" and, commas"] | @csv"#,
    r#"0,true,,"hello ""quotes"" and, commas""#
);
yields!(
    format_tsv,
    r#"[0, 0 == 0, {}.a, "hello \"quotes\" and \n\r\t\\ escapes"] | @tsv"#,
    "0\ttrue\t\thello \"quotes\" and \\n\\r\\t\\\\ escapes"
);
yields!(
    format_base64,
    r#""hello cruel world" | @base64"#,
    "aGVsbG8gY3J1ZWwgd29ybGQ="
);
yields!(
    format_unformat_base64,
    r#""hello cruel world" | @base64 | @base64d"#,
    "hello cruel world"
);
yields!(
    format_sh,
    r#"[0, 0 == 0, {}.a, "O'Hara!", ["Here", "there"] | @sh]"#,
    ["0", "true", "null", r#"'O'\''Hara!'"#, r#"'Here' 'there'"#,]
);
yields!(
    format_sh_rejects_objects,
    r#"{a: "b"} | try @sh catch -1"#,
    -1
);
yields!(
    format_sh_rejects_nested_arrays,
    r#"["fine, but", []] | try @sh catch -1"#,
    -1
);

#[test]
fn group_by() {
    gives(json!([]), "group_by(.)", [json!([])]);
    gives(
        json!([{"key":1, "value": "foo"},{"key":2, "value":"bar"},{"key":1,"value":"baz"}]),
        "group_by(.key)",
        [json!([[{"key":1,"value":"foo"}, {"key":1,"value":"baz"}],[{"key":2,"value":"bar"}]])],
    );
}

#[test]
fn has() {
    let err = Error::Index(Val::Null, Val::Int(0));
    fail(json!(null), "has(0)", err);
    let err = Error::Index(Val::Int(0), Val::Null);
    fail(json!(0), "has([][0])", err);
    let err = Error::Index(Val::Int(0), Val::Int(1));
    fail(json!(0), "has(1)", err);
    let err = Error::Index(Val::Str("a".to_string().into()), Val::Int(0));
    fail(json!("a"), "has(0)", err);

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

#[test]
fn json() {
    // TODO: correct this
    give(json!(1.0), "tojson", json!("1.0"));
    give(json!(0), "1.0 | tojson", json!("1.0"));
    give(json!(0), "1.1 | tojson", json!("1.1"));
    give(json!(0), "0.0 / 0.0 | tojson", json!("null"));
    give(json!(0), "1.0 / 0.0 | tojson", json!("null"));
}

#[test]
fn keys_unsorted() {
    give(json!([0, null, "a"]), "keys_unsorted", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "keys_unsorted", json!(["a", "b"]));

    let err = |v| Error::Type(v, Type::Iter);
    fail(json!(0), "keys_unsorted", err(Val::Int(0)));
    fail(json!(null), "keys_unsorted", err(Val::Null));
}

yields!(length_str_foo, r#""ƒoo" | length"#, 3);
yields!(length_str_namaste, r#""नमस्ते" | length"#, 6);
yields!(length_obj, r#"{"a": 5, "b": 3} | length"#, 2);
yields!(length_int_pos, " 2 | length", 2);
yields!(length_int_neg, "-2 | length", 2);
yields!(length_float_pos, " 2.5 | length", 2.5);
yields!(length_float_neg, "-2.5 | length", 2.5);

yields!(utf8bytelength_foo1, r#""foo" | utf8bytelength"#, 3);
yields!(utf8bytelength_foo2, r#""ƒoo" | utf8bytelength"#, 4);
yields!(utf8bytelength_namaste, r#""नमस्ते" | utf8bytelength"#, 18);

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(json!(null), "limit(0; 1,2)", []);
    give(json!(null), "[limit(1, 0, 3; 0, 1)]", json!([0, 0, 1]));

    // here, jaq diverges from jq, which returns `[0, 1]`
    give(json!(null), "[limit(-1; 0, 1)]", json!([]));
}

yields!(min_empty, "[] | min_by(.)", json!(null));
// when output is equal, min_by selects the left element and max_by the right one
yields!(
    min_max_eq,
    "[{a: 1, b: 3}, {a: 1, b: 2}] | [(min_by(.a), max_by(.a)) | .b]",
    [3, 2]
);
// multiple-output functions can be used to differentiate elements
yields!(
    max_mult,
    "[{a: 1, b: 3}, {a: 1, b: 2}] | max_by(.a, .b) | .b",
    3
);

yields!(
    math_0_argument_scalar_filters,
    "[-2.2, -1.1, 0, 1.1, 2.2 | sin as $s | cos as $c | $s * $s + $c * $c]",
    [1.0, 1.0, 1.0, 1.0, 1.0]
);

yields!(
    math_0_argument_vector_filters,
    "[3, 3.25, 3.5 | modf]",
    [[0.0, 3.0], [0.25, 3.0], [0.5, 3.0]]
);

yields!(
    math_2_argument_filters,
    "[pow(0.25, 4, 9; 1, 0.5, 2)]",
    [0.25, 0.5, 0.0625, 4.0, 2.0, 16.0, 9.0, 3.0, 81.0]
);

yields!(
    math_3_argument_filters,
    "[fma(2, 1; 3, 4; 4, 5)]",
    [10.0, 11.0, 12.0, 13.0, 7.0, 8.0, 8.0, 9.0]
);

yields!(range_pp, "[range(0; 6;  2)]", [0, 2, 4]);
yields!(range_pn, "[range(0; 6; -2)]", json!([]));
yields!(range_np, "[range(0; -6; 2)]", json!([]));
yields!(range_nn, "[range(0; -6; -2)]", [0, -2, -4]);
yields!(range_zz, "[range(0; 0; 0)]", json!([]));
yields!(range_fp, "[range(0.0; 2; 0.5)]", [0.0, 0.5, 1.0, 1.5]);
yields!(range_ip, "[limit(3; range(0; 1/0; 1))]", [0, 1, 2]);
yields!(range_in, "[limit(3; range(0; -1/0; -1))]", [0, -1, -2]);
// here, we diverge from jq, which just returns the empty list
yields!(range_pz, "[limit(3; range(0; 6; 0))]", json!([0, 0, 0]));
yields!(range_nz, "[limit(3; range(0; -6; 0))]", json!([0, 0, 0]));

#[test]
fn regex() {
    let date = r#"(\\d{4})-(\\d{2})-(\\d{2})"#;
    let s = "2012-03-14, 2013-01-01 and 2014-07-05";
    let f = |f, re, flags| format!("{f}(\"{re}\"; \"{flags}\")");

    let out = json!(["", ", ", " and ", ""]);
    give(json!(s), &f("split_", date, "g"), out);

    let c = |o: usize, s: &str| {
        json!({
          "offset": o,
          "length": s.chars().count(),
          "string": s
        })
    };
    let d1 = json!([c(00, "2012-03-14"), c(00, "2012"), c(05, "03"), c(08, "14")]);
    let d2 = json!([c(12, "2013-01-01"), c(12, "2013"), c(17, "01"), c(20, "01")]);
    let d3 = json!([c(27, "2014-07-05"), c(27, "2014"), c(32, "07"), c(35, "05")]);

    give(json!(s), &f("matches", date, "g"), json!([d1, d2, d3]));

    let out = json!(["", d1, ", ", d2, " and ", d3, ""]);
    give(json!(s), &f("split_matches", date, "g"), out);

    let out = json!(["", d1, ", 2013-01-01 and 2014-07-05"]);
    give(json!(s), &f("split_matches", date, ""), out);
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

    let err = |v| Error::Type(Val::from(v), Type::Num);
    fail(json!([]), "round", err(json!([])));
    fail(json!({}), "round", err(json!({})));
}

#[test]
fn startswith() {
    give(json!("foobar"), r#"startswith("")"#, json!(true));
    give(json!("foobar"), r#"startswith("bar")"#, json!(false));
    give(json!("foobar"), r#"startswith("foo")"#, json!(true));
    give(json!(""), r#"startswith("foo")"#, json!(false));
}

#[test]
fn endswith() {
    give(json!("foobar"), r#"endswith("")"#, json!(true));
    give(json!("foobar"), r#"endswith("foo")"#, json!(false));
    give(json!("foobar"), r#"endswith("bar")"#, json!(true));
    give(json!(""), r#"endswith("foo")"#, json!(false));
}

#[test]
fn ltrimstr() {
    give(json!("foobar"), r#"ltrimstr("")"#, json!("foobar"));
    give(json!("foobar"), r#"ltrimstr("foo")"#, json!("bar"));
    give(json!("foobar"), r#"ltrimstr("bar")"#, json!("foobar"));
    give(json!("اَلْعَرَبِيَّةُ"), r#"ltrimstr("ا")"#, json!("َلْعَرَبِيَّةُ"));
}

#[test]
fn rtrimstr() {
    give(json!("foobar"), r#"rtrimstr("")"#, json!("foobar"));
    give(json!("foobar"), r#"rtrimstr("bar")"#, json!("foo"));
    give(json!("foobar"), r#"rtrimstr("foo")"#, json!("foobar"));
    give(json!("اَلْعَرَبِيَّةُ"), r#"rtrimstr("ا")"#, json!("اَلْعَرَبِيَّةُ"));
}
