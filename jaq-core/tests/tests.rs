//! Tests for named core filters, sorted by name.

pub mod common;

use common::{fail, give, gives, UNSTABLE};
use jaq_interpret::error::{Error, Type};
use jaq_interpret::Val;
use serde_json::json;

yields!(
    repeat,
    UNSTABLE,
    "def r(f): f, r(f); [limit(3; r(1, 2))]",
    [1, 2, 1]
);

yields!(lazy_array, UNSTABLE, "def f: 1, [f]; limit(1; f)", 1);

yields!(
    lazy_foreach,
    UNSTABLE,
    "def f: f; limit(1; foreach (1, f) as $x (0; .))",
    0
);

yields!(
    nested_rec,
    UNSTABLE,
    "def f: def g: 0, g; g; def h: h; first(f)",
    0
);

yields!(
    rec_two_var_args,
    UNSTABLE,
    "def f($a; $b): [$a, $b], f($a+1; $b+1); [limit(3; f(0; 1))]",
    [[0, 1], [1, 2], [2, 3]]
);

yields!(
    rec_update,
    UNSTABLE,
    "def upto($x): .[$x], (if $x > 0 then upto($x-1) else empty end); [1, 2, 3, 4] | upto(1) |= .+1",
    [2, 3, 3, 4]
);

#[test]
fn ascii() {
    give(UNSTABLE, json!("aAaAäの"), "ascii_upcase", json!("AAAAäの"));
    give(
        UNSTABLE,
        json!("aAaAäの"),
        "ascii_downcase",
        json!("aaaaäの"),
    );
}

#[test]
fn dateiso8601() {
    give(
        UNSTABLE,
        json!("1970-01-02T00:00:00Z"),
        "fromdateiso8601",
        json!(86400),
    );
    give(
        UNSTABLE,
        json!("1970-01-02T00:00:00.123456789Z"),
        "fromdateiso8601",
        json!(86400.123456789),
    );
    give(
        UNSTABLE,
        json!(86400),
        "todateiso8601",
        json!("1970-01-02T00:00:00Z"),
    );
    give(
        UNSTABLE,
        json!(86400.123456789),
        "todateiso8601",
        json!("1970-01-02T00:00:00.123456789Z"),
    );
}

#[test]
fn explode_implode() {
    give(
        UNSTABLE,
        json!("❤ の"),
        "explode",
        json!([10084, 32, 12398]),
    );
    give(UNSTABLE, json!("y̆"), "explode", json!([121, 774]));

    give(UNSTABLE, json!("❤ の"), "explode | implode", json!("❤ の"));
    give(UNSTABLE, json!("y̆"), "explode | implode", json!("y̆"));

    give(
        UNSTABLE,
        json!([1114112]),
        "try implode catch -1",
        json!(-1),
    );
}

yields!(first_empty, UNSTABLE, "[first({}[])]", json!([]));
yields!(first_some, UNSTABLE, "first(1, 2, 3)", 1);

yields!(
    format_text,
    UNSTABLE,
    r#"[0, 0 == 0, {}.a, "hello", {}, [] | @text]"#,
    ["0", "true", "null", "hello", "{}", "[]"]
);
yields!(
    format_json,
    UNSTABLE,
    r#"[0, 0 == 0, {}.a, "hello", {}, [] | @json]"#,
    ["0", "true", "null", "\"hello\"", "{}", "[]"]
);
yields!(
    format_html,
    UNSTABLE,
    r#""<p style='visibility: hidden'>sneaky</p>" | @html"#,
    "&lt;p style=&apos;visibility: hidden&apos;&gt;sneaky&lt;/p&gt;"
);
yields!(
    format_uri,
    UNSTABLE,
    r#""abc123 ?#+&[]" | @uri"#,
    "abc123%20%3F%23%2B%26%5B%5D"
);
yields!(
    format_csv,
    UNSTABLE,
    r#"[0, 0 == 0, {}.a, "hello \"quotes\" and, commas"] | @csv"#,
    r#"0,true,,"hello ""quotes"" and, commas""#
);
yields!(
    format_tsv,
    UNSTABLE,
    r#"[0, 0 == 0, {}.a, "hello \"quotes\" and \n\r\t\\ escapes"] | @tsv"#,
    "0\ttrue\t\thello \"quotes\" and \\n\\r\\t\\\\ escapes"
);
yields!(
    format_base64,
    UNSTABLE,
    r#""hello cruel world" | @base64"#,
    "aGVsbG8gY3J1ZWwgd29ybGQ="
);
yields!(
    format_unformat_base64,
    UNSTABLE,
    r#""hello cruel world" | @base64 | @base64d"#,
    "hello cruel world"
);
yields!(
    format_sh,
    UNSTABLE,
    r#"[0, 0 == 0, {}.a, "O'Hara!", ["Here", "there"] | @sh]"#,
    ["0", "true", "null", r#"'O'\''Hara!'"#, r#"'Here' 'there'"#,]
);
yields!(
    format_sh_rejects_objects,
    UNSTABLE,
    r#"{a: "b"} | try @sh catch -1"#,
    -1
);
yields!(
    format_sh_rejects_nested_arrays,
    UNSTABLE,
    r#"["fine, but", []] | try @sh catch -1"#,
    -1
);

#[test]
fn group_by() {
    gives(UNSTABLE, json!([]), "group_by(.)", [json!([])]);
    gives(
        UNSTABLE,
        json!([{"key":1, "value": "foo"},{"key":2, "value":"bar"},{"key":1,"value":"baz"}]),
        "group_by(.key)",
        [json!([[{"key":1,"value":"foo"}, {"key":1,"value":"baz"}],[{"key":2,"value":"bar"}]])],
    );
}

#[test]
fn has() {
    let err = Error::Index(Val::Null, Val::Int(0));
    fail(UNSTABLE, json!(null), "has(0)", err);
    let err = Error::Index(Val::Int(0), Val::Null);
    fail(UNSTABLE, json!(0), "has([][0])", err);
    let err = Error::Index(Val::Int(0), Val::Int(1));
    fail(UNSTABLE, json!(0), "has(1)", err);
    let err = Error::Index(Val::Str("a".to_string().into()), Val::Int(0));
    fail(UNSTABLE, json!("a"), "has(0)", err);

    give(UNSTABLE, json!([0, null]), "has(0)", json!(true));
    give(UNSTABLE, json!([0, null]), "has(1)", json!(true));
    give(UNSTABLE, json!([0, null]), "has(2)", json!(false));

    give(
        UNSTABLE,
        json!({"a": 1, "b": null}),
        r#"has("a")"#,
        json!(true),
    );
    give(
        UNSTABLE,
        json!({"a": 1, "b": null}),
        r#"has("b")"#,
        json!(true),
    );
    give(
        UNSTABLE,
        json!({"a": 1, "b": null}),
        r#"has("c")"#,
        json!(false),
    );
}

#[test]
fn json() {
    // TODO: correct this
    give(UNSTABLE, json!(1.0), "tojson", json!("1.0"));
    give(UNSTABLE, json!(0), "1.0 | tojson", json!("1.0"));
    give(UNSTABLE, json!(0), "1.1 | tojson", json!("1.1"));
    give(UNSTABLE, json!(0), "0.0 / 0.0 | tojson", json!("null"));
    give(UNSTABLE, json!(0), "1.0 / 0.0 | tojson", json!("null"));
}

#[test]
fn keys_unsorted() {
    give(
        UNSTABLE,
        json!([0, null, "a"]),
        "keys_unsorted",
        json!([0, 1, 2]),
    );
    give(
        UNSTABLE,
        json!({"a": 1, "b": 2}),
        "keys_unsorted",
        json!(["a", "b"]),
    );

    let err = |v| Error::Type(v, Type::Iter);
    fail(UNSTABLE, json!(0), "keys_unsorted", err(Val::Int(0)));
    fail(UNSTABLE, json!(null), "keys_unsorted", err(Val::Null));
}

#[test]
fn length() {
    give(UNSTABLE, json!("ƒoo"), "length", json!(3));
    give(UNSTABLE, json!("नमस्ते"), "length", json!(6));
    give(UNSTABLE, json!({"a": 5, "b": 3}), "length", json!(2));
    give(UNSTABLE, json!(2), "length", json!(2));
    give(UNSTABLE, json!(-2), "length", json!(2));
    give(UNSTABLE, json!(2.5), "length", json!(2.5));
    give(UNSTABLE, json!(-2.5), "length", json!(2.5));
}

#[test]
fn utf8bytelength() {
    give(UNSTABLE, json!("foo"), "utf8bytelength", json!(3));
    give(UNSTABLE, json!("ƒoo"), "utf8bytelength", json!(4));
    give(UNSTABLE, json!("नमस्ते"), "utf8bytelength", json!(18));
}

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(UNSTABLE, json!(null), "limit(0; 1,2)", []);
    give(
        UNSTABLE,
        json!(null),
        "[limit(1, 0, 3; 0, 1)]",
        json!([0, 0, 1]),
    );

    // here, jaq diverges from jq, which returns `[0, 1]`
    give(UNSTABLE, json!(null), "[limit(-1; 0, 1)]", json!([]));
}

yields!(min_empty, UNSTABLE, "[] | min_by(.)", json!(null));
// when output is equal, min_by selects the left element and max_by the right one
yields!(
    min_max_eq,
    UNSTABLE,
    "[{a: 1, b: 3}, {a: 1, b: 2}] | [(min_by(.a), max_by(.a)) | .b]",
    [3, 2]
);
// multiple-output functions can be used to differentiate elements
yields!(
    max_mult,
    UNSTABLE,
    "[{a: 1, b: 3}, {a: 1, b: 2}] | max_by(.a, .b) | .b",
    3
);

yields!(
    math_0_argument_scalar_filters,
    UNSTABLE,
    "[-2.2, -1.1, 0, 1.1, 2.2 | sin as $s | cos as $c | $s * $s + $c * $c]",
    [1.0, 1.0, 1.0, 1.0, 1.0]
);

yields!(
    math_0_argument_vector_filters,
    UNSTABLE,
    "[3, 3.25, 3.5 | modf]",
    [[0.0, 3.0], [0.25, 3.0], [0.5, 3.0]]
);

yields!(
    math_2_argument_filters,
    UNSTABLE,
    "[pow(0.25, 4, 9; 1, 0.5, 2)]",
    [0.25, 0.5, 0.0625, 4.0, 2.0, 16.0, 9.0, 3.0, 81.0]
);

yields!(
    math_3_argument_filters,
    UNSTABLE,
    "[fma(2, 1; 3, 4; 4, 5)]",
    [10.0, 11.0, 12.0, 13.0, 7.0, 8.0, 8.0, 9.0]
);

#[test]
fn regex() {
    let date = r#"(\\d{4})-(\\d{2})-(\\d{2})"#;
    let s = "2012-03-14, 2013-01-01 and 2014-07-05";
    let f = |f, re, flags| format!("{f}(\"{re}\"; \"{flags}\")");

    let out = json!(["", ", ", " and ", ""]);
    give(UNSTABLE, json!(s), &f("split_", date, "g"), out);

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

    give(
        UNSTABLE,
        json!(s),
        &f("matches", date, "g"),
        json!([d1, d2, d3]),
    );

    let out = json!(["", d1, ", ", d2, " and ", d3, ""]);
    give(UNSTABLE, json!(s), &f("split_matches", date, "g"), out);

    let out = json!(["", d1, ", 2013-01-01 and 2014-07-05"]);
    give(UNSTABLE, json!(s), &f("split_matches", date, ""), out);
}

#[test]
fn round() {
    give(UNSTABLE, json!(1), "round", json!(1));
    give(UNSTABLE, json!(1.0), "round", json!(1));
    give(UNSTABLE, json!(-1.0), "round", json!(-1));
    give(UNSTABLE, json!(-1), "round", json!(-1));

    give(UNSTABLE, json!(-1.5), "round", json!(-2));
    give(UNSTABLE, json!(-1.5), "floor", json!(-2));
    give(UNSTABLE, json!(-1.5), "ceil", json!(-1));

    give(UNSTABLE, json!(-1.4), "round", json!(-1));
    give(UNSTABLE, json!(-1.4), "floor", json!(-2));
    give(UNSTABLE, json!(-1.4), "ceil", json!(-1));

    let err = |v| Error::Type(Val::from(v), Type::Num);
    fail(UNSTABLE, json!([]), "round", err(json!([])));
    fail(UNSTABLE, json!({}), "round", err(json!({})));
}

#[test]
fn split() {
    give(
        UNSTABLE,
        json!("aöß"),
        r#"split("")"#,
        json!(["a", "ö", "ß"]),
    );
    give(
        UNSTABLE,
        json!("abcabcdab"),
        r#"split("ab")"#,
        json!(["", "c", "cd", ""]),
    );
}

#[test]
fn startswith() {
    give(UNSTABLE, json!("foobar"), r#"startswith("")"#, json!(true));
    give(
        UNSTABLE,
        json!("foobar"),
        r#"startswith("bar")"#,
        json!(false),
    );
    give(
        UNSTABLE,
        json!("foobar"),
        r#"startswith("foo")"#,
        json!(true),
    );
    give(UNSTABLE, json!(""), r#"startswith("foo")"#, json!(false));
}

#[test]
fn endswith() {
    give(UNSTABLE, json!("foobar"), r#"endswith("")"#, json!(true));
    give(
        UNSTABLE,
        json!("foobar"),
        r#"endswith("foo")"#,
        json!(false),
    );
    give(UNSTABLE, json!("foobar"), r#"endswith("bar")"#, json!(true));
    give(UNSTABLE, json!(""), r#"endswith("foo")"#, json!(false));
}

#[test]
fn ltrimstr() {
    give(
        UNSTABLE,
        json!("foobar"),
        r#"ltrimstr("")"#,
        json!("foobar"),
    );
    give(
        UNSTABLE,
        json!("foobar"),
        r#"ltrimstr("foo")"#,
        json!("bar"),
    );
    give(
        UNSTABLE,
        json!("foobar"),
        r#"ltrimstr("bar")"#,
        json!("foobar"),
    );
    give(
        UNSTABLE,
        json!("اَلْعَرَبِيَّةُ"),
        r#"ltrimstr("ا")"#,
        json!("َلْعَرَبِيَّةُ"),
    );
}

#[test]
fn rtrimstr() {
    give(
        UNSTABLE,
        json!("foobar"),
        r#"rtrimstr("")"#,
        json!("foobar"),
    );
    give(
        UNSTABLE,
        json!("foobar"),
        r#"rtrimstr("bar")"#,
        json!("foo"),
    );
    give(
        UNSTABLE,
        json!("foobar"),
        r#"rtrimstr("foo")"#,
        json!("foobar"),
    );
    give(
        UNSTABLE,
        json!("اَلْعَرَبِيَّةُ"),
        r#"rtrimstr("ا")"#,
        json!("اَلْعَرَبِيَّةُ"),
    );
}
