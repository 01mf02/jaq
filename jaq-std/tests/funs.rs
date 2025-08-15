//! Tests for named core filters, sorted by name.

pub mod common;

use common::{fail, give, gives, Error, Val};
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
    "def upto($x): .[$x], (if $x > 0 then upto($x-1) else {}[] as $x | . end); [1, 2, 3, 4] | upto(1) |= .+1",
    [2, 3, 3, 4]
);

#[test]
fn ascii() {
    give(json!("aAaAäの"), "ascii_upcase", json!("AAAAäの"));
    give(json!("aAaAäの"), "ascii_downcase", json!("aaaaäの"));
}

yields!(
    fromdate,
    r#""1970-01-02T00:00:00Z" | fromdateiso8601"#,
    86400
);
yields!(
    fromdate_mu,
    r#""1970-01-02T00:00:00.123456Z" | fromdateiso8601"#,
    86400.123456
);
yields!(todate, r#"86400 | todateiso8601"#, "1970-01-02T00:00:00Z");
yields!(
    todate_mu,
    "86400.123456 | todateiso8601",
    "1970-01-02T00:00:00.123456Z"
);
yields!(
    strftime,
    r#"86400 | strftime("%F %T")"#,
    "1970-01-02 00:00:00"
);
yields!(
    strftime_arr,
    r#"[ 1970, 0, 2, 0, 0, 0, 5, 1 ] | strftime("%F %T")"#,
    "1970-01-02 00:00:00"
);
yields!(
    strftime_mu,
    r#"86400.123456 | strftime("%F %T.%6f")"#,
    "1970-01-02 00:00:00.123456"
);
yields!(gmtime, r"86400 | gmtime", [1970, 0, 2, 0, 0, 0, 5, 1]);
yields!(
    gmtime_mu,
    r"86400.123456 | gmtime",
    json!([1970, 0, 2, 0, 0, 0.123456, 5, 1])
);
yields!(
    gmtime_mktime_mu,
    r"86400.123456 | gmtime | mktime",
    86400.123456
);
yields!(
    strptime,
    r#""1970-01-02T00:00:00Z" | strptime("%Y-%m-%dT%H:%M:%SZ")"#,
    [1970, 0, 2, 0, 0, 0, 5, 1]
);
yields!(mktime, "[ 1970, 0, 2, 0, 0, 0, 5, 1 ] | mktime", 86400);

#[test]
fn fromtodate() {
    let fromto = "fromdateiso8601 | todateiso8601";
    let iso = "2000-01-01T00:00:00Z";
    give(json!(iso), fromto, json!(iso));
    let iso_mu = "2000-01-01T00:00:00.123456Z";
    give(json!(iso_mu), fromto, json!(iso_mu));
}

#[test]
fn explode_implode() {
    give(json!("❤ の"), "explode", json!([10084, 32, 12398]));
    give(json!("y̆"), "explode", json!([121, 774]));

    give(json!("❤ の"), "explode | implode", json!("❤ の"));
    give(json!("y̆"), "explode | implode", json!("y̆"));
}

yields!(implode_invalid, "[1114112] | try implode catch -1", -1);

yields!(first_empty, "[first({}[])]", json!([]));
yields!(first_some, "first(1, 2, 3)", 1);

yields!(
    encode_base64,
    r#""hello cruel world" | encode_base64"#,
    "aGVsbG8gY3J1ZWwgd29ybGQ="
);
yields!(
    encode_decode_base64,
    r#""hello cruel world" | encode_base64 | decode_base64"#,
    "hello cruel world"
);

yields!(
    escape_html,
    r#""<p style='visibility: hidden'>sneaky</p>" | escape_html"#,
    "&lt;p style=&apos;visibility: hidden&apos;&gt;sneaky&lt;/p&gt;"
);
yields!(
    encode_uri,
    r#""abc123 ?#+&[]" | encode_uri"#,
    "abc123%20%3F%23%2B%26%5B%5D"
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
fn keys_unsorted() {
    give(json!([0, null, "a"]), "keys_unsorted", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "keys_unsorted", json!(["a", "b"]));

    let err = |v| Error::typ(v, "iterable (array or object)");
    fail(json!(0), "keys_unsorted", err(Val::from(0)));
    fail(json!(null), "keys_unsorted", err(Val::Null));
}

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

yields!(limit_overflow, "[limit(0; def f: f | .; f)]", json!([]));

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

    give(json!(""), &f("matches", "", ""), json!([[c(0, "")]]));
    give(json!(""), &f("matches", "^$", ""), json!([[c(0, "")]]));
    give(
        json!("  "),
        &f("matches", "", "g"),
        json!([[c(0, "")], [c(1, "")], [c(2, "")]]),
    );
    give(json!("  "), &f("matches", "", "gn"), json!([]));

    let out = json!(["", d1, ", ", d2, " and ", d3, ""]);
    give(json!(s), &f("split_matches", date, "g"), out);

    let out = json!(["", d1, ", 2013-01-01 and 2014-07-05"]);
    give(json!(s), &f("split_matches", date, ""), out);
}

yields!(round_int, "[0, 1][1 | round]", 1);

yields!(round_pi, " 1   | round", 1);
yields!(round_pf, " 1.0 | round", 1);
yields!(round_ni, "-1   | round", -1);
yields!(round_nf, "-1.0 | round", -1);

yields!(round_mid, "-1.5 | round", -2);
yields!(floor_mid, "-1.5 | floor", -2);
yields!(ceili_mid, "-1.5 | ceil ", -1);

yields!(round_floor, "-1.4 | round", -1);
yields!(floor_floor, "-1.4 | floor", -2);
yields!(ceili_floor, "-1.4 | ceil ", -1);

yields!(round_nan, "nan | round | isnan", true);
yields!(round_inf, "infinite | round | isinfinite", true);
yields!(
    round_large,
    "2e22 | round | tostring",
    "20000000000000000000000"
);

yields!(
    sort_break_out,
    "[1, 2] | (label $x | sort_by(label $y | ., break $x)), 3",
    3
);
yields!(
    sort_break_in,
    "[1, 2] | label $x | sort_by(label $y | ., break $y)",
    [1, 2]
);

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

#[test]
fn trim() {
    give(json!(""), "trim", json!(""));
    give(json!(" "), "trim", json!(""));
    give(json!("  "), "trim", json!(""));
    give(json!("foo"), "trim", json!("foo"));
    give(json!(" foo  "), "trim", json!("foo"));
    give(json!("  foo  "), "trim", json!("foo"));
    give(json!("foo  "), "trim", json!("foo"));
    give(json!(" اَلْعَرَبِيَّةُ "), "trim", json!("اَلْعَرَبِيَّةُ"));
}

#[test]
fn ltrim() {
    give(json!(""), "ltrim", json!(""));
    give(json!(" "), "ltrim", json!(""));
    give(json!("  "), "ltrim", json!(""));
    give(json!("foo"), "ltrim", json!("foo"));
    give(json!(" foo  "), "ltrim", json!("foo  "));
    give(json!("  foo  "), "ltrim", json!("foo  "));
    give(json!("foo  "), "ltrim", json!("foo  "));
    give(json!(" اَلْعَرَبِيَّةُ "), "ltrim", json!("اَلْعَرَبِيَّةُ "));
}

#[test]
fn rtrim() {
    give(json!(""), "rtrim", json!(""));
    give(json!(" "), "rtrim", json!(""));
    give(json!("  "), "rtrim", json!(""));
    give(json!("foo"), "rtrim", json!("foo"));
    give(json!("  foo "), "rtrim", json!("  foo"));
    give(json!("  foo  "), "rtrim", json!("  foo"));
    give(json!("  foo"), "rtrim", json!("  foo"));
    give(json!(" اَلْعَرَبِيَّةُ "), "rtrim", json!(" اَلْعَرَبِيَّةُ"));
}

yields!(
    path_values,
    "[{a: 1, b: [2, 3]} | path_values]",
    json!([[["a"], 1], [["b"], [2, 3]], [["b", 0], 2], [["b", 1], 3]])
);
