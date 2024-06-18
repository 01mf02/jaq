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
fn date() {
    // aliases for fromdateiso8601 and todateiso8601
    give(json!("1970-01-02T00:00:00Z"), "fromdate", json!(86400));
    give(
        json!("1970-01-02T00:00:00.123456789Z"),
        "fromdate",
        json!(86400.123456789),
    );
    give(json!(86400), "todate", json!("1970-01-02T00:00:00Z"));
    give(
        json!(86400.123456789),
        "todate",
        json!("1970-01-02T00:00:00.123456789Z"),
    );
}

#[test]
fn date_roundtrip() {
    let epoch = 946684800;
    give(json!(epoch), "todate|fromdate", json!(epoch));
    let epoch_ns = 946684800.123456;
    give(json!(epoch_ns), "todate|fromdate", json!(epoch_ns));

    let iso = "2000-01-01T00:00:00Z";
    give(json!(iso), "fromdate|todate", json!(iso));
    let iso_ns = "2000-01-01T00:00:00.123456000Z";
    give(json!(iso_ns), "fromdate|todate", json!(iso_ns));
}

yields!(
    drem_nan,
    "[drem(nan, 1; nan, 1)] == [nan, nan, nan, 0.0]",
    true
);
yields!(
    drem_range,
    "[drem(3.5, -4; 6, 1.75, 2)]",
    [-2.5, 0.0, -0.5, 2.0, -0.5, -0.0]
);

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

// this diverges from jq, which returns [null]
yields!(last_empty, "[last({}[])]", json!([]));
yields!(last_some, "last(1, 2, 3)", 3);

yields!(logb_inf, "infinite | logb | . == infinite", true);
yields!(logb_nan, "nan | logb | isnan", true);
yields!(logb_neg_inf, "-infinite | logb | . == infinite", true);
yields!(
    logb_range,
    "[-2.2, -2, -1, 1, 2, 2.2] | map(logb)",
    [1.0, 1.0, 0.0, 0.0, 1.0, 1.0]
);
yields!(logb_zero, "0 | logb | . == -infinite", true);

// here we diverge from jq, which returns ["a", "b", "A", "B"]
yields!(
    match_many,
    r#""ABab" | [match("a", "b"; "", "i") | .string]"#,
    ["a", "A", "b", "B"]
);

#[test]
fn min_max() {
    give(json!([1, 4, 2]), "min", json!(1));
    give(json!([1, 4, 2]), "max", json!(4));
    // TODO: find examples where `min_by(f)` yields output different from `min`
    // (and move it then to jaq-core/tests/tests.rs)
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

yields!(range_many, "[range(-1, 1; 0, 2)]", json!([-1, -1, 0, 1, 1]));

#[test]
fn range_reverse() {
    give(json!(null), "[range(1, 2)]", json!([0, 0, 1]));

    give(json!(3), "[range(.)] | reverse", json!([2, 1, 0]));
}

yields!(
    recurse_update,
    "[0, [1, 2], 3] | recurse |= (.+1)? // .",
    json!([1, [2, 3], 4])
);

// the following tests show that sums are evaluated lazily
// (otherwise this would not terminate)
yields!(limit_inf_suml, "[limit(3; recurse(.+1) + 0)]", [0, 1, 2]);
yields!(limit_inf_sumr, "[limit(3; 0 + recurse(.+1))]", [0, 1, 2]);

yields!(limit_inf_path, "[limit(2; [1] | .[repeat(0)])]", [1, 1]);

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

yields!(
    recurse3,
    "[1 | recurse(if . < 3 then .+1 else empty end)]",
    [1, 2, 3]
);
yields!(
    reduce_recurse,
    "reduce recurse(if . == 1000 then empty else .+1 end) as $x (0; . + $x)",
    500500
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

const RECURSE_FLATTEN: &str = "def flatten($d):
  [ { d: $d, x: . } |
    recurse(if .d >= 0 and ([] <= .x and .x < {}) then { d: .d - 1, x: .x[] } else empty end) |
    if .d < 0 or (.x < [] or {} <= .x) then .x else empty end
  ];";

yields!(
    recurse_flatten,
    &(RECURSE_FLATTEN.to_owned() + "[[[1], 2], 3] | flatten(1)"),
    json!([[1], 2, 3])
);

#[test]
fn repeat() {
    let y = json!([0, 1, 0, 1]);
    give(json!([0, 1]), "[limit(4; repeat(.[]))]", y);
}

// the implementation of scalb in jq (or the libm.a library) doesn't
// allow for float exponents; jaq tolerates float exponents in scalb
// and rejects them in scalbln
yields!(
    scalb_eqv_pow2,
    "[-2.2, -1.1, -0.01, 0, 0.01, 1.1, 2.2] | [scalb(1.0; .[])] == [pow(2.0; .[])]",
    true
);
yields!(
    scalb_nan,
    "[scalb(nan, 1; nan, 1)] == [nan, nan, nan, 2.0]",
    true
);
yields!(
    scalb_range,
    "[scalb(-2.5, 0, 2.5; 2, 2.5, 3) * 1000 | round]",
    [-10000, -14142, -20000, 0, 0, 0, 10000, 14142, 20000]
);

// here we diverge from jq, which returns ["a", "b", "a", "A", "b", "B"]
yields!(
    scan,
    r#""abAB" | [scan("a", "b"; "g", "gi")]"#,
    // TODO: is this order really desired?
    json!(["a", "a", "A", "b", "b", "B"])
);

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

yields!(
    significand_inf,
    "infinite | significand | . == infinite",
    true
);
yields!(significand_nan, "nan | significand | isnan", true);
yields!(
    significand_neg_inf,
    "-infinite | significand | . == -infinite",
    true
);
yields!(
    significand_range,
    "[-123.456, -2.2, -2, -1, 0, 0.00001, 1, 2, 2.2, 123.456] | map(significand)",
    [-1.929, -1.1, -1.0, -1.0, 0.0, 1.31072, 1.0, 1.0, 1.1, 1.929]
);

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

yields!(sub, r#""XYxyXYxy" | sub("x";"Q")"#, "XYQyXYxy");
yields!(gsub, r#""XYxyXYxy" | gsub("x";"Q")"#, "XYQyXYQy");
yields!(isub, r#""XYxyXYxy" | sub("x";"Q";"i")"#, "QYxyXYxy");
yields!(gisub, r#""XYxyXYxy" | gsub("x";"Q";"i")"#, "QYQyQYQy");
// swap adjacent occurrences of upper- and lower-case characters
yields!(
    gsub_swap,
    r#""XYxyXYxy" | gsub("(?<upper>[A-Z])(?<lower>[a-z])"; .lower + .upper)"#,
    "XxYyXxYy"
);
// this diverges from jq, which yields ["XxYy", "!XxYy", "Xx!Yy", "!Xx!Yy"]
yields!(
    gsub_many,
    r#""XxYy" | [gsub("(?<upper>[A-Z])"; .upper, "!" + .upper)]"#,
    ["XxYy", "Xx!Yy", "!XxYy", "!Xx!Yy"]
);

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
    format_sh,
    r#"[0, 0 == 0, "O'Hara!", ["Here", "there"] | @sh]"#,
    ["0", "true", r#"'O'\''Hara!'"#, r#"'Here' 'there'"#,]
);
// here, we diverge from jq, which returns "null"
// jaq's behaviour is more consistent with other functions like @csv and @tsv,
// which also substitute null with an empty string instead of "null"
yields!(format_sh_null, r#"{}.a | @sh"#, "");

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
