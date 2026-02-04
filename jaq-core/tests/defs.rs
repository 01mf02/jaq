//! Tests for filters in the standard library, sorted by name.

pub mod common;

use common::{give, gives};
use serde_json::json;

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

yields!(join_empty, r#"[] | join(" ")"#, "");
yields!(
    join_strs,
    r#"["Hello", "world"] | join(" ")"#,
    "Hello world"
);
yields!(join_nums, r#"[2, 3, 4, 5] | join(",")"#, "2,3,4,5");

yields!(map, "[1, 2] | map(.+1)", [2, 3]);

// this diverges from jq, which returns [null]
yields!(last_empty, "[last({}[])]", json!([]));
yields!(last_some, "last(1, 2, 3)", 3);

yields!(range_many, "[range(-1, 1; 0, 2)]", json!([-1, -1, 0, 1, 1]));

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
fn recurse_obj() {
    let x = json!({"a":0,"b":[1]});
    gives(x.clone(), "recurse", [x, json!(0), json!([1]), json!(1)]);

    let y = [json!(1), json!(2), json!(3)];
    gives(json!(1), "recurse(.+1; . < 4)", y);

    let y = [json!(2), json!(4), json!(16)];
    gives(json!(2), "recurse(. * .; . < 20)", y);
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

yields!(tostring_zero, "1.0 - 1.0 | tostring", "0.0");
yields!(tostring_str, r#""\n" | tostring"#, "\n");
yields!(tostring_arr_str, r#"["\n"] | tostring"#, "[\"\\n\"]");
