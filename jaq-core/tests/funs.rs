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

yields!(first_empty, "[first({}[])]", json!([]));
yields!(first_some, "first(1, 2, 3)", 1);

#[test]
fn keys_unsorted() {
    give(json!([0, null, "a"]), "keys_unsorted", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "keys_unsorted", json!(["a", "b"]));

    let err = |v| Error::typ(v, "iterable (array or object)");
    fail(json!(0), "keys_unsorted", err(Val::from(0usize)));
    fail(json!(null), "keys_unsorted", err(Val::Null));
}

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(json!(null), "limit(0; 1,2)", []);
    give(json!(null), "[limit(1, 0, 3; 0, 1)]", json!([0, 0, 1]));

    // here, jaq diverges from jq, which returns `[0, 1]`
    give(json!(null), "[limit(-1; 0, 1)]", json!([]));
}

yields!(limit_overflow, "[limit(0; def f: f | .; f)]", json!([]));

yields!(limit_path, "[1, 2, 3] | [path(limit(2; .[]))]", [[0], [1]]);
yields!(skip_path, "[1, 2, 3] | [path(skip(1; .[]))]", [[1], [2]]);

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

yields!(
    path_value,
    "[{a: 1, b: [2, 3]} | skip(1; path_value(..))]",
    json!([[["a"], 1], [["b"], [2, 3]], [["b", 0], 2], [["b", 1], 3]])
);
