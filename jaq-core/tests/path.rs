pub mod common;

use common::{fail, give, gives, Error, Val};
use serde_json::json;

#[test]
fn index_access() {
    give(json!([0, 1, 2]), ".[-4]", json!(null));
    give(json!([0, 1, 2]), ".[-3]", json!(0));
    give(json!([0, 1, 2]), ".[-1]", json!(2));
    give(json!([0, 1, 2]), ".[ 0]", json!(0));
    give(json!([0, 1, 2]), ".[ 2]", json!(2));
    give(json!([0, 1, 2]), ".[ 3]", json!(null));

    give(json!({"a": 1}), ".a", json!(1));
    give(json!({"a": 1}), ".a?", json!(1));
    give(json!({"a": 1}), ".a ?", json!(1));
    give(json!({"a": 1}), r#"."a""#, json!(1));
    give(json!({"a": 1}), r#". "a""#, json!(1));
    give(json!({"a": 1}), r#".["a"]"#, json!(1));
    give(json!({"a": 1}), r#". ["a"]"#, json!(1));
    give(json!({"a_": 1}), ".a_", json!(1));
    give(json!({"_a": 1}), "._a", json!(1));
    give(json!({"_0": 1}), "._0", json!(1));

    // this diverges from jq, which fails here, because
    // jaq can create objects with non-string keys
    give(json!({"a": 1}), r#".[0]"#, json!(null));

    give(json!([0, 1, 2]), r#".["a", 0, 0 == 0]?"#, json!(0));
    give(json!([0, 1, 2]), r#".[3]?"#, json!(null));
    gives(json!("asdf"), ".[0]?", []);

    give(json!(1), "[1, 2, 3][.]", json!(2));

    gives(
        json!({"a": 1, "b": 2}),
        r#".["b", "a"]"#,
        [json!(2), json!(1)],
    );
}

#[test]
fn iter_access() {
    gives(json!([0, 1, 2]), ".[]", [json!(0), json!(1), json!(2)]);
    gives(json!({"a": [1, 2]}), ".a[]", [json!(1), json!(2)]);
    gives(json!({"a": 1, "b": 2}), ".[]", [json!(1), json!(2)]);
    // TODO: correct this
    //gives(json!({"b": 2, "a": 1}), ".[]", [json!(2), json!(1)]);
    gives(json!("asdf"), ".[]?", []);
}

#[test]
fn range_access() {
    give(json!("Möwe"), ".[1:-1]", json!("öw"));
    give(json!("नमस्ते"), ".[1:5]", json!("मस्त"));

    give(json!([0, 1, 2]), ".[-4:4]", json!([0, 1, 2]));
    give(json!([0, 1, 2]), ".[0:3]", json!([0, 1, 2]));
    give(json!([0, 1, 2]), ".[1:]", json!([1, 2]));
    give(json!([0, 1, 2]), ".[:-1]", json!([0, 1]));
    give(json!([0, 1, 2]), ".[1:0]", json!([]));
    give(json!([0, 1, 2]), ".[4:5]", json!([]));

    give(json!([0, 1, 2]), ".[0:2,3.14]?", json!([0, 1]));
}

#[test]
fn iter_assign() {
    give(json!([1, 2]), ".[] = .", json!([[1, 2], [1, 2]]));
    give(
        json!({"a": [1,2], "b": 3}),
        ".a[] = .b+.b",
        json!({"a": [6,6], "b": 3}),
    );
}

yields!(index_keyword, r#"{"if": 0} | .if"#, 0);
yields!(obj_keyword, "{if: 0} | .if", 0);

yields!(key_update1, "{} | .a  |= .+1", json!({"a": 1}));
yields!(key_update2, "{} | .a? |= .+1", json!({"a": 1}));

// `.[f]?` is *not* the same as `(.[f])?`
// (we're writing `0[]` to simulate `error` here)
yields!(index_opt_inner, "try .[0[]]? catch 1", 1);
yields!(index_opt_outer, "1, (.[0[]])?", 1);

#[test]
fn index_update() {
    give(json!({"a": 1}), ".b |= .", json!({"a": 1, "b": null}));
    give(json!({"a": 1}), ".b |= 1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1}), ".b |= .+1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1, "b": 2}), ".b |= {}[]", json!({"a": 1}));
    give(json!({"a": 1, "b": 2}), ".a += 1", json!({"a": 2, "b": 2}));

    give(json!([0, 1, 2]), ".[1] |= .+2", json!([0, 3, 2]));
    give(json!([0, 1, 2]), ".[-1,-1] |= {}[]", json!([0]));
    give(json!([0, 1, 2]), ".[ 0, 0] |= {}[]", json!([2]));

    /* TODO: reenable these tests
    use Error::IndexOutOfBounds as Oob;
    fail(json!([0, 1, 2]), ".[ 3] |=  3", Oob(3));
    fail(json!([0, 1, 2]), ".[-4] |= -1", Oob(-4));
    */

    give(json!([0, 1, 2]), r#".["a", 0]? |= .+1"#, json!([1, 1, 2]));
    give(json!([0, 1, 2]), r#".[3]? |= .+1"#, json!([0, 1, 2]));
    give(json!("asdf"), ".[0]? |= .+1", json!("asdf"));
}

#[test]
fn iter_update() {
    // precedence tests
    give(json!([]), ".[] |= . or 0", json!([]));
    gives(json!([]), ".[] |= .,.", [json!([]), json!([])]);
    give(json!([]), ".[] |= (.,.)", json!([]));
    give(json!([0]), ".[] |= .+1 | .+[2]", json!([1, 2]));
    // this yields a syntax error in jq, but it is consistent to permit this
    give(json!([[1]]), ".[] |= .[] |= .+1", json!([[2]]));
    // ditto
    give(json!([[1]]), ".[] |= .[] += 1", json!([[2]]));

    give(json!([1]), ".[] |= .+1", json!([2]));
    give(json!([[1]]), ".[][] |= .+1", json!([[2]]));
}

yields!(
    obj_iter_update,
    r#"{"a": 1, "b": 2} | .[] |= ((if .>1 then . else {}[] end) | .+1)"#,
    json!({"b": 3})
);
yields!(
    arr_iter_opt_update,
    r#"[[0, 1], "a"] | .[][]? |= .+1"#,
    json!([[1, 2], "a"])
);

#[test]
fn range_update() {
    give(json!([0, 1, 2]), ".[:2] |= [.[] | .+5]", json!([5, 6, 2]));
    give(json!([0, 1, 2]), ".[-2:-1] |= [5]+.", json!([0, 5, 1, 2]));
    give(
        json!([0, 1, 2]),
        ".[-2:-1,-1] |= [5,6]+.",
        json!([0, 5, 6, 5, 6, 1, 2]),
    );

    give(
        json!([0, 1, 2]),
        ".[:2,3.0]? |= [.[] | .+1]",
        json!([1, 2, 2]),
    );
}

// Test what happens when update filter returns multiple values.
// Watch out: here, jaq diverges frequently from jq;
// jq considers only the first value of the filter regardless of the updated value,
// whereas jaq may consider multiple values depending on the updated value.
// This behaviour is easier to implement and more flexible.
#[test]
fn update_mult() {
    // first the cases where jaq and jq agree
    give(json!({"a": 1}), ".a |= (.,.+1)", json!({"a": 1}));

    // jq returns null here
    gives(json!(1), ". |= {}[]", []);
    // jq returns just 1 here
    gives(json!(1), ". |= (.,.)", [json!(1), json!(1)]);
    // jq returns just [1] here
    give(json!([1]), ".[] |= (., .+1)", json!([1, 2]));
    // jq returns just [1,2] here
    give(json!([1, 3]), ".[] |= (., .+1)", json!([1, 2, 3, 4]));
    // here comes a huge WTF: jq returns [2,4] here
    // this is a known bug: <https://github.com/stedolan/jq/issues/2051>
    give(json!([1, 2, 3, 4, 5]), ".[] |= {}[]", json!([]));
}

#[test]
fn update_complex() {
    // jq returns 1 here, which looks like a bug
    // in general, `a | a |= .`
    // works in jq when `a` is either null, a number, or a boolean --- it
    // does *not* work when `a` is a string, an array, or an object!
    fail(json!(0), "0 |= .+1", Error::path_expr(Val::from(0)));
}

yields!(alt_update_l, "[1, 2] | .[] // . |= 3", [3, 3]);
yields!(alt_update_r, "[] | .[] // . |= 3", 3);
