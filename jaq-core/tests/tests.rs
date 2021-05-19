use core::convert::TryFrom;
use jaq_core::{ClosedFilter, Error, Main, Val};
use serde_json::{json, Value};
use std::rc::Rc;

fn give(x: Value, f: &str, y: Value) {
    gives(x, f, [y])
}

fn gives<const N: usize>(x: Value, f: &str, ys: [Value; N]) {
    yields(x, f, ys, None)
}

fn fail(x: Value, f: &str, err: Error) {
    fails(x, f, [], err)
}

fn fails<const N: usize>(x: Value, f: &str, ys: [Value; N], err: Error) {
    yields(x, f, ys, Some(err))
}

fn yields<const N: usize>(x: Value, f: &str, ys: [Value; N], err: Option<Error>) {
    let to = |v| Rc::new(Val::from(v));
    let f = Main::parse(f).unwrap();
    let f = f.open(jaq_core::std()).unwrap();
    let f = ClosedFilter::try_from(f).unwrap();

    // TODO: remove cloned() starting from Rust 1.53
    let expected = ys.iter().cloned().map(|y| Ok(to(y)));
    let expected: Vec<_> = expected.chain(err.into_iter().map(Err)).collect();

    let out: Vec<_> = f.run(to(x)).collect();
    assert_eq!(out, expected);
}

#[test]
fn any() {
    give(json!({"a": false, "b": true}), "any", json!(true));
}

#[test]
fn all() {
    give(json!({"a": false, "b": true}), "all", json!(false));
    give(json!({"a": 1, "b": 2}), "all", json!(true))
}

#[test]
fn math() {
    give(json!(1), ". + 2", json!(3));
    give(json!("Hello "), ". + \"world\"", json!("Hello world"));
    give(json!([1, 2]), ". + [3, 4]", json!([1, 2, 3, 4]));
    give(
        json!({"a": 1, "b": 2}),
        ". + {c: 3, a: 4}",
        json!({"a": 4, "b": 2, "c": 3}),
    );

    give(json!({}), ". + {}", json!({}));
    give(json!({"a": 1}), ". + {}", json!({"a": 1}));
}

#[test]
fn logic() {
    let tf = json!([true, false]);
    give(tf.clone(), "[.[] and .[]]", json!([true, false, false]));
    give(tf, "[.[] or .[]]", json!([true, true, false]));
}

#[test]
fn add() {
    give(json!({"a": 1, "b": 2}), "add", json!(3));
    give(json!([[0, 1], [2, 3]]), "add", json!([0, 1, 2, 3]));
}

#[test]
fn precedence() {
    // conjunction binds stronger than disjunction
    give(json!(true), "false and . or .", json!(true));
    give(json!(true), "false and (. or .)", json!(false));

    give(json!(null), "1 + 2 * 3", json!(7));
    give(json!(null), "2 * 3 + 1", json!(7));
}

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

    give(json!({"a": 1}), r#".[0, "a", true]?"#, json!(1));
    give(json!([0, 1, 2]), r#".["a", 0, true]?"#, json!(0));
    give(json!([0, 1, 2]), r#".[3]?"#, json!(null));
    gives(json!("asdf"), ".[0]?", []);

    gives(
        json!({"a": 1, "b": 2}),
        r#".["b", "a"]"#,
        [json!(2), json!(1)],
    );
}

#[test]
fn iter_access() {
    gives(json!([0, 1, 2]), ".[]", [json!(0), json!(1), json!(2)]);
    gives(json!({"a": 1, "b": 2}), ".[]", [json!(1), json!(2)]);
    gives(json!({"b": 2, "a": 1}), ".[]", [json!(2), json!(1)]);
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

#[test]
fn index_update() {
    give(json!({"a": 1}), ".b |= .", json!({"a": 1, "b": null}));
    give(json!({"a": 1}), ".b |= 1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1}), ".b |= .+1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1, "b": 2}), ".b |= empty", json!({"a": 1}));
    give(json!({"a": 1, "b": 2}), ".a += 1", json!({"a": 2, "b": 2}));

    give(json!([0, 1, 2]), ".[1] |= .+2", json!([0, 3, 2]));
    give(json!([0, 1, 2]), ".[-1,-1] |= empty", json!([0]));
    give(json!([0, 1, 2]), ".[ 0, 0] |= empty", json!([2]));
    fail(json!([0, 1, 2]), ".[ 3] |=  3", Error::IndexOutOfBounds(3));
    fail(json!([0, 1, 2]), ".[-4] |= -1", Error::IndexOutOfBounds(-4));

    give(json!({"a": 1}), r#".[0, "a"]? |= .+1"#, json!({"a": 2}));
    give(json!([0, 1, 2]), r#".["a", 0]? |= .+1"#, json!([1, 1, 2]));
    give(json!([0, 1, 2]), r#".[3]? |= .+1"#, json!([0, 1, 2]));
    give(json!("asdf"), ".[0]? |= .+1", json!("asdf"));
}

#[test]
fn iter_update() {
    // precedence tests
    give(json!([]), ".[] |= . or true", json!([]));
    gives(json!([]), ".[] |= .,.", [json!([]), json!([])]);
    give(json!([]), ".[] |= (.,.)", json!([]));
    give(json!([0]), ".[] |= .+1 | .+[2]", json!([1, 2]));
    // this yields a syntax error in jq, but it is consistent to permit this
    give(json!([[1]]), ".[] |= .[] |= .+1", json!([[2]]));
    // ditto
    give(json!([[1]]), ".[] |= .[] += 1", json!([[2]]));

    give(json!([1]), ".[] |= .+1", json!([2]));
    give(json!([[1]]), ".[][] |= .+1", json!([[2]]));

    give(
        json!({"a": 1, "b": 2}),
        ".[] |= (select(.>1) | .+1)",
        json!({"b": 3}),
    );

    give(json!([[0, 1], "a"]), ".[][]? |= .+1", json!([[1, 2], "a"]));
}

#[test]
fn range_update() {
    give(json!([0, 1, 2]), ".[:2] |= map(.+5)", json!([5, 6, 2]));
    give(json!([0, 1, 2]), ".[-2:-1] |= [5]+.", json!([0, 5, 1, 2]));
    give(
        json!([0, 1, 2]),
        ".[-2:-1,-1] |= [5,6]+.",
        json!([0, 5, 6, 5, 6, 1, 2]),
    );

    give(json!([0, 1, 2]), ".[:2,3.0]? |= map(.+1)", json!([1, 2, 2]));
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
    gives(json!(1), ". |= empty", []);
    // jq returns just 1 here
    gives(json!(1), ". |= (.,.)", [json!(1), json!(1)]);
    // jq returns just [1] here
    give(json!([1]), ".[] |= (., .+1)", json!([1, 2]));
    // jq returns just [1,2] here
    give(json!([1, 3]), ".[] |= (., .+1)", json!([1, 2, 3, 4]));
    // here comes a huge WTF: jq returns [2,4] here -- looks like a bug?
    give(json!([1, 2, 3, 4, 5]), ".[] |= empty", json!([]));
}

#[test]
fn object() {
    gives(
        json!(null),
        r#"{("a", "b"): 1}"#,
        [json!({"a": 1}), json!({"b": 1})],
    );
    give(
        json!("c"),
        r#"{a: 1, "b": 2, (.): 3}"#,
        json!({"a": 1, "b": 2, "c": 3}),
    );
    give(
        json!({"a": 1, "b": 2}),
        "{a, c: 3}",
        json!({"a": 1, "c": 3}),
    );
}

#[test]
fn if_then_else() {
    gives(
        json!([-1, 42, -42]),
        r#".[] | if . < 0 then "n" else "p" end"#,
        [json!("n"), json!("p"), json!("n")],
    )
}

#[test]
fn length() {
    give(json!("ƒoo"), "length", json!(3));
    give(json!("नमस्ते"), "length", json!(6));
    give(json!({"a": 5, "b": 3}), "length", json!(2));
}

#[test]
fn typ() {
    give(json!({"a": 1, "b": 2}), "type", json!("object"));
    give(json!("Hello"), "type", json!("string"));
    give(json!(1), "type", json!("number"));
    give(json!(1.0), "type", json!("number"));
    give(json!(true), "type", json!("boolean"));
    give(json!(null), "type", json!("null"));
}

#[test]
fn map() {
    give(json!([1, 2]), "map(.+1)", json!([2, 3]));
}

#[test]
fn select() {
    give(json!([1, 2]), ".[] | select(.>1)", json!(2));
    give(json!([0, 1, 2]), "map(select(.<1, 1<.))", json!([0, 2]));
}

#[test]
fn recurse() {
    let x = json!({"a":0,"b":[1]});
    gives(x.clone(), "recurse", [x, json!(0), json!([1]), json!(1)]);

    let y = [json!(1), json!(2), json!(3)];
    gives(json!(1), "recurse(if . < 3 then .+1 else empty end)", y);

    let y = [json!(2), json!(4), json!(16)];
    gives(json!(2), "recurse(. * .; . < 20)", y);
}

#[test]
fn fib() {
    let fib = "[0,1] | recurse([.[1], add]) | .[0]";
    give(json!(10), &format!("nth(.; {})", fib), json!(55));
}

#[test]
fn fold() {
    // the corresponding jq command is:
    //     reduce recurse(if . == 1000 then empty else .+1 end) as $x (0; . + $x)
    give(
        json!(0),
        "fold(recurse(if . == 1000 then empty else .+1 end); 0; .acc + .x)",
        json!(500500),
    );
}

#[test]
fn first_last() {
    gives(json!([]), "first(.[])", []);
    gives(json!([]), "last(.[])", []);
    give(json!([1, 2, 3]), "first(.[])", json!(1));
    give(json!([1, 2, 3]), "last(.[])", json!(3));
}

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(json!(null), "limit(0; 1,2)", []);
    // jaq does not support negative indices in limit
    give(json!(null), "[limit(1, 0, 3; 0, 1)]", json!([0, 0, 1]));
}

#[test]
fn range_reverse() {
    give(json!(null), "[range(1, 2)]", json!([0, 0, 1]));
    give(
        json!(null),
        "[range(-1, 1; 0, 2)]",
        json!([-1, -1, 0, 1, 1]),
    );
    give(json!(3), "[range(.)] | reverse", json!([2, 1, 0]));
}

#[test]
fn min_max() {
    give(json!([]), "min", json!(null));
    give(json!([]), "max", json!(null));
    give(json!([1, 4, 2]), "min", json!(1));
    give(json!([1, 4, 2]), "max", json!(4));
}

#[test]
fn transpose() {
    give(
        json!([[1, 3], [2]]),
        "transpose",
        json!([[1, 2], [3, null]]),
    );
    give(
        json!([[1, 3], [2, 4]]),
        "transpose",
        json!([[1, 2], [3, 4]]),
    );
}

#[test]
fn repeat() {
    give(
        json!([0, 1]),
        "[limit(4; repeat(.[]))]",
        json!([0, 1, 0, 1]),
    );
}

#[test]
fn ord() {
    give(json!(null), ". < false", json!(true));
    give(json!(false), ". < true", json!(true));
    give(json!(1), ". > 0.0", json!(true));
    give(json!(1), ". < 1.5", json!(true));
    give(json!("ab"), ". < \"b\"", json!(true));
    give(json!("a"), ". < \"ab\"", json!(true));
    give(json!({"a": 2}), r#". < {"a": 1, "b": 0}"#, json!(true));
}

#[test]
fn eq() {
    give(json!(1), ". == 1", json!(true));
    give(json!(1), "0 == . - .", json!(true));
    give(json!(1), ". == -1 * -1", json!(true));
    give(json!(1), ". == 2 / 2", json!(true));

    gives(json!([0, 1]), ".[] == 0", [json!(true), json!(false)]);

    // here, we diverge from jq, which outputs true
    give(json!(1), ". == 1.0", json!(false));
    give(json!(1), ". == 2 / 2.0", json!(false));
}

#[test]
fn def() {
    give(
        json!([1, 2]),
        "def positive(f): all(f; . > 0); positive(.[])",
        json!(true),
    );
}
