use jaq_core::{Filter, Val};
use serde_json::json;
use std::rc::Rc;

fn to(v: serde_json::Value) -> Rc<Val> {
    Rc::new(Val::from(v))
}

fn give(x: serde_json::Value, f: &str, y: serde_json::Value) {
    gives(x, f, vec![y])
}

fn gives(x: serde_json::Value, f: &str, ys: Vec<serde_json::Value>) {
    let x = to(x);
    let f = Filter::parse(f).unwrap();
    let ys: Vec<_> = ys.into_iter().map(to).collect();

    let out: Vec<_> = f.run(x).map(|y| y.unwrap()).collect();
    assert_eq!(out, ys);
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
}

#[test]
fn add() {
    give(json!({"a": 1, "b": 2}), "add", json!(3));
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
fn path() {
    give(json!({"a": 1}), ".a", json!(1));
    give(json!({"a": 1}), ".a?", json!(1));
    give(json!({"a": 1}), ".a ?", json!(1));
    give(json!({"a": 1}), r#"."a""#, json!(1));
    give(json!({"a": 1}), r#". "a""#, json!(1));
    give(json!({"a": 1}), r#".["a"]"#, json!(1));
    give(json!({"a": 1}), r#". ["a"]"#, json!(1));

    gives(
        json!({"a": 1, "b": 2}),
        r#".["b", "a"]"#,
        vec![json!(2), json!(1)],
    );
}

#[test]
fn assign() {
    give(json!([1, 2]), ".[] = .", json!([[1, 2], [1, 2]]));
    give(
        json!({"a": [1,2], "b": 3}),
        ".a[] = .b+.b",
        json!({"a": [6,6], "b": 3}),
    );
}

#[test]
fn update() {
    // precedence tests
    give(json!([]), ".[] |= . or true", json!([]));
    gives(json!([]), ".[] |= .,.", vec![json!([]), json!([])]);
    give(json!([]), ".[] |= (.,.)", json!([]));
    give(json!([0]), ".[] |= .+1 | .+[2]", json!([1, 2]));
    // this yields a syntax error in jq, but it is consistent to permit this
    give(json!([[1]]), ".[] |= .[] |= .+1", json!([[2]]));
    // ditto
    give(json!([[1]]), ".[] |= .[] += 1", json!([[2]]));

    give(json!({"a": 1}), ".b |= .", json!({"a": 1, "b": null}));
    give(json!({"a": 1}), ".b |= 1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1}), ".b |= .+1", json!({"a": 1, "b": 1}));
    give(json!({"a": 1, "b": 2}), ".b |= empty", json!({"a": 1}));
    give(json!({"a": 1, "b": 2}), ".a += 1", json!({"a": 2, "b": 2}));

    give(json!([1]), ".[] |= .+1", json!([2]));
    give(json!([[1]]), ".[][] |= .+1", json!([[2]]));
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
    gives(json!(1), ". |= empty", vec![]);
    // jq returns just 1 here
    gives(json!(1), ". |= (.,.)", vec![json!(1), json!(1)]);
    // jq returns just [1] here
    give(json!([1]), ".[] |= (., .+1)", json!([1, 2]));
    // jq returns just [1,2] here
    give(json!([1, 3]), ".[] |= (., .+1)", json!([1, 2, 3, 4]));
    // here comes a huge WTF: jq returns [2,4] here -- looks like a bug?
    give(json!([1, 2, 3, 4, 5]), ".[] |= empty", json!([]));
}

// TODO!
//#[test]
fn punning() {
    give(
        json!({"a": 1, "b": 2}),
        "{a, c: 3}",
        json!({"a": 1, "c": 3}),
    );
}

#[test]
fn index() {
    give(json!([0, 1, 2]), ".[-4]", json!(null));
    give(json!([0, 1, 2]), ".[-3]", json!(0));
    give(json!([0, 1, 2]), ".[-1]", json!(2));
    give(json!([0, 1, 2]), ".[ 0]", json!(0));
    give(json!([0, 1, 2]), ".[ 2]", json!(2));
    give(json!([0, 1, 2]), ".[ 3]", json!(null));
}

#[test]
fn iter() {
    give(json!([0, 1, 2]), "[.[]]", json!([0, 1, 2]));
    give(json!({"a": 1, "b": 2}), "[.[]]", json!([1, 2]));
    give(json!({"b": 2, "a": 1}), "[.[]]", json!([2, 1]));
}

#[test]
fn range() {
    give(json!([0, 1, 2]), ".[-4:4]", json!([0, 1, 2]));
    give(json!([0, 1, 2]), ".[0:3]", json!([0, 1, 2]));
    give(json!([0, 1, 2]), ".[1:]", json!([1, 2]));
    give(json!([0, 1, 2]), ".[:-1]", json!([0, 1]));
    give(json!([0, 1, 2]), ".[1:0]", json!([]));
    give(json!([0, 1, 2]), ".[4:5]", json!([]));
}

#[test]
fn if_then_else() {
    gives(
        json!([-1, 42, -42]),
        r#".[] | if . < 0 then "n" else "p" end"#,
        vec![json!("n"), json!("p"), json!("n")],
    )
}

#[test]
fn length() {
    give(json!("ƒoo"), "length", json!(3));
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
    gives(
        json!(1),
        "recurse(if . < 3 then .+1 else empty end)",
        vec![json!(1), json!(2), json!(3)],
    );
}

#[test]
fn fib() {
    give(
        json!(10),
        "nth(.; [0,1] | recurse([.[1], add]) | .[0])",
        json!(55),
    );
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
    gives(json!([]), "first(.[])", vec![]);
    gives(json!([]), "last(.[])", vec![]);
    give(json!([1, 2, 3]), "first(.[])", json!(1));
    give(json!([1, 2, 3]), "last(.[])", json!(3));
}

#[test]
fn limit() {
    // a big WTF: jq outputs "1" here! that looks like another bug ...
    gives(json!(null), "limit(0; 1,2)", vec![]);
    give(
        json!(null),
        "[limit(1, -1, 3; 0, 1)]",
        json!([0, 0, 1, 0, 1]),
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

    gives(json!([0, 1]), ".[] == 0", vec![json!(true), json!(false)]);

    // here, we diverge from jq, which outputs true
    give(json!(1), ". == 1.0", json!(false));
    give(json!(1), ". == 2 / 2.0", json!(false));
}
