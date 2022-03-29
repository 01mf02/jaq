pub mod common;

use common::{give, gives};
use serde_json::json;

#[test]
fn add() {
    give(json!(1), ". + 2", json!(3));
    give(json!(1.0), ". + 2.", json!(3.0));
    give(json!(1), "2.0 + .", json!(3.0));
    give(json!(null), "1.e1 + 2.1e2", json!(220.0));

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
fn sub() {
    give(json!(1), ". - -2", json!(3));
    give(json!(1.0), ". - 0.1", json!(0.9));
    give(json!(1.0), ". - 1", json!(0.0));
}

#[test]
fn logic() {
    let tf = json!([true, false]);
    give(tf.clone(), "[.[] and .[]]", json!([true, false, false]));
    give(tf, "[.[] or .[]]", json!([true, true, false]));
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
fn fib() {
    let fib = "[0,1] | recurse([.[1], add]) | .[0]";
    give(json!(10), &format!("nth(.; {})", fib), json!(55));
}

#[test]
fn ord() {
    give(json!(null), ". < false", json!(true));
    give(json!(false), ". < true", json!(true));
    give(json!(1), ". > 0.0", json!(true));
    give(json!(1), ". < 1.5", json!(true));
    give(json!(1.1), ". < 1.5", json!(true));
    give(json!(1.5), ". > 1.1", json!(true));
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

    give(json!({"a": 1, "b": 2}), ". == {b: 2, a: 1}", json!(true));
}

#[test]
fn def() {
    give(
        json!([1, 2]),
        "def positive(f): all(f; . > 0); positive(.[])",
        json!(true),
    );
}
