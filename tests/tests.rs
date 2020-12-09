use jaq::{Filter, Val};
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

    let out: Vec<_> = f.run(x).collect();
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
fn add() {
    give(json!({"a": 1, "b": 2}), "add", json!(3));
}

#[test]
fn precedence() {
    // conjunction binds stronger than disjunction
    give(json!(true), "false and . or .", json!(true));
    give(json!(true), "false and (. or .)", json!(false));
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
fn map() {
    give(json!([1, 2]), "map(.+1)", json!([2, 3]));
}

#[test]
fn ord() {
    give(json!(false), ". < true", json!(true));
    give(json!({"a": 2}), r#". < {"a": 1, "b": 0}"#, json!(true));
}
