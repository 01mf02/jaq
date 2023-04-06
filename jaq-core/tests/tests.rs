//! Tests for unnamed core filters.

pub mod common;

use common::{give, gives};
use serde_json::json;

#[test]
fn update_assign() {
    let ab = |v| json!({"a": v, "b": 2});
    gives(ab(1), ".a  = (.a, .b)", [ab(1), ab(2)]);
    gives(ab(1), ".a += (.a, .b)", [ab(2), ab(3)]);
    gives(ab(1), ".a |= (.+1, .)", [ab(2)]);
}

#[test]
fn cartesian() {
    give(
        json!(0),
        "[{a: (1,2), b: (3,4)}]",
        json!([{"a": 1, "b": 3}, {"a": 1, "b": 4}, {"a": 2, "b": 3}, {"a": 2, "b": 4}]),
    );

    // here, jaq diverges from jq, which returns [3,6,4,8]!
    // idem for other arithmetic operations
    give(json!(0), "[(1,2) * (3,4)]", json!([3, 4, 6, 8]));
}

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
fn alt() {
    give(json!([]), ".[] // 0", json!(0));
    give(json!([null, false]), ".[] // 0", json!(0));
    give(json!([null, 1, false, 2]), "[.[] // 0]", json!([1, 2]));
    give(json!([1, 2]), "[.[] // 0]", json!([1, 2]));
    give(json!([1, 2]), "[.[] // error]", json!([1, 2]));
}

#[test]
fn try_() {
    give(json!(0), ".?", json!(0));
    give(json!(0), "keys?, 1", json!(1));
    give(json!(0), "[(1, error, 2)?]", json!([1, 2]));
}

#[test]
fn precedence() {
    // concatenation binds stronger than application
    give(json!(null), "[0, 1 | . + 1]", json!([1, 2]));
    give(json!(null), "[0, (1 | . + 1)]", json!([0, 2]));

    // assignment binds stronger than concatenation
    give(json!(1), "[. += 1, 2]", json!([2, 2]));
    give(json!(1), "[. += (1, 2)]", json!([2, 3]));

    // alternation binds stronger than assignment
    give(json!(false), "[(., .) | . = . // 0]", json!([0, 0]));
    give(json!(false), "((., .) | . = .) // 0", json!(0));

    // disjunction binds stronger than alternation
    give(json!(false), ". or . // 0", json!(0));
    give(json!(false), ". or (. // 0)", json!(true));

    // conjunction binds stronger than disjunction
    give(json!(true), "(0 != 0) and . or .", json!(true));
    give(json!(true), "(0 != 0) and (. or .)", json!(false));

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
        json!(null),
        "{a:1, b:2, c:3,}",
        json!({"a": 1, "b": 2, "c": 3}),
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
    give(json!({"a": 1, "b": 2}), "{a,}", json!({"a": 1}));
}

#[test]
fn if_then_else() {
    gives(json!([]), "if . | .[] then 0 else 0 end", []);

    let f = r#".[] | if . < 0 then "n" else "p" end"#;
    gives(json!([-1, 1, -1]), f, [json!("n"), json!("p"), json!("n")]);

    let f = r#".[] | if .<0 then "n" elif .>0 then "p" else "z" end"#;
    gives(json!([-1, 0, 1]), f, [json!("n"), json!("z"), json!("p")]);

    let f = r#"if .>0, .<0 then 0 elif .>0, .<0 then 1 else 2 end"#;
    gives(json!(1), f, [json!(0), json!(1), json!(2)]);
}

#[test]
fn ord() {
    give(json!(null), ". < (0 != 0)", json!(true));
    give(json!(false), ". < (0 == 0)", json!(true));
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

    give(json!(0), ". == -.", json!(true));
    give(json!(0), "-. == .", json!(true));
    give(json!(0.0), ". == -.", json!(true));
    give(json!(0.0), "-. == .", json!(true));

    gives(json!([0, 1]), ".[] == 0", [json!(true), json!(false)]);

    give(json!(1), ". == 1.0", json!(true));
    give(json!(1), ". == 2 / 2.0", json!(true));

    give(json!({"a": 1, "b": 2}), ". == {b: 2, a: 1}", json!(true));
}

#[test]
fn vars() {
    give(json!(1), " 2  as $x | . + $x", json!(3));
    give(json!(1), ".+1 as $x | . + $x", json!(3));
    give(json!(1), ". as $x | (2 as $y | 3) | $x", json!(1));

    give(json!(1), "def g(f): f; . as $x | g($x)", json!(1));

    let f = r#"def g(f): "z" as $z | f | .+$z; "x" as $x | g("y" as $y | $x+$y)"#;
    give(json!(null), f, json!("xyz"));

    let f = r#"def g(f): "a" as $a | "b" as $b | $a + $b + f; "c" as $c | g($c)"#;
    give(json!(null), f, json!("abc"));

    let f = r#". as $x | ("y" as $y | "z") | $x"#;
    give(json!("x"), f, json!("x"));

    let out = || json!([[1, 4], [1, 5], [2, 4], [2, 5]]);
    let f = "def f($a; b; $c; d): [$a+b, $c+d]; [f((1, 2); 0; (3, 4); 1)]";
    give(json!(null), f, out());
    let f = "def f($a; b; $c; d): [$a+b, $c+d]; 0 as $a | 1 as $b | [f((1, 2); $a; (3, 4); $b)]";
    give(json!(null), f, out());
}

#[test]
fn redefine() {
    let f = "def a: 1; def b: a; def a: 2; [a, b]";
    give(json!(0), f, json!([2, 1]));
}

#[test]
fn shadow() {
    // arguments from the right are stronger than from the left
    give(json!(0), "def f(g; g): g; f(1; 2)", json!(2));

    give(json!(0), "1 as $x | 2 as $x | $x", json!(2));
}

#[test]
fn reduce() {
    let f = "reduce recurse(if . == 1000 then empty else .+1 end) as $x (0; . + $x)";
    give(json!(0), f, json!(500500));

    let ff = |s| format!(". as $x | reduce 2 as $y (4; {}) | . + $x", s);

    let f = ff("3 as $z | . + $x + $y + $z");
    give(json!(1), &f, json!(11));

    let f = "def g(x; y): 3 as $z | . + x + y + $z; ".to_owned() + &ff("g($x; $y)");
    give(json!(1), &f, json!(11));
}

#[test]
fn foreach() {
    let f = "[foreach .[] as $x (0; .+$x)]";
    give(json!([1, 2, 3]), f, json!([1, 3, 6]));
    let f = "[for .[] as $x (0; .+$x)]";
    give(json!([1, 2, 3]), f, json!([0, 1, 3, 6]));

    // jq will give only [4, 3, 7, 12] here because
    // it keeps only the *last* output value as
    // input value for the next iteration, whereas
    // jaq keeps all output values as input values
    let f = "[foreach .[] as $x (1; .+$x, .*$x)]";
    give(json!([3, 4]), f, json!([4, 8, 16, 3, 7, 12]));
    let f = "[for .[] as $x (1; .+$x, .*$x)]";
    give(json!([3, 4]), f, json!([1, 4, 8, 16, 3, 7, 12]));
}
