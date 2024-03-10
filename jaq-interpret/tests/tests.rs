//! Tests for unnamed core filters.

pub mod common;

use chumsky::error::{Simple, SimpleReason};
use common::{give, gives, parse_fail, UNSTABLE};
use serde_json::json;

#[test]
fn update_assign() {
    let ab = |v| json!({"a": v, "b": 2});
    gives(
        UNSTABLE,
        ab(Some(1)),
        ".a  = (.a, .b)",
        [ab(Some(1)), ab(Some(2))],
    );
    gives(UNSTABLE, ab(Some(1)), ".a |= (.+1, .)", [ab(Some(2))]);
    gives(
        UNSTABLE,
        ab(None),
        ".a = .a+.b | ., .a = .a+.b",
        [ab(Some(2)), ab(Some(4))],
    );
    if UNSTABLE {
        gives(
            UNSTABLE,
            ab(None),
            ".a //= .a+.b | ., .a //= .a+.b",
            [ab(Some(2)), ab(Some(2))],
        );
    } else {
        parse_fail(
            UNSTABLE,
            ".a //= .a+.b | ., .a //= .a+.b",
            |errs| match &errs[..] {
                [e @ Simple { .. }] => {
                    assert_eq!(e.span(), 5..7);
                    assert_eq!(e.reason(), &SimpleReason::Unexpected);
                    assert_eq!(e.found().map(|i| &**i), Some("="));
                }
                _ => panic!("errs: {:?}", errs),
            },
        );
    }
    gives(
        UNSTABLE,
        ab(Some(1)),
        ".a += (.a, .b)",
        [ab(Some(2)), ab(Some(3))],
    );
}

// here, jaq diverges from jq, which returns [3,6,4,8]!
// idem for other arithmetic operations
yields!(cartesian_arith, UNSTABLE, "[(1,2) * (3,4)]", [3, 4, 6, 8]);

#[test]
fn add() {
    give(UNSTABLE, json!(1), ". + 2", json!(3));
    give(UNSTABLE, json!(1.0), ". + 2.", json!(3.0));
    give(UNSTABLE, json!(1), "2.0 + .", json!(3.0));
    give(UNSTABLE, json!(null), "1.e1 + 2.1e2", json!(220.0));

    give(
        UNSTABLE,
        json!("Hello "),
        ". + \"world\"",
        json!("Hello world"),
    );
    give(UNSTABLE, json!([1, 2]), ". + [3, 4]", json!([1, 2, 3, 4]));
    give(
        UNSTABLE,
        json!({"a": 1, "b": 2}),
        ". + {c: 3, a: 4}",
        json!({"a": 4, "b": 2, "c": 3}),
    );

    give(UNSTABLE, json!({}), ". + {}", json!({}));
    give(UNSTABLE, json!({"a": 1}), ". + {}", json!({"a": 1}));
}

#[test]
fn sub() {
    give(UNSTABLE, json!(1), ". - -2", json!(3));
    give(UNSTABLE, json!(1.0), ". - 0.1", json!(0.9));
    give(UNSTABLE, json!(1.0), ". - 1", json!(0.0));
}

#[test]
fn mul() {
    give(UNSTABLE, json!(1), ". * 2", json!(2));
    give(UNSTABLE, json!(1.0), ". * 2.", json!(2.0));
    give(UNSTABLE, json!(1), "2.0 * .", json!(2.0));

    give(UNSTABLE, json!("Hello"), "2 * .", json!("HelloHello"));
    give(UNSTABLE, json!(2), ". * \"Hello\"", json!("HelloHello"));

    give(UNSTABLE, json!("Hello"), "0 * .", json!(null));
    give(UNSTABLE, json!(-1), ". * \"Hello\"", json!(null));
    give(
        UNSTABLE,
        json!({"k": {"a": 1, "b": 2}}),
        ". * {k: {a: 0, c: 3}}",
        json!({"k": {"a": 0, "b": 2, "c": 3}}),
    );
}

#[test]
fn logic() {
    let tf = json!([true, false]);
    give(
        UNSTABLE,
        tf.clone(),
        "[.[] and .[]]",
        json!([true, false, false]),
    );
    give(UNSTABLE, tf, "[.[] or .[]]", json!([true, true, false]));
}

#[test]
fn alt() {
    give(UNSTABLE, json!([]), ".[] // 0", json!(0));
    give(UNSTABLE, json!([null, false]), ".[] // 0", json!(0));
    give(
        UNSTABLE,
        json!([null, 1, false, 2]),
        "[.[] // 0]",
        json!([1, 2]),
    );
    give(UNSTABLE, json!([1, 2]), "[.[] // 0]", json!([1, 2]));
    give(UNSTABLE, json!([1, 2]), r#"[.[] // -"a"]"#, json!([1, 2]));
}

#[test]
fn try_() {
    give(UNSTABLE, json!(0), ".?", json!(0));
    give(UNSTABLE, json!(0), r#"(-"a")?, 1"#, json!(1));
    give(UNSTABLE, json!(0), r#"[(1, -"a", 2)?]"#, json!([1, 2]));
}

#[test]
fn precedence() {
    // concatenation binds stronger than application
    give(UNSTABLE, json!(null), "[0, 1 | . + 1]", json!([1, 2]));
    give(UNSTABLE, json!(null), "[0, (1 | . + 1)]", json!([0, 2]));

    // assignment binds stronger than concatenation
    give(UNSTABLE, json!(1), "[. += 1, 2]", json!([2, 2]));
    give(UNSTABLE, json!(1), "[. += (1, 2)]", json!([2, 3]));

    // alternation binds stronger than assignment
    give(
        UNSTABLE,
        json!(false),
        "[(., .) | . = . // 0]",
        json!([0, 0]),
    );
    give(UNSTABLE, json!(false), "((., .) | . = .) // 0", json!(0));

    // disjunction binds stronger than alternation
    give(UNSTABLE, json!(false), ". or . // 0", json!(0));
    give(UNSTABLE, json!(false), ". or (. // 0)", json!(true));

    // conjunction binds stronger than disjunction
    give(UNSTABLE, json!(true), "(0 != 0) and . or .", json!(true));
    give(UNSTABLE, json!(true), "(0 != 0) and (. or .)", json!(false));

    give(UNSTABLE, json!(null), "1 + 2 * 3", json!(7));
    give(UNSTABLE, json!(null), "2 * 3 + 1", json!(7));
}

yields!(
    interpolation,
    UNSTABLE,
    r#"1 | "yields \(.+1)!""#,
    "yields 2!"
);
// this diverges from jq, which yields ["2 2", "3 2", "2 4", "3 4"],
// probably due to different order of evaluation addition
yields!(
    interpolation_many,
    UNSTABLE,
    r#"2 | ["\(., .+1) \(., .*2)"]"#,
    ["2 2", "2 4", "3 2", "3 4"]
);
// this does not work in jq, because jq does not allow for defining formatters
yields!(
    interpolation_fmt,
    UNSTABLE,
    r#"def @say: "say " + .; @say "I \("disco"), you \("party")""#,
    "I say disco, you say party"
);
yields!(
    interpolation_nested,
    UNSTABLE,
    r#""Here \("be \("nestings")")""#,
    "Here be nestings"
);

yields!(
    obj_trailing_comma,
    UNSTABLE,
    "{a:1, b:2, c:3,}",
    json!({"a": 1, "b": 2, "c": 3})
);
yields!(
    obj_complex_key,
    UNSTABLE,
    r#""c" | {a: 1, "b": 2, (.): 3}"#,
    json!({"a": 1, "b": 2, "c": 3})
);
yields!(obj_proj, UNSTABLE, "{a: 1, b: 2} | {a,}", json!({"a": 1}));
yields!(
    obj_proj_set,
    UNSTABLE,
    "{a: 1, b: 2} | {a, c: 3}",
    json!({"a": 1, "c": 3})
);
yields!(
    obj_multi_keys,
    UNSTABLE,
    r#"[{("a", "b"): 1}]"#,
    json!([{"a": 1}, {"b": 1}])
);
yields!(
    obj_multi_vals,
    UNSTABLE,
    "[{a: (1,2), b: (3,4)}]",
    json!([{"a": 1, "b": 3}, {"a": 1, "b": 4}, {"a": 2, "b": 3}, {"a": 2, "b": 4}])
);

#[test]
fn if_then_else() {
    gives(UNSTABLE, json!([]), "if . | .[] then 0 else 0 end", []);

    let f = r#".[] | if . < 0 then "n" else "p" end"#;
    gives(
        UNSTABLE,
        json!([-1, 1, -1]),
        f,
        [json!("n"), json!("p"), json!("n")],
    );

    let f = r#".[] | if .<0 then "n" elif .>0 then "p" else "z" end"#;
    gives(
        UNSTABLE,
        json!([-1, 0, 1]),
        f,
        [json!("n"), json!("z"), json!("p")],
    );

    let f = r#"if .>0, .<0 then 0 elif .>0, .<0 then 1 else 2 end"#;
    gives(UNSTABLE, json!(1), f, [json!(0), json!(1), json!(2)]);
}

// This behaviour diverges from jq. In jaq, a `try` will propagate all
// errors in the stream to the `catch` filter.
yields!(
    try_catch_does_not_short_circuit,
    UNSTABLE,
    "[try (\"1\", \"2\", {}[0], \"4\") catch .]",
    ["1", "2", "cannot index {} with 0", "4"]
);
yields!(
    try_catch_nested,
    UNSTABLE,
    "try try {}[0] catch {}[1] catch .",
    "cannot index {} with 1"
);
yields!(
    try_catch_multi_valued,
    UNSTABLE,
    "[(try (1,2,3[0]) catch (3,4)) | . - 1]",
    [0, 1, 2, 3]
);
yields!(try_without_catch, UNSTABLE, "[try (1,2,3[0],4)]", [1, 2, 4]);
yields!(
    try_catch_prefix_operation,
    UNSTABLE,
    r#"(try -[] catch .) | . > "" and . < []"#,
    true
);
yields!(
    try_catch_postfix_operation,
    UNSTABLE,
    "[try 0[0]? catch .]",
    json!([])
);

// try should not gulp expressions after an infix operator; if it did,
// the inner try in these tests would resolve to empty and omit the
// 1[1] error expression, and the whole expression would yield an
// empty stream
yields!(
    try_parsing_isnt_greedy_wrt_comma,
    UNSTABLE,
    "try (try 0[0], 1[1]) catch . == try 1[1] catch .",
    true
);
yields!(
    try_parsing_isnt_greedy_wrt_pipe,
    UNSTABLE,
    "try (try 0 | 1[1]) catch . == try 1[1] catch .",
    true
);
yields!(
    try_parsing_isnt_greedy_wrt_plus,
    UNSTABLE,
    "try (try 0 + 1[1]) catch . == try 1[1] catch .",
    true
);

#[test]
fn ord() {
    give(UNSTABLE, json!(null), ". < (0 != 0)", json!(true));
    give(UNSTABLE, json!(false), ". < (0 == 0)", json!(true));
    give(UNSTABLE, json!(1), ". > 0.0", json!(true));
    give(UNSTABLE, json!(1), ". < 1.5", json!(true));
    give(UNSTABLE, json!(1.1), ". < 1.5", json!(true));
    give(UNSTABLE, json!(1.5), ". > 1.1", json!(true));
    give(UNSTABLE, json!("ab"), ". < \"b\"", json!(true));
    give(UNSTABLE, json!("a"), ". < \"ab\"", json!(true));
    give(
        UNSTABLE,
        json!({"a": 2}),
        r#". < {"a": 1, "b": 0}"#,
        json!(true),
    );
}

#[test]
fn eq() {
    give(UNSTABLE, json!(1), ". == 1", json!(true));
    give(UNSTABLE, json!(1), "0 == . - .", json!(true));
    give(UNSTABLE, json!(1), ". == -1 * -1", json!(true));
    give(UNSTABLE, json!(1), ". == 2 / 2", json!(true));

    give(UNSTABLE, json!(0), ". == -.", json!(true));
    give(UNSTABLE, json!(0), "-. == .", json!(true));
    give(UNSTABLE, json!(0.0), ". == -.", json!(true));
    give(UNSTABLE, json!(0.0), "-. == .", json!(true));

    gives(
        UNSTABLE,
        json!([0, 1]),
        ".[] == 0",
        [json!(true), json!(false)],
    );

    give(UNSTABLE, json!(1), ". == 1.0", json!(true));
    give(UNSTABLE, json!(1), ". == 2 / 2.0", json!(true));

    give(
        UNSTABLE,
        json!({"a": 1, "b": 2}),
        ". == {b: 2, a: 1}",
        json!(true),
    );
}

yields!(def_var_filter, UNSTABLE, "def f($a; b): $a+b; f(1; 2)", 3);

#[test]
fn vars() {
    give(UNSTABLE, json!(1), " 2  as $x | . + $x", json!(3));
    give(UNSTABLE, json!(1), ".+1 as $x | . + $x", json!(3));
    give(UNSTABLE, json!(1), ". as $x | (2 as $y | 3) | $x", json!(1));

    give(UNSTABLE, json!(1), "def g(f): f; . as $x | g($x)", json!(1));

    let f = r#"def g(f): "z" as $z | f | .+$z; "x" as $x | g("y" as $y | $x+$y)"#;
    give(UNSTABLE, json!(null), f, json!("xyz"));

    let f = r#"def g(f): "a" as $a | "b" as $b | $a + $b + f; "c" as $c | g($c)"#;
    give(UNSTABLE, json!(null), f, json!("abc"));

    let f = r#". as $x | ("y" as $y | "z") | $x"#;
    give(UNSTABLE, json!("x"), f, json!("x"));

    let out = || json!([[1, 4], [1, 5], [2, 4], [2, 5]]);
    let f = "def f($a; b; $c; d): [$a+b, $c+d]; [f((1, 2); 0; (3, 4); 1)]";
    give(UNSTABLE, json!(null), f, out());
    let f = "def f($a; b; $c; d): [$a+b, $c+d]; 0 as $a | 1 as $b | [f((1, 2); $a; (3, 4); $b)]";
    give(UNSTABLE, json!(null), f, out());
}

yields!(
    shadow_funs,
    UNSTABLE,
    "def a: 1; def b: a; def a: 2; a + b",
    3
);
yields!(shadow_vars, UNSTABLE, "1 as $x | 2 as $x | $x", 2);
// arguments from the right are stronger than from the left
yields!(shadow_args, UNSTABLE, "def f(g; g): g; f(1; 2)", 2);

yields!(id_var, UNSTABLE, "def f($a): $a; f(0)", 0);
yields!(id_arg, UNSTABLE, "def f( a):  a; f(0)", 0);
yields!(
    args_mixed,
    UNSTABLE,
    "def f(a; $b): a + $b; 1 as $a | f($a; 2)",
    3
);

yields!(
    nested_comb_args,
    UNSTABLE,
    "def f(a): def g(b): a + b; g(1); f(2)",
    3
);

const ACKERMANN: &str = "def ack($m; $n):
  if $m == 0 then $n + 1
  elif $n == 0 then ack($m-1; 1)
  else ack($m-1; ack($m; $n-1))
  end;";

yields!(
    ackermann,
    UNSTABLE,
    &(ACKERMANN.to_owned() + "ack(3; 4)"),
    125
);

#[test]
fn reduce() {
    let ff = |s| format!(". as $x | reduce 2 as $y (4; {}) | . + $x", s);

    let f = ff("3 as $z | . + $x + $y + $z");
    give(UNSTABLE, json!(1), &f, json!(11));

    let f = "def g(x; y): 3 as $z | . + x + y + $z; ".to_owned() + &ff("g($x; $y)");
    give(UNSTABLE, json!(1), &f, json!(11));
}

yields!(
    foreach_cumulative_sum,
    UNSTABLE,
    "[1, 2, 3] | [foreach .[] as $x (0; .+$x)]",
    [1, 3, 6]
);
yields!(
    for_cumulative_sum,
    UNSTABLE,
    "[1, 2, 3] | [for .[] as $x (0; .+$x)]",
    [0, 1, 3, 6]
);

// jq will give only [4, 3, 7, 12] here because
// it keeps only the *last* output value as input value for the next iteration, whereas
// jaq keeps all output values as input values
yields!(
    foreach_many_outputs,
    UNSTABLE,
    "[foreach (3,4) as $x (1; .+$x, .*$x)]",
    [4, 8, 16, 3, 7, 12]
);
yields!(
    for_many_outputs,
    UNSTABLE,
    "[for (3,4) as $x (1; .+$x, .*$x)]",
    [1, 4, 8, 16, 3, 7, 12]
);
