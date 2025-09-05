use std::{env, io, process, str};

fn golden_test(args: &[&str], input: &str, out_ex: &str) -> io::Result<()> {
    let mut child = process::Command::new(env!("CARGO_BIN_EXE_jaq"))
        .args(args)
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn()?;

    use io::Write;
    child.stdin.take().unwrap().write_all(input.as_bytes())?;
    let output = child.wait_with_output()?;
    assert!(output.status.success());

    let out_act = str::from_utf8(&output.stdout).expect("invalid UTF-8 in output");
    // remove '\r' from output for compatibility with Windows
    let out_act = out_act.replace('\r', "");
    if out_ex.trim() != out_act.trim() {
        println!("Expected output:\n{}\n---", out_ex);
        println!("Actual output:\n{}\n---", out_act);
        process::exit(2);
    }
    Ok(())
}

macro_rules! test {
    ($name:ident, $args:expr, $input:expr, $output:expr) => {
        #[test]
        fn $name() -> io::Result<()> {
            golden_test($args, $input, $output)
        }
    };
}

test!(no_args, &[], "[0, 1]", "[\n  0,\n  1\n]");
test!(one, &["1"], "0", "1");
test!(sparse, &["."], "[2,3]", "[\n  2,\n  3\n]");

test!(
    arg,
    &["--arg", "x", "y", "--arg", "a", "b", "$x + $a"],
    "0",
    "\"yb\""
);

test!(
    argjson,
    &[
        "--argjson",
        "a",
        "[1,2,3]",
        "--argjson",
        "b",
        r#""abc""#,
        "$a,$b"
    ],
    "0",
    r#"[
  1,
  2,
  3
]
"abc""#
);

test!(
    args,
    &["-c", "--args", "--arg", "x", "y", "$ARGS", "a", "--", "--test", "--"],
    "0",
    r#"{"positional":["a","--test","--"],"named":{"x":"y"}}"#
);

test!(
    join_output,
    &["-j", "."],
    r#"[] "foo" "bar" 1 2 {}"#,
    "[]foobar12{}"
);

test!(
    raw_output,
    &["-r", "."],
    r#""foo" "bar" ["baz"]"#,
    r#"foo
bar
[
  "baz"
]"#
);

test!(
    compact,
    &["-c", "."],
    r#"[2,3]
{"a":1,   "b":["c"   ]}"#,
    r#"[2,3]
{"a":1,"b":["c"]}"#
);

test!(
    sort_keys,
    &["-Sc", "."],
    r#"{"b": 1, "a": 2}"#,
    r#"{"a":2,"b":1}"#
);

test!(
    inputs,
    &["-c", r#"{".": .}, {input: input}"#],
    "0\n1\n2\n3",
    r#"{".":0}
{"input":1}
{".":2}
{"input":3}"#
);

test!(
    null_input,
    &["-nc", r#"{".": .}, {inputs: [inputs]}"#],
    "0\n1\n2\n3",
    r#"{".":null}
{"inputs":[0,1,2,3]}"#
);

const ONE23: &str = "One\nTwo\nThree\n";

test!(raw_input_slurp, &["-Rs"], ONE23, r#""One\nTwo\nThree\n""#);

test!(
    rawfile,
    &[
        "--rawfile",
        "a",
        "tests/256.bin",
        "($a | tobytes) == ([range(256)] | tobytes)"
    ],
    "0",
    "true"
);

test!(
    raw_input,
    &["-R"],
    ONE23,
    r#""One"
"Two"
"Three""#
);

test!(
    fmt_str,
    &[],
    r#""\u0000\u200b\r\t\n asdf""#,
    r#""\u0000​\r\t\n asdf""#
);

test!(
    fmt_obj_key,
    &["-c"],
    r#"{"विश\u094dव": 1}"#,
    r#"{"विश्व":1}"#
);

test!(
    non_str_key,
    &["-c"],
    r#"{1: 2, 3.1415: 4, ["foo"]: "bar", {true: false}: true}"#,
    r#"{1: 2,3.1415: 4,["foo"]: "bar",{true: false}: true}"#
);

test!(surrogate_pair, &[], r#""\uD801\uDC37""#, r#""𐐷""#);

test!(
    mods,
    &["-c", "-L", "tests", r#"include "a"; [a, data, d]"#],
    "0",
    r#"["bcddd",[1,2],3]"#
);
