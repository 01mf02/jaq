// SPDX-FileCopyrightText: 2021 Michael Färber
//
// SPDX-License-Identifier: MIT

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
    compact,
    &["-c", "."],
    r#"[2,3]
{"a":1,   "b":["c"   ]}"#,
    r#"[2,3]
{"a":1,"b":["c"]}"#
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
    raw_input,
    &["-R"],
    ONE23,
    r#""One"
"Two"
"Three""#
);
