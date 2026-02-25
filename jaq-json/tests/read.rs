//! Tests for comment support in the JSON reader (`ws_tk`).
//!
//! These tests verify that `//` single-line comments, `/* */` block comments,
//! and the existing `#` comments are all handled correctly.

use jaq_json::read::parse_single;
use jaq_json::Val;

/// Helper: parse a single value from a byte slice, expecting success.
fn single_ok(input: &[u8]) -> Val {
    parse_single(input).expect("expected successful parse")
}

/// Helper: parse a single value from a byte slice, expecting an error.
fn single_err(input: &[u8]) {
    parse_single(input).expect_err("expected parse error");
}

// --------------------------------------------------------------------------
// Single-line // comments
// --------------------------------------------------------------------------

#[test]
fn single_line_comment_after_value() {
    // { "key": "value" // comment\n }
    let input = b"{\"key\": \"value\" // comment\n}";
    let val = single_ok(input);
    // Should parse as an object with key "key" and value "value"
    match &val {
        Val::Obj(obj) => {
            let key = Val::from("key".to_string());
            let expected_val = Val::from("value".to_string());
            assert_eq!(obj.get(&key), Some(&expected_val));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn single_line_comment_before_value() {
    let input = b"// header\n42";
    let val = single_ok(input);
    assert_eq!(val, Val::from(42isize));
}

#[test]
fn single_line_comment_no_trailing_newline() {
    // "true // comment at EOF" -- the comment runs to EOF, which is fine
    let input = b"true // comment at EOF";
    let val = single_ok(input);
    assert_eq!(val, Val::Bool(true));
}

#[test]
fn comment_inside_string_not_stripped() {
    // "// not a comment" should parse as a string containing "// not a comment"
    let input = b"\"// not a comment\"";
    let val = single_ok(input);
    assert_eq!(val, Val::from("// not a comment".to_string()));
}

// --------------------------------------------------------------------------
// Block /* */ comments
// --------------------------------------------------------------------------

#[test]
fn block_comment_inline() {
    let input = b"{\"key\": /* comment */ \"value\"}";
    let val = single_ok(input);
    match &val {
        Val::Obj(obj) => {
            let key = Val::from("key".to_string());
            let expected_val = Val::from("value".to_string());
            assert_eq!(obj.get(&key), Some(&expected_val));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn block_comment_multiline() {
    let input = b"/* this is\na multiline\ncomment */ [1, 2, 3]";
    let val = single_ok(input);
    // Should be an array [1, 2, 3]
    match &val {
        Val::Arr(arr) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Val::from(1isize));
            assert_eq!(arr[1], Val::from(2isize));
            assert_eq!(arr[2], Val::from(3isize));
        }
        other => panic!("expected Arr, got {:?}", other),
    }
}

#[test]
fn block_comment_with_stars() {
    let input = b"/* ** comment ** */ 42";
    let val = single_ok(input);
    assert_eq!(val, Val::from(42isize));
}

#[test]
fn unterminated_block_comment() {
    // A block comment that never closes should produce an error
    let input = b"/* this comment never ends";
    single_err(input);
}

#[test]
fn block_comment_inside_string_not_stripped() {
    let input = b"\"/* not a comment */\"";
    let val = single_ok(input);
    assert_eq!(val, Val::from("/* not a comment */".to_string()));
}

// --------------------------------------------------------------------------
// Mixed comments
// --------------------------------------------------------------------------

#[test]
fn mixed_comments() {
    // A file using //, /* */, and # comments
    let input = b"# hash comment\n// slash comment\n/* block */ 42";
    let val = single_ok(input);
    assert_eq!(val, Val::from(42isize));
}

// --------------------------------------------------------------------------
// Edge case: only comments, no values
// --------------------------------------------------------------------------

#[test]
fn empty_input_with_only_comments() {
    // Only a comment with no value -- parse_single should error (no value found)
    let input = b"// just a comment\n";
    single_err(input);
}
