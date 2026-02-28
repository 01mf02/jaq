//! Tests for comment support and JSON5 in the JSON reader.
//!
//! Plain JSON tests use `parse_single` / `parse_many` (only `#` comments).
//! JSONC tests use `parse_single_jsonc` / `parse_many_jsonc` (`#`, `//`, `/* */`).
//! JSON5 tests use `parse_single_json5` / `parse_many_json5` (all JSON5 features).

use jaq_json::read::{parse_many_jsonc, parse_single, parse_single_jsonc, parse_single_json5};
use jaq_json::Val;

/// Helper: parse a single JSONC value, expecting success.
fn single_ok(input: &[u8]) -> Val {
    parse_single_jsonc(input).expect("expected successful parse")
}

/// Helper: parse a single JSONC value, expecting an error.
fn single_err(input: &[u8]) {
    parse_single_jsonc(input).expect_err("expected parse error");
}

/// Helper: parse a single JSON5 value, expecting success.
fn json5_ok(input: &[u8]) -> Val {
    parse_single_json5(input).expect("expected successful JSON5 parse")
}

/// Helper: parse a single JSON5 value, expecting an error.
fn json5_err(input: &[u8]) {
    parse_single_json5(input).expect_err("expected JSON5 parse error");
}

// --------------------------------------------------------------------------
// Plain JSON: `#` comments work, `//` and `/*` do NOT
// --------------------------------------------------------------------------

#[test]
fn plain_json_hash_comment() {
    let input = b"# comment\n42";
    let val = parse_single(input).unwrap();
    assert_eq!(val, Val::from(42isize));
}

#[test]
fn plain_json_rejects_slash_comment() {
    // Plain JSON parser does not understand //, so "// comment\n42"
    // starts with '/' which is not a valid JSON token
    let input = b"// comment\n42";
    parse_single(input).expect_err("plain JSON should reject // comments");
}

#[test]
fn plain_json_rejects_block_comment() {
    let input = b"/* comment */ 42";
    parse_single(input).expect_err("plain JSON should reject /* */ comments");
}

// --------------------------------------------------------------------------
// JSONC: Single-line // comments
// --------------------------------------------------------------------------

#[test]
fn single_line_comment_after_value() {
    let input = b"{\"key\": \"value\" // comment\n}";
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
fn single_line_comment_before_value() {
    let input = b"// header\n42";
    let val = single_ok(input);
    assert_eq!(val, Val::from(42isize));
}

#[test]
fn single_line_comment_no_trailing_newline() {
    let input = b"true // comment at EOF";
    let val = single_ok(input);
    assert_eq!(val, Val::Bool(true));
}

#[test]
fn comment_inside_string_not_stripped() {
    let input = b"\"// not a comment\"";
    let val = single_ok(input);
    assert_eq!(val, Val::from("// not a comment".to_string()));
}

// --------------------------------------------------------------------------
// JSONC: Block /* */ comments
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
    single_err(b"/* this comment never ends");
}

#[test]
fn block_comment_inside_string_not_stripped() {
    let input = b"\"/* not a comment */\"";
    let val = single_ok(input);
    assert_eq!(val, Val::from("/* not a comment */".to_string()));
}

// --------------------------------------------------------------------------
// JSONC: Mixed comments
// --------------------------------------------------------------------------

#[test]
fn mixed_comments() {
    let input = b"# hash comment\n// slash comment\n/* block */ 42";
    let val = single_ok(input);
    assert_eq!(val, Val::from(42isize));
}

// --------------------------------------------------------------------------
// JSONC: parse_many with comments between values
// --------------------------------------------------------------------------

#[test]
fn parse_many_with_comments() {
    let input = b"1 // first\n2 /* second */ 3";
    let vals: Vec<Val> = parse_many_jsonc(input).collect::<Result<_, _>>().unwrap();
    assert_eq!(vals.len(), 3);
    assert_eq!(vals[0], Val::from(1isize));
    assert_eq!(vals[1], Val::from(2isize));
    assert_eq!(vals[2], Val::from(3isize));
}

// --------------------------------------------------------------------------
// JSONC: Nested block comments (not supported — ends at first */)
// --------------------------------------------------------------------------

#[test]
fn nested_block_comment_not_supported() {
    let input = b"/* outer /* inner */ */ 42";
    single_err(input);
}

// --------------------------------------------------------------------------
// JSONC: Edge case — only comments, no values
// --------------------------------------------------------------------------

#[test]
fn empty_input_with_only_comments() {
    let input = b"// just a comment\n";
    single_err(input);
}

// --------------------------------------------------------------------------
// JSONC: Incomplete comment errors
// --------------------------------------------------------------------------

#[test]
fn bare_slash_is_error() {
    single_err(b"/");
}

#[test]
fn bare_slash_after_value_is_error() {
    let results: Vec<_> = parse_many_jsonc(b"1 /").collect();
    assert_eq!(results.len(), 2);
    assert!(results[0].is_ok());
    assert!(results[1].is_err());
}

#[test]
fn unterminated_block_comment_after_value_is_error() {
    let results: Vec<_> = parse_many_jsonc(b"1 /*").collect();
    assert_eq!(results.len(), 2);
    assert!(results[0].is_ok());
    assert!(results[1].is_err());
}

#[test]
fn unterminated_block_comment_with_content_is_error() {
    single_err(b"/* comment without end");
}

#[test]
fn bare_slash_in_array_is_error() {
    single_err(b"[1, /]");
}

#[test]
fn unterminated_block_comment_in_object_is_error() {
    single_err(b"{\"key\": /* unterminated");
}

// --------------------------------------------------------------------------
// Edge cases: comments at structural positions
// --------------------------------------------------------------------------

#[test]
fn comment_between_array_elements() {
    let val = single_ok(b"[1, /* between */ 2, // also\n3]");
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
fn comment_before_closing_bracket() {
    let val = single_ok(b"[1, 2 /* trailing */]");
    match &val {
        Val::Arr(arr) => {
            assert_eq!(arr.len(), 2);
        }
        other => panic!("expected Arr, got {:?}", other),
    }
}

#[test]
fn comment_after_opening_bracket() {
    let val = single_ok(b"[/* leading */ 1]");
    match &val {
        Val::Arr(arr) => {
            assert_eq!(arr.len(), 1);
            assert_eq!(arr[0], Val::from(1isize));
        }
        other => panic!("expected Arr, got {:?}", other),
    }
}

#[test]
fn comment_between_key_and_colon() {
    let val = single_ok(b"{\"k\" /* comment */ : 1}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("k".to_string())), Some(&Val::from(1isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn comment_between_object_entries() {
    let val = single_ok(b"{\"a\": 1, /* gap */ \"b\": 2}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("a".to_string())), Some(&Val::from(1isize)));
            assert_eq!(obj.get(&Val::from("b".to_string())), Some(&Val::from(2isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn comment_before_closing_brace() {
    let val = single_ok(b"{\"a\": 1 // end\n}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("a".to_string())), Some(&Val::from(1isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn comment_in_empty_array() {
    let val = single_ok(b"[/* nothing */]");
    match &val {
        Val::Arr(arr) => assert_eq!(arr.len(), 0),
        other => panic!("expected empty Arr, got {:?}", other),
    }
}

#[test]
fn comment_in_empty_object() {
    let val = single_ok(b"{/* nothing */}");
    match &val {
        Val::Obj(obj) => assert!(obj.is_empty()),
        other => panic!("expected empty Obj, got {:?}", other),
    }
}

// --------------------------------------------------------------------------
// Edge cases: empty and minimal block comments
// --------------------------------------------------------------------------

#[test]
fn empty_block_comment() {
    assert_eq!(single_ok(b"/**/ 42"), Val::from(42isize));
}

#[test]
fn block_comment_only_whitespace() {
    assert_eq!(single_ok(b"/*   */ true"), Val::Bool(true));
}

#[test]
fn adjacent_block_comments() {
    assert_eq!(single_ok(b"/* a *//* b */ 42"), Val::from(42isize));
}

#[test]
fn block_comment_ending_double_star() {
    assert_eq!(single_ok(b"/** comment **/ 42"), Val::from(42isize));
}

// --------------------------------------------------------------------------
// Edge cases: comments containing other comment delimiters
// --------------------------------------------------------------------------

#[test]
fn hash_comment_containing_slashes() {
    assert_eq!(single_ok(b"# has // and /* inside\n42"), Val::from(42isize));
}

#[test]
fn slash_comment_containing_hash() {
    assert_eq!(single_ok(b"// has # inside\n42"), Val::from(42isize));
}

#[test]
fn slash_comment_containing_block_open() {
    assert_eq!(single_ok(b"// has /* inside\n42"), Val::from(42isize));
}

#[test]
fn block_comment_containing_slashes() {
    assert_eq!(single_ok(b"/* has // inside */ 42"), Val::from(42isize));
}

#[test]
fn block_comment_containing_hash() {
    assert_eq!(single_ok(b"/* has # inside */ 42"), Val::from(42isize));
}

// --------------------------------------------------------------------------
// Edge cases: strings that look like comments (must NOT be stripped)
// --------------------------------------------------------------------------

#[test]
fn string_with_hash_not_stripped() {
    assert_eq!(single_ok(b"\"# not a comment\""), Val::from("# not a comment".to_string()));
}

#[test]
fn string_key_with_slashes() {
    let val = single_ok(b"{\"//key\": 1}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("//key".to_string())), Some(&Val::from(1isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn string_value_with_path() {
    assert_eq!(
        single_ok(b"\"/path/to/file\""),
        Val::from("/path/to/file".to_string()),
    );
}

#[test]
fn string_with_star_slash() {
    assert_eq!(
        single_ok(b"\"end */ marker\""),
        Val::from("end */ marker".to_string()),
    );
}

// --------------------------------------------------------------------------
// Edge cases: multiple consecutive comments
// --------------------------------------------------------------------------

#[test]
fn many_consecutive_line_comments() {
    assert_eq!(
        single_ok(b"// one\n// two\n// three\n42"),
        Val::from(42isize),
    );
}

#[test]
fn alternating_comment_styles() {
    assert_eq!(
        single_ok(b"# a\n// b\n/* c */ # d\n// e\n42"),
        Val::from(42isize),
    );
}

// --------------------------------------------------------------------------
// Edge cases: whitespace and line endings
// --------------------------------------------------------------------------

#[test]
fn crlf_after_line_comment() {
    assert_eq!(single_ok(b"// comment\r\n42"), Val::from(42isize));
}

#[test]
fn tabs_around_block_comment() {
    assert_eq!(single_ok(b"\t/* comment */\t42"), Val::from(42isize));
}

#[test]
fn block_comment_at_eof_after_value() {
    assert_eq!(single_ok(b"42 /* trailing */"), Val::from(42isize));
}

// --------------------------------------------------------------------------
// Edge cases: deeply nested structures with comments
// --------------------------------------------------------------------------

#[test]
fn deeply_nested_with_comments() {
    let input = b"[/* a */ {\"k\": /* b */ [1, // c\n2]}]";
    let val = single_ok(input);
    match &val {
        Val::Arr(outer) => {
            assert_eq!(outer.len(), 1);
            match &outer[0] {
                Val::Obj(obj) => {
                    let inner = obj.get(&Val::from("k".to_string())).unwrap();
                    match inner {
                        Val::Arr(arr) => {
                            assert_eq!(arr.len(), 2);
                            assert_eq!(arr[0], Val::from(1isize));
                            assert_eq!(arr[1], Val::from(2isize));
                        }
                        other => panic!("expected inner Arr, got {:?}", other),
                    }
                }
                other => panic!("expected Obj, got {:?}", other),
            }
        }
        other => panic!("expected outer Arr, got {:?}", other),
    }
}

// --------------------------------------------------------------------------
// Edge cases: parse_many_jsonc
// --------------------------------------------------------------------------

#[test]
fn parse_many_only_comments() {
    let vals: Vec<_> = parse_many_jsonc(b"// nothing\n/* here */").collect();
    assert!(vals.is_empty());
}

#[test]
fn parse_many_comments_between_all_values() {
    let vals: Vec<Val> = parse_many_jsonc(b"/* a */ 1 /* b */ 2 /* c */ 3 /* d */")
        .collect::<Result<_, _>>()
        .unwrap();
    assert_eq!(vals, vec![Val::from(1isize), Val::from(2isize), Val::from(3isize)]);
}

#[test]
fn parse_many_line_comments_as_separators() {
    let vals: Vec<Val> = parse_many_jsonc(b"1\n// sep\n2\n// sep\n3")
        .collect::<Result<_, _>>()
        .unwrap();
    assert_eq!(vals, vec![Val::from(1isize), Val::from(2isize), Val::from(3isize)]);
}

#[test]
fn parse_many_error_mid_stream_recovers_not() {
    // After a bare `/` error, the iterator should stop (no further values)
    let results: Vec<_> = parse_many_jsonc(b"1 / 2").collect();
    assert!(results[0].is_ok());
    assert!(results[1].is_err());
}

// --------------------------------------------------------------------------
// Edge cases: incomplete comment errors in various positions
// --------------------------------------------------------------------------

#[test]
fn bare_slash_before_value_is_error() {
    single_err(b"/ 42");
}

#[test]
fn bare_slash_in_object_value_is_error() {
    single_err(b"{\"k\": /}");
}

#[test]
fn unterminated_block_in_array_is_error() {
    single_err(b"[1, /* forever");
}

#[test]
fn bare_slash_between_key_colon_is_error() {
    single_err(b"{\"k\" / : 1}");
}

#[test]
fn slash_followed_by_letter_is_error() {
    single_err(b"/x 42");
}

#[test]
fn slash_followed_by_number_is_error() {
    single_err(b"/1");
}

// --------------------------------------------------------------------------
// Edge cases: block comment boundary — `*` before EOF
// --------------------------------------------------------------------------

#[test]
fn block_comment_star_at_eof_is_error() {
    single_err(b"/* almost done *");
}

#[test]
fn block_comment_with_many_stars_no_close() {
    single_err(b"/* **** ");
}

// ==========================================================================
// JSON5: Trailing commas
// ==========================================================================

#[test]
fn json5_trailing_comma_array() {
    let val = json5_ok(b"[1, 2, 3,]");
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
fn json5_trailing_comma_object() {
    let val = json5_ok(b"{\"a\": 1, \"b\": 2,}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("a".to_string())), Some(&Val::from(1isize)));
            assert_eq!(obj.get(&Val::from("b".to_string())), Some(&Val::from(2isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_trailing_comma_nested() {
    let val = json5_ok(b"{\"a\": [1, 2,], \"b\": {\"c\": 3,},}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.len(), 2);
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_trailing_comma_single_element() {
    let val = json5_ok(b"[42,]");
    match &val {
        Val::Arr(arr) => {
            assert_eq!(arr.len(), 1);
            assert_eq!(arr[0], Val::from(42isize));
        }
        other => panic!("expected Arr, got {:?}", other),
    }
}

#[test]
fn json5_empty_array_no_comma() {
    let val = json5_ok(b"[]");
    match &val {
        Val::Arr(arr) => assert_eq!(arr.len(), 0),
        other => panic!("expected empty Arr, got {:?}", other),
    }
}

#[test]
fn json5_empty_object_no_comma() {
    let val = json5_ok(b"{}");
    match &val {
        Val::Obj(obj) => assert!(obj.is_empty()),
        other => panic!("expected empty Obj, got {:?}", other),
    }
}

// ==========================================================================
// JSON5: Single-quoted strings
// ==========================================================================

#[test]
fn json5_single_quoted_string() {
    assert_eq!(json5_ok(b"'hello'"), Val::from("hello".to_string()));
}

#[test]
fn json5_single_quoted_with_escapes() {
    assert_eq!(json5_ok(b"'hello\\nworld'"), Val::from("hello\nworld".to_string()));
}

#[test]
fn json5_single_quoted_with_escaped_quote() {
    assert_eq!(json5_ok(b"'it\\'s'"), Val::from("it's".to_string()));
}

#[test]
fn json5_single_quoted_with_double_quote() {
    assert_eq!(json5_ok(b"'say \"hi\"'"), Val::from("say \"hi\"".to_string()));
}

#[test]
fn json5_single_quoted_empty() {
    assert_eq!(json5_ok(b"''"), Val::from("".to_string()));
}

#[test]
fn json5_single_quoted_tab_escape() {
    assert_eq!(json5_ok(b"'a\\tb'"), Val::from("a\tb".to_string()));
}

#[test]
fn json5_single_quoted_unicode_escape() {
    // \u0041 = 'A'
    assert_eq!(json5_ok(b"'\\u0041'"), Val::from("A".to_string()));
}

#[test]
fn json5_unterminated_single_quoted_string() {
    json5_err(b"'unterminated");
}

#[test]
fn json5_single_quoted_as_object_key() {
    let val = json5_ok(b"{'key': 42}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("key".to_string())), Some(&Val::from(42isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

// ==========================================================================
// JSON5: Unquoted keys
// ==========================================================================

#[test]
fn json5_unquoted_key_simple() {
    let val = json5_ok(b"{name: \"jaq\"}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("name".to_string())), Some(&Val::from("jaq".to_string())));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_unquoted_key_with_underscore() {
    let val = json5_ok(b"{_private: 1}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("_private".to_string())), Some(&Val::from(1isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_unquoted_key_with_dollar() {
    let val = json5_ok(b"{$ref: 1}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("$ref".to_string())), Some(&Val::from(1isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_unquoted_key_with_digits() {
    let val = json5_ok(b"{abc123: true}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("abc123".to_string())), Some(&Val::Bool(true)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_unquoted_key_keyword() {
    // "null", "true", "false" as unquoted keys — these should be treated as identifiers, not values
    let val = json5_ok(b"{null: 1, true: 2, false: 3}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.len(), 3);
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_invalid_unquoted_key_starts_with_digit() {
    json5_err(b"{123abc: 1}");
}

// ==========================================================================
// JSON5: Hex numbers
// ==========================================================================

#[test]
fn json5_hex_lowercase() {
    assert_eq!(json5_ok(b"0xff"), Val::from(255isize));
}

#[test]
fn json5_hex_uppercase() {
    assert_eq!(json5_ok(b"0XFF"), Val::from(255isize));
}

#[test]
fn json5_hex_mixed_case() {
    assert_eq!(json5_ok(b"0x1A2b3C"), Val::from(0x1A2B3Cisize));
}

#[test]
fn json5_hex_negative() {
    assert_eq!(json5_ok(b"-0xff"), Val::from(-255isize));
}

#[test]
fn json5_hex_zero() {
    assert_eq!(json5_ok(b"0x0"), Val::from(0isize));
}

#[test]
fn json5_hex_no_digits_error() {
    json5_err(b"0x");
}

#[test]
fn json5_hex_in_object() {
    let val = json5_ok(b"{\"version\": 0xFF}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(
                obj.get(&Val::from("version".to_string())),
                Some(&Val::from(255isize))
            );
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

// ==========================================================================
// JSON5: Leading decimal
// ==========================================================================

#[test]
fn json5_leading_decimal() {
    let val = json5_ok(b".5");
    // Leading decimal is stored as Dec("0.5")
    assert_eq!(format!("{val}"), "0.5");
}

#[test]
fn json5_leading_decimal_with_exponent() {
    let val = json5_ok(b".5e2");
    assert_eq!(format!("{val}"), "0.5e2");
}

#[test]
fn json5_bare_dot_no_digit_error() {
    json5_err(b".abc");
}

// ==========================================================================
// JSON5: Trailing decimal
// ==========================================================================

#[test]
fn json5_trailing_decimal() {
    let val = json5_ok(b"5.");
    // Trailing decimal stored as Dec("5.")
    assert_eq!(format!("{val}"), "5.");
}

// ==========================================================================
// JSON5: Combined features
// ==========================================================================

#[test]
fn json5_combined_all_features() {
    let input = b"{
        name: 'jaq',
        version: 0xff,
        items: [1, 2, 3,],
        ratio: .5,
        // a comment
        count: 5.,
    }";
    let val = json5_ok(input);
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("name".to_string())), Some(&Val::from("jaq".to_string())));
            assert_eq!(obj.get(&Val::from("version".to_string())), Some(&Val::from(255isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

#[test]
fn json5_comments_inherited_from_jsonc() {
    let val = json5_ok(b"// header\n/* block */ {key: 42}");
    match &val {
        Val::Obj(obj) => {
            assert_eq!(obj.get(&Val::from("key".to_string())), Some(&Val::from(42isize)));
        }
        other => panic!("expected Obj, got {:?}", other),
    }
}

// ==========================================================================
// Plain JSON rejects JSON5 features
// ==========================================================================

#[test]
fn plain_json_rejects_trailing_comma() {
    parse_single(b"[1, 2,]").expect_err("plain JSON should reject trailing commas");
}

#[test]
fn plain_json_rejects_single_quoted_string() {
    parse_single(b"'hello'").expect_err("plain JSON should reject single-quoted strings");
}

#[test]
fn plain_json_rejects_unquoted_key() {
    parse_single(b"{key: 1}").expect_err("plain JSON should reject unquoted keys");
}

#[test]
fn plain_json_rejects_hex_number() {
    parse_single(b"0xff").expect_err("plain JSON should reject hex numbers");
}

#[test]
fn plain_json_rejects_leading_decimal() {
    parse_single(b".5").expect_err("plain JSON should reject leading decimal");
}

// ==========================================================================
// JSONC also rejects JSON5 features (only comments are added)
// ==========================================================================

#[test]
fn jsonc_rejects_trailing_comma() {
    single_err(b"[1, 2,]");
}

#[test]
fn jsonc_rejects_single_quoted_string() {
    single_err(b"'hello'");
}

#[test]
fn jsonc_rejects_unquoted_key() {
    single_err(b"{key: 1}");
}
