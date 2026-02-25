#!/bin/bash
# End-to-end integration tests for JSONC comment support.
set -euo pipefail

JAQ="./target/debug/jaq"
PASS=0
FAIL=0

assert_eq() {
    local desc="$1" got="$2" expected="$3"
    if [ "$got" = "$expected" ]; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc"
        echo "    expected: $expected"
        echo "    got:      $got"
        FAIL=$((FAIL + 1))
    fi
}

assert_err() {
    local desc="$1" input="$2" filter="$3"
    if echo "$input" | $JAQ "$filter" >/dev/null 2>&1; then
        echo "  FAIL: $desc (expected error, got success)"
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    fi
}

echo "=== JSONC Integration Tests ==="

# --- Single-line comments ---
echo "--- // single-line comments ---"

got=$(printf '{"name": "test" // comment\n}' | $JAQ '.name')
assert_eq "// after value" "$got" '"test"'

got=$(printf '// header\n42' | $JAQ '.')
assert_eq "// before value" "$got" "42"

got=$(echo 'true // trailing' | $JAQ '.')
assert_eq "// at end of line" "$got" "true"

# --- Block comments ---
echo "--- /* */ block comments ---"

got=$(echo '{"name": /* inline */ "test"}' | $JAQ '.name')
assert_eq "/* */ inline" "$got" '"test"'

got=$(printf '{\n  /* multi\n     line */\n  "x": 1\n}' | $JAQ '.x')
assert_eq "/* */ multiline" "$got" "1"

got=$(echo '/* ** stars ** */ 42' | $JAQ '.')
assert_eq "/* */ with extra stars" "$got" "42"

# --- Comments in strings (must be preserved) ---
echo "--- comments inside strings ---"

got=$(echo '{"url": "http://example.com"}' | $JAQ '.url')
assert_eq "// in URL string preserved" "$got" '"http://example.com"'

got=$(echo '{"s": "/* not a comment */"}' | $JAQ '.s')
assert_eq "/* */ in string preserved" "$got" '"/* not a comment */"'

# --- Mixed comment styles ---
echo "--- mixed comments ---"

got=$(printf '# hash\n// slash\n/* block */ {"v": 99}' | $JAQ '.v')
assert_eq "mixed #, //, /* */" "$got" "99"

# --- .jsonc file with auto-detection ---
echo "--- .jsonc file extension ---"

TMP=$(mktemp /tmp/test.XXXX.jsonc)
cat > "$TMP" << 'JSONC'
{
    // Database config
    "host": "localhost",
    /* Port for the
       database connection */
    "port": 5432
}
JSONC

got=$($JAQ '.host' "$TMP")
assert_eq ".jsonc file: .host" "$got" '"localhost"'
got=$($JAQ '.port' "$TMP")
assert_eq ".jsonc file: .port" "$got" "5432"
rm -f "$TMP"

# --- --from jsonc flag ---
echo "--- --from jsonc flag ---"

TMP=$(mktemp /tmp/test.XXXX.txt)
cat > "$TMP" << 'JSONC'
{
    // This is a .txt file but we force jsonc
    "key": "value"
}
JSONC

got=$($JAQ --from jsonc '.key' "$TMP")
assert_eq "--from jsonc with .txt file" "$got" '"value"'
rm -f "$TMP"

# --- Edge case: unterminated comment ---
echo "--- unterminated block comment ---"

# An unterminated block comment consumes all remaining input.
# With no value before it, jaq produces no output (exit 0, like empty input).
# With a value before it, jaq processes that value normally.
got=$(echo '42 /* unterminated' | $JAQ '.')
assert_eq "value before unterminated comment" "$got" "42"

# --- Summary ---
echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
