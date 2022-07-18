#!/bin/bash
JQ=jq
JAQ=target/release/jaq
TIME='/usr/bin/time -f %U'

declare -a BENCHES=(
'[range(1000000)] | reverse | length'
'[range(1000000) | -.] | sort | length'
'[range(1000000) | [.]] | add | length'
'[range(100000) | {(tostring): .}] | add | length'
'[range(  5000) | {(tostring): .}] | add | .[] += 1 | length'
'[range(100000) | {(tostring): .}] | add | with_entries(.value += 1) | length'
'[limit(1000000; repeat("a"))] | add | explode | implode | length'
'reduce range(1000000) as $x ([]; . + [$x + .[-1]]) | length'
'def trees: recurse([., .]); 0 | nth(16; trees) | flatten | length'
'def trees: recurse([., .]); 0 | nth(16; trees) | (.. | scalars) |= .+1 | length'
'"[" + ([range(100000) | tojson] | join(",")) + "]" | fromjson | add'
)

echo -n '|' '`empty` (128 iterations)'
echo -n '|' $($TIME bash -c "for n in {1..128}; do $JAQ -n 'empty'; done" 2>&1)
echo -n '|' $($TIME bash -c "for n in {1..128}; do $JQ  -n 'empty'; done" 2>&1)
echo    '|'

for val in "${BENCHES[@]}"; do
	echo -n	'|' \`$val\`
	echo -n '|' $($TIME $JAQ -n "$val" 2>&1 > /dev/null)
	echo -n '|' $($TIME $JQ  -n "$val" 2>&1 > /dev/null)
	echo    '|'
done
