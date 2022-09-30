#!/bin/bash
JQ=jq
JAQ=target/release/jaq
TIME='/usr/bin/time -f %U'

declare -a BENCHES=(
'1000000 | [range(.)] | reverse'
'1000000 | [range(.) | -.] | sort'
'1000000 | [range(.) | [.]] | add'
' 100000 | [range(.) | {(tostring): .}] | add'
'   5000 | [range(.) | {(tostring): .}] | add | .[] += 1'
' 100000 | [range(.) | {(tostring): .}] | add | with_entries(.value += 1)'
'1000000 | [limit(.; repeat("a"))] | add | explode | implode'
'1000000 | reduce range(.) as $x ([]; . + [$x + .[-1]])'
'     16 | nth(.; 0 | recurse([., .])) | flatten'
'     16 | nth(.; 0 | recurse([., .])) | (.. | scalars) |= .+1'
' 100000 | [range(.) | tojson] | join(",") | "[" + . + "]" | fromjson'
)


echo -n '|' '`empty` (100 iterations)'
echo -n '|' $($TIME bash -c "for n in {1..100}; do $JAQ -n 'empty'; done" 2>&1)
echo -n '|' $($TIME bash -c "for n in {1..100}; do $JQ  -n 'empty'; done" 2>&1)
echo    '|'

for val in "${BENCHES[@]}"; do
	echo -n	'|' \`$val\`
	echo -n '|' $($TIME $JAQ -n "$val | length" 2>&1 > /dev/null)
	echo -n '|' $($TIME $JQ  -n "$val | length" 2>&1 > /dev/null)
	echo    '|'
done
