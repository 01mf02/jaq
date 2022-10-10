#!/bin/bash

# Make sure that you are running a version of jq newer than 1.6,
# otherwise this benchmark will likely take a looooooooong time.

: "${JQ:=jq}"
: "${JAQ:=target/release/jaq}"

TIME='/usr/bin/time -f %U'

# 2^16 =   65536
# 2^17 =  131072
# 2^20 = 1048576
declare -a BENCHES=(
'1048576 | [range(.)] | reverse'
'1048576 | [range(.) | -.] | sort'
'1048576 | [range(.) | [.]] | add'
' 131072 | [range(.) | {(tostring): .}] | add'
' 131072 | [range(.) | {(tostring): .}] | add | .[] += 1'
' 131072 | [range(.) | {(tostring): .}] | add | with_entries(.value += 1)'
'1048576 | [limit(.; repeat("a"))] | add | explode | implode'
'1048576 | reduce range(.) as $x ([]; . + [$x + .[-1]])'
'     17 | nth(.; 0 | recurse([., .])) | flatten'
'     17 | nth(.; 0 | recurse([., .])) | (.. | scalars) |= .+1'
'  65536 | [range(.) | tojson] | join(",") | "[" + . + "]" | fromjson'
)


echo -n '|' '`empty` (512 iterations)'
echo -n '|' $($TIME bash -c "for n in {1..512}; do $JAQ -n 'empty'; done" 2>&1)
echo -n '|' $($TIME bash -c "for n in {1..512}; do $JQ  -n 'empty'; done" 2>&1)
echo    '|'

echo -n '|' '`bf-fib`'
echo -n '|' $($TIME $JAQ -sRrf examples/bf.jq examples/fib.bf 2>&1 > /dev/null)
echo -n '|' $($TIME $JQ  -sRrf examples/bf.jq examples/fib.bf 2>&1 > /dev/null)
echo    '|'

for val in "${BENCHES[@]}"; do
	echo -n	'|' \`$val\`
	echo -n '|' $($TIME $JAQ -n "$val | length" 2>&1 > /dev/null)
	echo -n '|' $($TIME $JQ  -n "$val | length" 2>&1 > /dev/null)
	echo    '|'
done
