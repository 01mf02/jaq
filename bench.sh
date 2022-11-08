#!/bin/bash
#
# Benchmark jq implementations passed as arguments
#
# Example usage:
#
#     ./bench.sh target/release/jaq jq
#
# Make sure that you are running a version of jq newer than 1.6,
# otherwise this benchmark will likely take a looooooooong time.

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

echo -n '|Benchmark|n'
for j in $@; do echo -n '|' $j; done
echo '|'
echo -n '|-|-'
for j in $@; do echo -n '|-'; done
echo '|'

echo -n '|empty|512'
for j in $@; do echo -n '|' $($TIME bash -c "for n in {1..512}; do $j -n 'empty'; done" 2>&1); done
echo    '|'

echo -n '|bf-fib|13'
for j in $@; do echo -n '|' $($TIME $j -sRrf examples/bf.jq examples/fib.bf 2>&1 > /dev/null); done
echo    '|'

while read -r line; do
  b=`echo $line | $1 -r .name`
  n=`echo $line | $1 .n`
  echo -n "|$b|$n"
  for j in $@; do
    echo -n '|' $(echo $n | $TIME $j "$(cat examples/$b.jq) | length" 2>&1 > /dev/null)
  done
  echo '|'
done <examples/benches.json
