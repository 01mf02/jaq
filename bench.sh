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

TIME='timeout 10 /usr/bin/time -f %U'
NA='echo N/A'

echo -n '|Benchmark|n'
for j in $@; do echo -n '|' $j; done
echo '|'
echo -n '|-|-:'
for j in $@; do echo -n '|-:'; done
echo '|'

echo -n '|empty|512'
for j in $@; do
  echo -n '|' $($TIME bash -c "for n in {1..512}; do $j -n 'empty'; done" 2>&1 || $NA)
done
echo '|'

echo -n '|bf-fib|13'
for j in $@; do
  echo -n '|' $($TIME $j -sRrf examples/bf.jq examples/fib.bf 2>&1 > /dev/null || $NA)
done
echo '|'

while read -r line; do
  b=`echo $line | $1 -r .name`
  n=`echo $line | $1 .n`
  echo -n "|$b|$n"
  for j in $@; do
    echo -n '|' $(echo $n | $TIME $j "$(cat examples/$b.jq) | length" 2>&1 > /dev/null || $NA)
  done
  echo '|'
done <examples/benches.json
# 2^16 =   65536
# 2^17 =  131072
# 2^20 = 1048576
