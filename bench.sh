#!/bin/bash
#
# Benchmark jq implementations passed as arguments
#
# Example usage:
#
#     ./bench.sh target/release/jaq jq

TIME='timeout 10 /usr/bin/time -f %U'

echo -n '{"name": "empty", "n": 512, "time": {'
for j in $@; do
  t=$($TIME bash -c "for n in {1..512}; do $j -n 'empty'; done" 2>&1)
  [ $j != $1 ] && echo -n ', '
  echo -n '"'$j'": ['$t']'
done
echo '}}'

echo -n '{"name": "bf-fib", "n": 13, "time": {'
for j in $@; do
  t=$($TIME $j -sRrf examples/bf.jq examples/fib.bf 2>&1 > /dev/null)
  [ $j != $1 ] && echo -n ', '
  echo -n '"'$j'": ['$t']'
done
echo '}}'

echo -n '{"name": "defs", "n": 100000, "time": {'
for j in $@; do
  t=$($TIME $j -n -f <(for i in `seq 100000`; do echo "def a: 0;"; done; echo empty) 2>&1 > /dev/null)
  [ $? != 0 ] && t="" # on error
  [ $j != $1 ] && echo -n ', '
  echo -n '"'$j'": ['$t']'
done
echo '}}'

while read -r line; do
  b=`echo $line | $1 -r .name`
  n=`echo $line | $1 .n`
  echo -n '{"name": "'$b'", "n": '$n', "time": {'
  for j in $@; do
    [ $j != $1 ] && echo -n ', '
    echo -n '"'$j'": ['
    for i in `seq 3`; do
      t=$(echo $n | $TIME $j "$(cat examples/$b.jq) | length" 2>&1 > /dev/null)
      [ -z "$t" ] && break # terminate on timeout
      [ $i -ne 1 ] && echo -n ', '
      echo -n $t
    done
    echo -n ']'
  done
  echo '}}'
done <examples/benches.json
# 2^16 =   65536
# 2^17 =  131072
# 2^20 = 1048576
