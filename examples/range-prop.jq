# verifies that for a range of inputs,
# range($from; $upto; $by) yields
# max(0, ⌈($upto - $from) / $by⌉) outputs
# (if $by is not 0 and $upto, $from, and $by are integer)
[
  { from: 1, upto: range(-.; .), by: range(-.; .) | select(. != 0) } |
  ([range(.from; .upto; .by)] | length) ==
  ([(.upto - .from) / .by | ceil, 0] | max)
]
