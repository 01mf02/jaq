# An implementation of jaq's `fold` function for jq.
# This takes two filters:
# * init: produces initial values
# * xs: produces "current" values
# * f: produces a new accumulator value from an array [acc, x], where
#      acc is the old accumulator value and x the current array element
def fold(init; xs; f): reduce xs as $x (init; [., $x] | f);
