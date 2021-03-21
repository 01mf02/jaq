# An implementation of jaq's `fold` function for jq.
# This takes two filters:
# * init: produces initial values
# * f: produces a new accumulator value from an object {acc, x}, where
#      acc is the old accumulator value and x the current array element
def fold(init; f):
  def step:
    if .i == (.arr | length)
    # we have reached the end
    then empty
    # update the accumulator using f, and advance in the array
    else {acc: {acc, x: .arr[.i]} | f, i: (.i + 1), arr}
    end;
  [{acc: init, i: 0, arr: .} | last(recurse(step)) | .acc];
