# Brainfuck interpreter
#
#  Input: a string containing the Brainfuck program
# Output: a string containing its output
#
# Example usage:
#
#     jaq -sRrf examples/bf.jq examples/fib.bf
#
# Adapted by Michael Färber from <https://github.com/itchyny/brainfuck>

def skip_loop: last(recurse(
  .input[.cursor:.cursor+1] as $c |
  .cursor += 1 |
  if $c == "[" then .depth += 1
  elif $c == "]" then .depth -= 1 | select(.saved_depth <= .depth)
  elif $c == "" then error("unmatching loop")
  else .
  end)) | .cursor += 1 | .depth -= 1;

def backward_loop: last(recurse(
  .input[.cursor:.cursor+1] as $c |
  .cursor -= 1 |
  if $c == "[" then .depth -= 1 | select(.saved_depth < .depth)
  elif $c == "]" then .depth += 1
  elif .cursor < 0 then error("unmatching loop")
  else .
  end)) | .depth -= 1;

# Given an array, assure that it has at least length i+1 by filling it up with `null`,
# then update the i-th position with f
def assign(i; f):
  if i < length then .[i] |= f
  # this yields different output in jq due to its implementation of `limit`,
  # where `limit(0; f)` yields the first element of `f` (instead of no elements)
  else . + [limit(i - length; repeat(null)), (null | f)] end;

{ input: ., cursor: 0, memory: [], pointer: 0, depth: 0, output: [] } |
  until(
    .cursor >= (.input | length);
    .input[.cursor:.cursor+1] as $c |
    .cursor += 1 |
    if $c == ">" then .pointer += 1
    elif $c == "<" then .pointer -= 1 | if .pointer < 0 then error("negative pointer") else . end
    elif $c == "+" then .pointer as $p | .memory |= assign($p; (. + 1) % 256)
    elif $c == "-" then .pointer as $p | .memory |= assign($p; (. + 255) % 256)
    elif $c == "." then .memory[.pointer] as $m | .output += [$m]
    elif $c == "," then error(", is not implemented")
    elif $c == "[" then .depth += 1 | if .memory[.pointer] > 0 then . else .saved_depth = .depth | skip_loop end
    elif $c == "]" then .depth -= 1 | .cursor -= 1 | .saved_depth = .depth | backward_loop
    else .
    end
  ) | .output | implode
