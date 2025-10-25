# Advanced features


## Assignments

Like jq, jaq allows for assignments of the form `p |= f`.
However, jaq interprets these assignments differently.
Fortunately, in most cases, the result is the same.

In jq, an assignment `p |= f` first constructs paths to all values that match `p`.
*Only then*, it applies the filter `f` to these values.

In jaq, an assignment `p |= f` applies `f` *immediately* to any value matching `p`.
Unlike in jq, assignment does not explicitly construct paths.

jaq's implementation of assignment likely yields higher performance,
because it does not construct paths.
Furthermore, this allows jaq to use multiple outputs of the right-hand side, whereas
jq uses only the first.
For example,
`0 | (., .) |= (., .+1) --> 0 1 1 2` in jaq,
whereas it yields only `0` in jq.
However,
`{a: 1} | .a |= (2, 3) --> {"a": 2}` in both jaq and jq,
because an object can only associate a single value with any given key,
so we cannot use multiple outputs in a meaningful way here.

Because jaq does not construct paths,
it does not allow some filters on the left-hand side of assignments,
for example `first`, `last`, `limit`:
For example, `[1, 2, 3] | first(.[]) |= .-1`
yields `[0, 2, 3]` in jq, but is invalid in jaq.
Similarly, `[1, 2, 3] | limit(2; .[]) |= .-1`
yields `[0, 1, 3]` in jq, but is invalid in jaq.
(Inconsequentially, jq also does not allow for `last`.)

::: Compatibility
In jq, `[0, 1] | .[3] = 3` yields `[0, 1, null, 3]`; that is,
jq fills up the list with `null`s if we update beyond its size.
In contrast, jaq fails with an out-of-bounds error in such a case.
:::

### Equivalences

Unary:

- `.  |= f`: `f`
- `.. |= f`: `def rec_up: (.[]? | rec_up), .; rec_up |= f`

Binary:

- `(l | r) |= f`: `l |= (r |= f)`
- `(l , r) |= f`: `l |= f | r |= f`
- `(l as $x | r) |= f`: `(l1 as $x | r) |= f | ... | (ln as $x | r) |= f`
  (assuming that `l` yields outputs `l1`, ..., `ln`)
- `l // r |= f`: `if first(l // false) then l |= f else r |= f`

It follows from the above definitions that
`empty |= f` is equivalent to `.`.

Keywords:

- `if $p then t else e end |= f`: `if $p then t |= f else e |= f end`

Path operators:

- `.[] |= f`:

  - Array: `[.[] | f]`
  - Object: `with_entries(.[].value |= f)`
- `.[$i] |= f`:

  - Array (if `0 <= $i < length`): `.[:$i] + [.[$i] | first(f)] + .[$i+1:]`
  - Array (if `-length <= $i < 0`): `.[length + $i] |= f`
  - Object (if `has($i)`): `with_entries(.[] |= if .key == $i then {key, value: first(.value | f)})`
  - Object (if `has($i) | not`): `. + {($i): first(null | f)}`

The operators above throw an error if they encounter an unhandled case, whereas
the variants `.[]? |= f` and `.[$i] |= f` return their input in that case.


## Patterns

`. as {a: [$x, {("b", "c"): $y, $z}]} | $x, $y, $z`


## Modules

### `include "path";`

### `import "path" as mod;`

### `import "path" as $data;`


## Comments
