# Standard library {#stdlib}

## Basic

### `empty`

The filter `empty` yields no output.

::: Advanced

This filter is defined as `([][] as $x | .)`.
While a simpler filter like `[][]` also yields no outputs,
this rather contrived-looking definition guarantees that
`empty` can be used on the left-hand side of [assignments](#assignments).
This comes into play when you use [`select(p)`](#select),
which uses `empty` under the hood.

:::

### `error`, `error(f)` {#error}

The filter `error` takes its input and throws an error with the input as payload.
The filter `error(f)` is equivalent to `f | error`.

For example,
`try error(41) catch (. + 1) --> 42`.

### `length`

The output of the filter `length` depends on its input type:

- `null`: `0`, i.e. `null | length --> 0`
- boolean: error, i.e. `true | try length catch "fail" --> "fail"`
- number: the absolute value of the number, i.e. `-1, 1 | length --> 1 1`
- text string: the number of characters, i.e. `"ゼノギアス" | length --> 5`
- byte string: the number of bytes, i.e. `"ゼノギアス" | tobytes | length --> 15`
- array: the number of values, i.e. `[1, [2, 3], 4] | length --> 3`
- object: the number of key-value pairs, i.e. `{a: 0, b: 1} | length --> 2`

### `keys`, `keys_unsorted` {#keys}

The filter `keys_unsorted` yields an array that contains
all keys if the input is an object or
all indices if the input is an array.
The filter `keys` is equivalent to `keys_unsorted | sort`.
For example:

- `{c: 1, b: 2, a: 1} | keys_unsorted --> ["c", "b", "a"]`
- `{c: 1, b: 2, a: 1} | keys          --> ["a", "b", "c"]`
- `[1, 2, 3] | keys_unsorted --> [0, 1, 2]`
- `[1, 2, 3] | keys          --> [0, 1, 2]`

::: Advanced

The filter `keys_unsorted` is equivalent to
`to_entries | .[] |= .key`; for example,
`{a: 1, b: 2} | to_entries | .[] |= .key --> ["a", "b"]`.

:::

### `map(f)`, `map_values(f)` {#map}

The filter `map(f)` obtains all values of the input (via `.[]`),
applies `f` to the values, and collects all results into an array.
For example:

- `[1, 2, 3]    | map(., .*2) --> [1, 2, 2, 4, 3, 6]`.
- `{a: 1, b: 2} | map(., .*2) --> [1, 2, 2, 4]`.

The filter `map_values(f)` has the same effect as `map(f)`
when the input is an array, but when the input is an object,
`map_values(f)` also outputs an object.
For example:

- `[1, 2, 3, 4] | map_values(.*2) --> [2, 4, 6, 8]`
- `{a: 1, b: 2} | map_values(.*2) --> {"a": 2, "b": 4}`

::: Advanced

The filter `map(f)` is equivalent to `[.[] | f]` and
the filter `map_values(f)` is equivalent to `.[] |= f`.

:::

### `to_entries`, `from_entries`, `with_entries(f)` {#entries}

The filter `to_entries` takes as input an array or an object.
It converts them to an array of objects of the shape
`{key: k, value: v}`, such that `.[k]` on the original input yields `v`.
For example:

- `[   1,    2] | to_entries --> [{"key":  0 , "value": 1}, {"key":  1 , "value": 2}]`
- `{a: 1, b: 2} | to_entries --> [{"key": "a", "value": 1}, {"key": "b", "value": 2}]`

The filter `from_entries` constructs an object from
an array of entries as given by `to_entries`.
For example, `{a: 1, b: 2} | to_entries | from_entries --> {"a": 1, "b": 2}`.

The filter `with_entries(f)` is equivalent to `to_entries | map(f) | from_entries`.
For example, `{"a": 1, "b": 2} | with_entries(.key |= ascii_upcase) --> {"A": 1, "B": 2}`

### `not`

The filter `not` converts its input to its [boolean value](#booleans) and returns its negation.
For example:

- `true  | not --> false`
- `false | not --> true`
- `"foo" | not --> false`

::: Advanced

The filter `not` is equivalent to `if . then false else true end`.
We can obtain the boolean value of a value by `not | not`.

:::

### `type`

The filter `type` returns the type of its input value as string. For example:

- `null  | type --> "null"`
- `false | type --> "boolean"`
- `0     | type --> "number"`
- `"foo" | type --> "string"`
- `[1]   | type --> "array"`
- `{}    | type --> "object"`

Note that both text strings and byte strings both have the same type `"string"`.

::: Advanced

The `type` filter can be relatively slow to run;
if you use it for simple comparisons such as
`type == "string"`, then you can also use filters like
[`isstring`](#istype).

:::

## Stream consumers

### `first`, `first(f)`, `last`, `last(f)` {#first-last}

The filter `first(f)` yields the first output of `f` if there is one, else nothing.
For example,
`first(1, 2, 3) --> 1` and
`first(empty) -->` (no output).

This filter stops evaluating `f` after the first output, meaning that
it yields an output even if `f` yields infinitely many outputs.
For example,
`first(repeat(0)) --> 0` and
`first(1, def f: f; f) --> 1`.

Similarly, `last(f)` yields the last output of `f` if there is one, else nothing.
If `f` yields an error, then the first error of `f` is yielded.
For example,
`last(1, 2, 3) --> 3`,
`last(empty) -->` (no output), and
`try last(1, error("fail"), 3) catch . --> "fail"`.

The filters `first` and `last` are short forms for
`first(.[])` and `last(.[])`, respectively.
You can use them to retrieve the first/last element of an array, such as
`[1, 2, 3] | first, last --> 1 3`.

### `limit($n; f)` {#limit}

The filter `limit($n; f)` yields the first `$n$` outputs of `f`.
If `$n <= 0`, it yields no outputs.
For example:

- `limit( 3; 1, 2      ) --> 1 2`
- `limit( 3; 1, 2, 3, 4) --> 1 2 3`
- `limit(-1; 1, 2      ) -->` (no output)

::: Compatibility

When `$n < 0`, `jq` yields an error instead.

:::

### `skip($n; f)` {#skip}

The filter `skip($n; f)` yields all outputs after the first `$n$` outputs of `f`.
If `$n <= 0`, it yields all outputs of `f`.
For example:

- `skip( 3; 1, 2      ) -->` (no output)
- `skip( 1; 1, 2, 3, 4) --> 2 3 4`
- `skip(-1; 1, 2      ) --> 1 2`

### `nth($i)`, `nth($i; f)` {#nth}

The filter `nth($i; f)` yields the `$i`-th output of `f`.
If `f` yields less than `$i` outputs, then this filter yields no output.
For example:

- `nth(0; 1, 2, 3) --> 1`
- `nth(2; 1, 2, 3) --> 3`
- `nth(3; 1, 2, 3) -->` (no output)

The filter `nth($i)` is a short form for `.[$i]`; e.g.
`[1, 2, 3] | nth(0) --> 1`.


## Stream generators

### `range($upto)`, `range($from; $upto)`, `range($from; $upto; $step)` {#range}

The filter `range($from; $upto; $step)`
adds `$step` to `$from` until it exceeds `$upto`.
For example:

- `range(1;  9;  2) --> 1 3 5 7`
- `range(1; 10;  2) --> 1 3 5 7 9`
- `range(9;  1; -2) --> 9 7 5 3`
- `range(9;  0; -2) --> 9 7 5 3 1`

The filter `range($from; $upto)` is a short form of `range($from; $upto; 1)` and
the filter `range($upto)` is a short form of `range(0; $upto)`.
For example:

- `range(5) --> 0 1 2 3 4`
- `range(2; 5) --> 2 3 4`

::: Compatibility

In `jq`, `range/1` and `range/2` are more restrictive versions of `range/3`
that prohibit non-numeric arguments.

:::

::: Advanced

The filter is equivalent to:

~~~
def range($from; $to; $by): $from |
   if $by > 0 then while(.  < $to; . + $by)
 elif $by < 0 then while(.  > $to; . + $by)
   else            while(. != $to; . + $by)
   end;
range(1; 10; 2) -->
1 3 5 7 9
~~~

For that reason, we can also use it with other values than numbers:

- `range(""; "aaa"; "a") --> "" "a" "aa"`
- `range([]; [1, 1, 1]; [1]) --> [] [1] [1, 1]`

This makes it quite easy to accidentally create an infinite sequence, e.g. by
`range(""; "b"; "a")`.

:::

### `recurse`, `recurse(f)`

The filter `recurse(f)` is equivalent to `., (f | recurse(f))`.
It first outputs its input, then runs `f` and `recurse(f)` on its outputs.
This is useful to create infinite sequences.
You can create a finite sequence by having `f` return `empty`, e.g. via `select`.
For example:

- `0 | limit(5; recurse(.+1))       --> 0 1 2 3 4`
- `0 | recurse(.+1 | select(. < 5)) --> 0 1 2 3 4`

The filter `recurse(f; p)` is equivalent to `recurse(f | select(p))`.
That means that it recurses only on
outputs of `f` for which `p` yields a `true` output.
For example:

- `0 | recurse(.+1; . < 5) --> 0 1 2 3 4`

The filter `recurse` is a short form for `recurse(.[]?)`.
It returns all values recursively contained in the input, e.g.
`[1, [2], {a: 3}] | recurse --> [1, [2], {"a": 3}] 1 [2] 2 {"a":3} 3`.


## Selection

The filters in this section classify their inputs or output them selectively.

### `select(p)` {#select}

The filter `select(p)` yields its input if `p` yields a `true` value for it.
For example,
`(0, 1, -1, 2, -2) | select(. >= 0) --> 0 1 2`.

### `nulls`, `booleans`, `numbers`, `strings`, `arrays`, `objects` {#select-type}

Any of these filters yields its input if it is of the given type, else nothing.
For example:

- `null, true, 0, "Hi!", [1, 2], {a: 1} | nulls    --> null`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | booleans --> true`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | numbers  --> 0`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | strings  --> "Hi!"`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | arrays   --> [1, 2]`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | objects  --> {"a": 1}`

::: Advanced

These filters are equivalent to
`select(. == null)`,
`select(isboolean)`,
...,
`select(isobject)`.

:::

### `isboolean`, `isnumber`, `isstring`, `isarray`, `isobject` {#istype}

For every filter in this section, like `isboolean`, ..., `isobject`, there is
a corresponding filter in the previous section like, `booleans`, ..., `objects`.
Any of these filters yields `true` if
its corresponding filter in the previous section yields some output, else `false`.
For example:

- `null | isboolean --> false`, because `null | booleans -->` (no output).
- `true | isboolean --> true `, because `true | booleans --> true`.

::: Compatibility

`jq` does not implement these filters.

:::

### `normals`, `finites` {#select-number}

These filters return its input if
`isnormal` or `isfinite` is `true`, respectively, else `false`.

### `values`, `iterables`, `scalars`

The filter `values` yields its input if it is *not* `null`, else nothing.

If a value is either an array or an object,
it is said to be *iterable*; otherwise,
it is said to be *scalar*.
(The [iteration](#iterating) filter `.[]` succeeds on any iterable value,
whereas it fails on any scalar.)

The filters `iterables` and `scalars` yield their input if
it is iterable or scalar, respectively, else nothing.

Examples:

- `null, true, 0, "Hi!", [1, 2], {a: 1} | values    --> true 0 "Hi!" [1, 2] {"a": 1}`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | scalars   --> null true 0 "Hi!"`
- `null, true, 0, "Hi!", [1, 2], {a: 1} | iterables --> [1, 2] {"a": 1}`

### `isnan`, `isinfinite`, `isfinite`, `isnormal` {#isnumber}

The filter `isnan` yields `true` if
its input is `NaN`, else `false`.
Note that it is *not* equivalent to `. == nan`, because
`nan` is not equal to itself; see [equality](#equality).

The filter `isinfinite` yields `true` if
its input is either `Infinity` or `-Infinity`, else `false`.

The filter `isfinite` yields `true` if
its input is a number that is not infinite, else `false`.

The filter `isnormal` yields `true` if
its input is a number that is neither `0`, `NaN`, nor infinite.

Examples:

- `nan      | isnan, isinfinite, isfinite, isnormal --> true  false true  false`
- `infinite | isnan, isinfinite, isfinite, isnormal --> false true  false false`
- `0        | isnan, isinfinite, isfinite, isnormal --> false false true  false`
- `1        | isnan, isinfinite, isfinite, isnormal --> false false true  true`


## Membership

### `contains($x)`, `inside($x)`

The filter `contains($x)` yields `true`
if any of the following conditions holds, else `false`.

- The input is a string and `$x` is a substring of it.
- The input is an array, `$x` is an array, and
  for every value `v` in `$x`,
  there is some value in the input that `contains(v)`.
- The input is an object, `$x` is an object, and
  for every key-value pair `{k: v}` in `$x`,
  there is a value for the key `k` in the input that `contains(v)`.
- The input is `null`, boolean, or a number, and `$x` is equal to the input.

Examples:

- `"Hello, world!" | contains("world")  --> true`
- `[1, 2, 3]       | contains([1, 3])   --> true`
- `[[1, 2], 3]     | contains([3, [1]]) --> true`
- `{a: 1, b: 2}    | contains({a: 1})   --> true`
- `{a: [1, 2]}     | contains({a: [1]}) --> true`
- `0               | contains(0)        --> true`

The filter `inside($x)` is a flipped version of `contains`.
For example,
`"world" | inside("Hello, world") --> true`.

::: Advanced

The filter `inside($x)` is equivalent to `. as $i | $x | contains($i)`.

:::

### `indices($x)`

The filter `indices($x)` yields the following:

- If the input and `$x` are either both strings or both arrays, then it yields
  the indices `i` for which `.[i:][:$x | length] == $x`; e.g.
  `"Alice, Bob, and Carol" | indices(", ") --> [5, 10]` and
  `[0, 1, 2, 3, 1, 2, 3] | indices([1, 2]) --> [1, 4]`.
- If the input is an array and `$x` is not an array, then it yields
  the indices `i` for which `.[i] == $x`; e.g.
  `[0, 1, 2, 3, 1, 2, 3] | indices(1) --> [1, 4]`.
- Otherwise, it yields an error.

This means that `[[1, 2], 3] | indices([1, 2]) --> []`, because
the input array has neither `1` nor `2`, just `[1, 2]` and `3`.

::: Advanced

We can verify the property given above:

~~~
def verify($x): all(indices($x)[] as $i | .[$i:][:$x | length]; . == $x);
("Alice, Bob, and Carol" | verify(", "  )),
([0, 1, 2, 3, 1, 2, 3]   | verify([1, 2]))
--> true true
~~~

:::

### `index($x)`, `rindex($x)` {#index}

The filters `index($x)` and `rindex($x)` are shorthand for
`indices($x) | first` and
`indices($x) | last`, respectively.
For example:

- `"Alice, Bob, and Carol" | index(", ") --> 5`
- `[0, 1, 2, 3, 1, 2, 3] | rindex([1, 2]) --> 4`
- `"Hello world!" | index(", ") --> null`
- `[0, 1, 2] | rindex(3) --> null`

### `has($k)`, `in($x)`

The filter `has($k)` yields `true` if
`$k` is among the keys of the input, else `false`.
For example:

- `[1, 2, 3]    | has( 0 ,  3 ) --> true false`
- `{a: 1, b: 2} | has("a", "c") --> true false`

The filter `in($x)` is a flipped version of `has`, just like
`inside` is a flipped version of `contains`.
For example,
`"a" | in({a: 1, b: 2}) --> true`.

::: Advanced

The filter `has($k)` is a more performant version of `any(keys[]; . == $k)`.
For example:

- `[1, 2, 3]    | any(keys[]; . ==  0 ) --> true`
- `{a: 1, b: 2} | any(keys[]; . == "a") --> true`

:::


## Paths

### `path(f)`

### `del(f)`

### `pick(f)`


## Reduction

### `add`, `add(f)`

The filter `add(f)` yields the sum of all elements yielded by `f`, or
`null` if `f` yields no outputs.
For example:

- `add(1, 2, 3) --> 6`
- `add(empty) --> null`

The filter `add` is a short form of `add(.[])`.
You can use it to add all values of an array or object:

- `[1, 2, 3]    | add --> 6`
- `{a: 1, b: 2} | add --> 3`

::: Advanced

The filter `add(f)` is equivalent to
`reduce f as $x (null; . + $x)`. For example:
`reduce (1, 2, 3) as $x (null; . + $x) --> 6`.

:::

### `any`, `any(p)`, `any(f; p)`

The filter `any(f; p)` yields `true` if
any output of `f | p` has the boolean value `true`, else `false`.
For example:

- `any(0, 1, 2; . == 42) --> false`
- `any(0, 1, 2; . == 42, . == 2) --> true`

The filters `any(p)` and `any` are short forms of
`any(.[]; p)` and `any(.)`, respectively.
For example:

- `[1, 2, 3] | any(. % 2 == 0) --> true`
- `[false, true] | any --> true`

### `all`, `all(p)`, `all(f; p)`

The filter `all(f; p)` yields `true` if
all outputs of `f | p` have the boolean value `true`, else `false`.
For example:

- `all(0, 1, 2; . >  0) --> false`
- `all(0, 1, 2; . >= 0) --> true`

The filters `all(p)` and `all` are defined analogously to `any(p)` and `any`.


## Numbers

### `infinite`, `nan`

The filters `infinite` and `nan` yield the floating-point numbers
`Infinity` and `NaN`:

- `infinite --> Infinity`
- `nan | isnan --> true`
  (we cannot test for equality with `NaN` here, because `nan == nan --> false`)

::: Advanced

We can also produce `Infinity` and `NaN` by:

- `1 / 0 --> Infinity`
- `0 / 0 | isnan --> true`

:::

### `abs`

The filter `abs`
yields the negation of the input if the input is smaller than `0`, else it
yields the input.
Note that due to this definition, strings, arrays, and objects
are also returned unchanged, because they are larger than `0`;
see [ordering](#ordering).

Examples:

- `-2.0, -1, 0, 1, 2.0 | abs --> 2.0 1 0 1 2.0`
- `"foo", [], {}       | abs --> "foo" [] {}`

### `floor`, `round`, `ceil` {#round}

The filters `floor`, `round` and `ceil` round a number
to its closest smaller integer,
to its closest integer, and
to its closest larger integer, respectively.
For example:

- ` 0.5 | floor, round, ceil -->  0  1 1`
- ` 0.4 | floor, round, ceil -->  0  0 1`
- ` 0.0 | floor, round, ceil -->  0  0 0`
- `-0.4 | floor, round, ceil --> -1  0 0`
- `-0.5 | floor, round, ceil --> -1 -1 0`
- `0, 1 | round --> 0 1`
- `nan | round | isnan --> true`
- `infinite | round --> Infinity`

### `libm` functions

jaq implements many filters via [`libm`](https://docs.rs/libm).
If not specified otherwise, these filters take and return floating-point numbers.

Zero-argument filters:

- `acos`
- `acosh`
- `asin`
- `asinh`
- `atan`
- `atanh`
- `cbrt`
- `cos`
- `cosh`
- `erf`
- `erfc`
- `exp`
- `exp10`
- `exp2`
- `expm1`
- `fabs`
- `frexp`, which returns pairs of (float, integer).
- `gamma`
- `ilogb`, which returns integers.
- `j0`
- `j1`
- `lgamma`
- `log`
- `log10`
- `log1p`
- `log2`
- `logb`
- `modf`, which returns pairs of (float, float).
- `nearbyint`
- `pow10`
- `rint`
- `significand`
- `sin`
- `sinh`
- `sqrt`
- `tan`
- `tanh`
- `tgamma`
- `trunc`
- `y0`
- `y1`

Two-argument filters that ignore `.`:

- `atan2`
- `copysign`
- `drem`
- `fdim`
- `fmax`
- `fmin`
- `fmod`
- `hypot`
- `jn`, which takes an integer as first argument.
- `ldexp`, which takes an integer as second argument.
- `nextafter`
- `nexttoward`
- `pow`
- `remainder`
- `scalb`
- `scalbln`, which takes as integer as second argument.
- `yn`, which takes an integer as first argument.

Three-argument filters that ignore `.`:

- `fma`

For example, to establish that `sin(pi)` is smaller than `10^-5`, we can use
`(3.141592 | sin) < (-5 | pow10) --> true`.


## Arrays

### `sort`, `sort_by(f)`

### `group_by(f)`

### `min`, `max`, `min_by(f)`, `max_by(f)`

### `unique`, `unique_by(f)`

### `reverse`

### `transpose`

The filter `transpose` takes an array of arrays and yields its transposition.

Examples:

- `[[1 ,  2, 3], [4, 5, 6]] | transpose --> [[1, 4], [2, 5], [3, 6]]`
- `[[1], [2, 3], [4, 5, 6]] | transpose --> [[1, 2, 4], [null, 3, 5], [null, null, 6]]`

::: Advanced

More precisely, `transpose` yields an array `$t` that contains
`map(length) | max` arrays of length `length`, such that
`$t[x][y] == .[y][x]` for every `x` and `y`.
We can verify this:

~~~
def verify: transpose as $t |
  ($t | length) == (map(length) | max),
  (range($t | length) as $x |
    ($t[$x] | length) == length,
    (range(length) as $y |
      $t[$x][$y] == .[$y][$x]
    )
  );
[[1,   2, 3], [4, 5, 6]],
[[1], [2, 3], [4, 5, 6]] | all(verify; .) --> true true
~~~

:::

### `flatten`, `flatten($depth)`

### `bsearch($x)`

The filter `bsearch($x)` takes a sorted array and
performs a binary search for `$x` in the array.
If the array contains `$x`, then
the filter yields a positive `$i` such that `.[$i] == $x`; otherwise,
the filter yields a negative `$i` such that inserting `$x` at the index `-$i-1`
in the array would preserve its the ordering.

Examples:

- `[0, 4, 8] | bsearch(8, 4, 0) --> 2 1 0`
- `[0, 4, 8] | bsearch(-2, 2, 6, 10) --> -1 -2 -3 -4`

If the input array is not sorted, then the output of this filter is meaningless.

::: Advanced

We can verify the property above for negative `$i`.
First, let us search for the value `6` that is not in the input array:

`[0, 4, 8] | bsearch(6) --> -3`.

Now, the definition postulates that we can insert `6` at the index `-$i-1`,
which is `--3-1 --> 2`:

`[0, 4, 8] | .[2:2] = [6] --> [0, 4, 6, 8]`.

We can see that the resulting array is sorted.

:::


## Strings

Unless stated otherwise, all filters in this section
take a text string as input, and fail if the input is of any other type.

### `utf8bytelength`

The filter `utf8bytelength` yields the number of bytes of the input string.
It is equivalent to `tobytes | length`, but different from `length`,
which counts the number of *characters*.

For example,
`"ゼノギアス" | length, utf8bytelength, (tobytes | length) --> 5 15 15`.

### `startswith($s)`, `endswith($s)`

The filter `startswith($s)` yields
`true` if the input string starts with the string `$s`, else `false`.
Similar for `endswith($s)`.
For example:

- `"ゼノギアス" | startswith("ゼノ") --> true`
- `"ゼノギアス" | endswith("ギアス") --> true`

### `trim`, `ltrim`, `rtrim`

The filters `ltrim` and `rtrim` remove from the input string all
leading and trailing whitespace, respectively.
Here, whitespace corresponds to the `White_Space` Unicode property.
The filter `trim` is equivalent to `ltrim | rtrim`.

For example:

- `" \t\n　Bonjour !   \r  " | ltrim --> "Bonjour !   \r  "`
- `" \t\n　Bonjour !   \r  " | rtrim --> " \t\n　Bonjour !"`

Note that there are a few quite unusual whitespace characters in this string.

### `ltrimstr($s)`, `rtrimstr($s)`

The filters `ltrimstr($s)` and `rtrimstr($s)` remove a single occurrence of
`$s` from the start or the end of the string, respectively.
If there is no such occurrence, the original string is returned.
For example:

- `"foofoobar" | ltrimstr("foo") --> "foobar"`
- `"foobarbar" | rtrimstr("bar") --> "foobar"`

### `explode`, `implode`

The filter `explode` yields an array containing
a positive number for each valid Unicode code points of the input string and
a negative number for each byte of each invalid Unicode code point.
For example:

~~~
"Dear ☀️" + (255 | tobytes | tostring) | explode -->
[68,101,97,114,32,9728,65039,-255]
~~~

Here, we can see that `"☀️"` has turned into two code points, namely
`9728` and `65039`, whereas the invalid `FF` byte (= 255) has become `-255`.

The inverse filter of `explode` is `implode`:

~~~
[68,101,97,114,32,9728,65039, -255] | implode[:-1] -->
"Dear ☀️"
~~~

(I omitted the `FF` byte at the end, because it is hard to save in a text editor.)

::: Compatibility

`jq` does not permit invalid code points in text strings, so it
returns and accepts only natural numbers in `explode` and `implode`. 

:::

### `split($s)` {#split}

This filter yields `. / $s` if its input `.` and `$s` are both strings, else it fails.
See the section on [division](#mul-div) for details.

Note that there is also [`split($re; $flags)`](#splits) that splits by a regex.

### `join($s)`

The filter `join($s)` takes as input an array `[x1, ..., xn]` and yields
`""` if the array is empty, otherwise
`"\(x1)" + $s + ... + $s + "\(xn)"`.
That is, it concatenates the string representations of the array values interspersed with `$s`.

For example, to memorise the hierarchy of values in jq:
`["null", "boolean", "number", "string", "array", "object"] | join(" < ") -->
"null < boolean < number < string < array < object"`.

::: Compatibility

Unlike jq, jaq does not map `null` values in the array to `""`,
nor does it reject array or object values in the array.

:::

### `ascii_downcase`, `ascii_upcase`

The filters `ascii_downcase` and `ascii_upcase` convert all
ASCII letters in the input string to their lower/upper case variants, respectively.
For example:

- `"Der λΠ-Kalkül" | ascii_downcase --> "der λΠ-kalkül"`
- `"Der λΠ-Kalkül" | ascii_upcase   --> "DER λΠ-KALKüL"`


## String formatting

### `@json`

### `@text`

### `@csv`

### `@tsv`

### `@html`

### `@sh`

### `@base64`, `@base64d`

### `@uri`, `@urid`


## Byte strings

### `tobytes`

The filter `tobytes` converts its input to a [byte string](#byte-strings).
Its output depends on the type of input:

- Natural number in the range `0` to `255` (`0xFF`):
  Yields a byte string with a single byte, e.g.
  `0 | tobytes --> b"\x00"`.
- Text string:
  Yields a byte string containing the underlying bytes of the text string, e.g.
  `"Hi" | tobytes --> b"Hi"`.
- Byte string: Yields the byte string unchanged.
- Array: Converts each element to a byte string and yields their concatenation, e.g.
  `[0, "Hi", [1, 255]] | tobytes --> b"\x00Hi\x01\xFF"`.
  This is equivalent to `map(tobytes) | add`.
- Anything else: Yields an error.

This is inspired by Erlang's `iolist_to_binary` function.

::: Compatibility

`jq` does not have byte strings and thus does not have `tobytes`.
`fq`, which has pioneered the `tobytes` filter, has both.

:::


## Recursion

### `walk(f)` {#walk}

The filter `walk(f)` recursively updates its input with `f`.
For example:

- `[[1, 2], [3]] | walk(numbers += 1) --> [[2, 3], [4]]`

::: Advanced

In jaq, `walk(f)` is defined as `.. |= f`, whereas
in `jq`, a definition similar to the following is used:

~~~
def walk(f): def rec: (.[]? |= rec) | f; rec;
~~~

This is a more efficient version of:

~~~
def walk(f): (.[]? |= walk(f)) | f;
~~~

We can show that in jaq, `.. |= f` and `jq`'s definition of `walk(f)` are equivalent.
First, let us recall that `.. |= f` is equivalent to the following in jaq:

~~~
def rec_up: (.[]? | rec_up), .; rec_up |= f
~~~

We can thus unfold `.. |= f`:

~~~
..                   |= f  === (unfolding .. |= f)
rec_up               |= f  === (unfolding rec_up)
((.[]? | rec_up), .) |= f  === (because (l, r) |= f  ===  (l |= f) | (r |= f))
((.[]? | rec_up) |= f)  | (. |= f)  === (because . |= f  ===  f)
((.[]? | rec_up) |= f)  | f         === (because (l | r) |= f  ===  l |= (r |= f))
(.[]? |= (rec_up |= f)) | f         === (because rec_up |= f  ===  .. |= f)
(.[]? |= (.. |= f))     | f
~~~

We can see thus that
`.. |= f` is equivalent to
`(.[]? |= (.. |= f)) | f`.
In the same sense,
`walk(f)` is equivalent to
`(.[]? |= walk(f)) | f`.
We can conclude that `.. |= f` is equivalent to `walk(f)`.

Note, however, that this equivalence does *not* hold in `jq`,
because `jq`'s updates work differently than jaq's.
The difference shows in particular when `f` returns multiple values.

:::

## Serialisation & Deserialisation

### `tostring`

The filter `tostring` converts its input to a string.
Its output depends on the type of its input:

- Text strings are returned unchanged, i.e. `"Hi" | tostring --> "Hi"`
- Byte strings are interpreted as text string, i.e. `"Hi" | tobytes | tostring --> "Hi"`
- Any other value is formatted compactly as if output by `jaq -c`. For example:
  - `null | tostring --> "null"`,
  - `[0, 1] | tostring --> "[0,1]"`,
  - `{a: 1} | tostring --> "{\"a\":1}"`.

::: Advanced

String interpolation without an explicit format, such as
`"\(null) and \([0, 1])" --> "null and [0,1]"`, behaves as if
the output of every interpolated filter was piped through `tostring`.

:::

### `tonumber`

The filter `tostring` takes as input either a number or a string.
If the input is a number, it is returned unchanged;
if the input is a string, it is parsed to a number, failing if this does not succeed.
For example:

- `  42   | tonumber --> 42`
- ` "42"  | tonumber --> 42`
- `"[42]" | try tonumber catch "fail" --> "fail"`

### `toboolean`

The filter `toboolean` is for booleans what `tonumber` is for numbers. 
For example:

- `  true   | toboolean --> true`
- ` "true"  | toboolean --> true`
- `"[true]" | try toboolean catch "fail" --> "fail"`

### `fromjson`, `tojson`

The filter `fromjson` takes a string as input,
parses it to JSON values and yields them.
For example:

~~~
"null true 0 \"foo\" [1] {\"foo\": 1}" | fromjson -->
 null true 0  "foo"  [1] { "foo" : 1}
~~~

The filter `tojson` takes an arbitrary value and
outputs a string containing its JSON representation.
For example:

~~~
 [null,true,0, "foo" ,[1],{ "foo" :1}] | tojson -->
"[null,true,0,\"foo\",[1],{\"foo\":1}]"
~~~

Note that `tojson` behaves similarly to `tostring`, but
when its input is a string, it will also encode it to JSON,
instead of returning it unchanged; i.e.
`"Hi" | tojson --> "\"Hi\""`.

::: Compatibility

In `jq`, `fromjson` yields an error when its input string contains multiple JSON values.
Furthermore, in `jaq`,
`tojson | fromjson` is equivalent to identity (`.`), whereas in `jq`,
this is not the case, because
`nan | tojson | fromjson` yields `null`, not `nan`.

:::


## Date & Time

The filters in this section serve to
obtain the current time and to
convert between different time formats, such as:

- Unix epoch: Marks a point in time by the number of seconds passed since
  January 1, 1970 00:00:00 (UTC).
- ISO-8601 datetime string: Represents a date, a time, and a time zone as a string,
  such as `"1970-01-01T00:00:00Z"` (corresponding to Unix epoch `0`).
- "Broken down time" (BDT) array: Represents a date and a time as an array of the shape
  `[year, month, day, hour, minute, second, weekday, yearday]`.
  All components are integers, except for `second`,
  which may be a floating-point number.
  The `month` is counted from `0`,
  the `weekday` is counted from Sunday (which is `0`), and
  the `yearday` is the day in the year counted from `0`.
  When a BDT array is used as input, only the first six components are considered.

You can convert between these representations via:

- Unix epoch from/to ISO 8601: `fromdate`, `todate`
- BDT to Unix epoch: `mktime`
- Unix epoch to BDT: `gmtime, localtime`
- Unix epoch or BDT from/to custom string: `strptime`, `strftime`, `strflocaltime`

As example, let us consider the time where the
Hill Valley courthouse's clock tower was struck by lightning, namely
[Saturday, November 12, 1955, at 10:04 p.m. PST](https://en.wikipedia.org/wiki/Hill_Valley_(Back_to_the_Future)#Fictional_history).
The corresponding date can be written in ISO 8601 as
`"1955-11-12T10:04:00-08:00"`.
We can convert that to a Unix epoch and from there to a (UTC) BDT via:

~~~
"1955-11-12T22:04:00-08:00" | fromdate | gmtime -->
[
  1955,
  10,
  13,
  6,
  4,
  0,
  0,
  316
]
~~~

We can infer that at this moment, in UTC
it was November 13
(the BDT month is `10` and not `11`, because BDT months are counted from `0`),
at 06:04:00.
Furthermore, that day
was a Sunday (because the `weekday` is `0`), which
was the `316`-th day of the year (where `0` is the first day).

::: Compatibility

`jq` does not allow time zone information in ISO 8601 datetime strings.

:::

### `now`

This filter yields the Unix epoch as floating-point number.

### `fromdate`, `todate`, `fromdateiso8601`, `todateiso8601`

These filters convert between Unix time and ISO-8601 timestamps.

For example, the
[Apollo 13 accident](https://en.wikipedia.org/wiki/Apollo_13#Accident)
happened at 03:08 UTC on April 14, 1970.
Its corresponding Unix time is
`"1970-04-14T03:08:00Z" | fromdate --> 8910480`.
We can get back the ISO-8601 timestamp via
`8910480 | todate --> "1970-04-14T03:08:00Z"`.

These filters can handle floating-point numbers, e.g.
`0.123456 | todate --> "1970-01-01T00:00:00.123456Z"` and
`"1970-01-01T00:00:00.123456Z" | fromdate --> 0.123456`.
In particular, `fromdate` yields a floating-point number if
the time cannot be represented losslessly as an integer.

The filters
`fromdateiso8601` and `todateiso8601` are synonyms of
`fromdate` and `todate`, respectively.

### `strftime($fmt)`, `strflocaltime($fmt)`

The filters `strftime($fmt)` and `strflocaltime($fmt)` take as input either
a number that is interpreted as Unix epoch, or
a BDT array.
The filters yield a string representation of the input time, using the format `$fmt`.

If the input is a Unix epoch,
both `strftime` and `strflocaltime` interpret it as UTC timestamp.
If the input is a BDT array, then
`strftime` interprets input as UTC and
`strflocaltime` interprets input as user local time.
`strftime` outputs the time as UTC and
`strflocaltime` outputs the time as user local time.

For example, if the user is in the CET zone (+0100):

- `0 | strftime("%T %z (%Z)") --> "00:00:00 +0000 (UTC)"`
- `[1970, 0, 1, 0, 0, 0] | strftime("%T %z (%Z)") --> "00:00:00 +0000 (UTC)"`
- `0 | strflocaltime("%T %z (%Z)")` yields `"01:00:00 +0100 (CET)"`
- `[1970, 0, 1, 0, 0, 0] | strflocaltime("%T %z (%Z)")` yields `"00:00:00 +0100 (CET)"`

::: Compatibility

`jq` prints `GMT` instead of `UTC` in the examples above; however,
GMT is not the same as UTC.

:::

### `strptime($fmt)`

The filter `strptime($fmt)` takes a string and parses it using the format `$fmt`,
yielding a BDT array.
If no time zone is inferred from the input (e.g. via `%Z`), it is assumed to be UTC.
For example:

- `"1970-01-01 00:00:00" | strptime("%F %T") --> [1970, 0, 1, 0, 0, 0, 4, 0]`
- `"1970-01-01 00:00:00 Europe/Vienna" | strptime("%F %T %Q") --> [1970, 0, 1, 0, 0, 0, 4, 0]`

### `gmtime`, `localtime`

The filters `gmtime` and `localtime` take a Unix epoch as input and
yield a corresponding BDT array, containing
the time in UTC (`gmtime`) or in the user local time (`localtime`).

For example, if the user is in the CET zone (+0100):

- `0 | gmtime --> [1970, 0, 1, 0, 0, 0, 4, 0]`
- `0 | localtime` yields `[1970, 0, 1, 1, 0, 0, 4, 0]`

### `mktime`

The filter `mktime` takes a BDT array that is assumed to be in UTC,
and yields the corresponding Unix epoch.
For example, `[1970, 0, 1, 0, 0, 0] | mktime --> 0`.


## Regular expressions

All the filters in this section, such as `test`, take a string as input and
fail if they receive any other type of value.
Furthermore, they all take two string arguments, namely
the regular expression `$re` and
the `$flags` that determine how the regular expression is interpreted.
Omitting `$flags` is equivalent to passing `""` as `$flags`.
For example,
`test($re)` is equivalent to `test($re; "")`.

The supported flags are:

- `g`: global search
- `n`: ignore empty matches
- `i`: case-insensitive
- `m`: multi-line mode: `^` and `$` match begin/end of line
- `s`: single-line mode: allow `.` to match `\n`
- `l`: greedy
- `x`: extended mode: ignore whitespace and allow line comments (starting with `#`)

::: Compatibility

jaq uses the [`regex-lite`](https://docs.rs/regex-lite) crate to
compile and run regular expressions (regexes).
See the crate documentation for a description of the supported regex syntax.

:::

### `test`

The filter `test` yields
`true` if some part of the input matches the regular expression, else `false`.
For example:

- `"jaq v3.0" | test("v[0-9]+\\.[0-9]+") --> true`
- `"jaq V3.0" | test("v[0-9]+\\.[0-9]+") --> false`
- `"jaq V3.0" | test("v[0-9]+\\.[0-9]+"; "i") --> true`

### `scan`

The filter `scan` yields all parts of the input that match the regular expression.
For example:

- `"v2.0, v3.0" | scan("v[0-9]+\\.[0-9]+"     ) --> "v2.0"`
- `"v2.0, v3.0" | scan("v[0-9]+\\.[0-9]+"; "g") --> "v2.0" "v3.0"`
- `"V2.0"       | scan("v[0-9]+\\.[0-9]+") -->` (no output)

### `match`

The filter `match` yields an object for every part of the input that matches the regular expression, containing:

- `"offset"`: the character index of the start of the match 
- `"length"`: the number of characters of the match 
- `"string"`: the contents of the match
- `"captures"`: an array with an object for every capture group, containing:
    - `"offset"`,
    - `"length"`,
    - `"string"`: as above, but for the capture group instead of the whole match
    - `"name"`: the name of the capture group if it has one, else this key is omitted

Example:

~~~
"v2.0, v3.0" | match("v(?<maj>[0-9]+)\\.([0-9]+)"; "g") -->
{
  "offset": 0,
  "length": 4,
  "string": "v2.0",
  "captures": [
    {
      "offset": 1,
      "length": 1,
      "string": "2",
      "name": "maj"
    },
    {
      "offset": 3,
      "length": 1,
      "string": "0"
    }
  ]
}
{
  "offset": 6,
  "length": 4,
  "string": "v3.0",
  "captures": [
    {
      "offset": 7,
      "length": 1,
      "string": "3",
      "name": "maj"
    },
    {
      "offset": 9,
      "length": 1,
      "string": "0"
    }
  ]
}
~~~

### `capture`

The filter `capture` yields an object for every part of the input that matches the regular expression, containing
for each named capture group an entry with
the group name as key and its matched string as value.

Example:

~~~
"v2.0, v3.0" | capture("v(?<maj>[0-9]+)\\.(?<min>[0-9]+)"; "g") -->
{
  "maj": "2",
  "min": "0"
}
{
  "maj": "3",
  "min": "0"
}
~~~

### `split`, `splits` {#splits}

The filter `split($re; $flags)` yields an array of
those parts of the input string that do *not* match the regular expression `$re`.
For example:

- `"Here be\tspaces" | split("\\s" ; "") --> ["Here", "be", "spaces"]`
- `"  More\n\n"      | split("\\s+"; "") --> ["", "More", ""]`
- `""                | split("\\s" ; "") --> [""]`

Note that `split($re; $flags)` is equivalent to `split($re; "g" + $flags)`,
meaning that the string is split not only by the first match, but by all matches.
Furthermore, unlike all other filters in this section,
`split($s)` is *not* equivalent to `split($s; $flags)`, because
`split($s)` splits a string by
a separator that is *not* interpreted as regular expression;
see [`split`](#split).

The filter `splits($re; $flags)` yields the elements of the array yielded by `split($re; $flags)`.
For example,
`"Here be\tspaces" | splits("\\s") --> "Here" "be" "spaces"`.
The filter `splits($re)` is equivalent to `splits($re; "")`.

### `sub`, `gsub`

The filter `sub($re; f; $flags)` replaces
all parts of the input string that match `$re` by
the output of `f`.
Here, `f` receives an object as returned by [`capture`](#capture); that is,
for every named capture group, it contains
its name as key and its matched string as value.

For example:

~~~
"Mr. 高橋 & Mrs. 嵯峨" | sub("(?<title>(Mr|Ms|Mrs)\\.) (?<name>\\S+)"; "\(.name) (\(.title))"; "g") -->
"高橋 (Mr.) & 嵯峨 (Mrs.)"
~~~

When the filter `f` yields multiple outputs,
then all potential combinations are output.
For example:

~~~
"Thanks, fine." | sub("(?<word>\\w+)"; .word, (.word | ascii_upcase); "g") -->
"Thanks, fine."
"Thanks, FINE."
"THANKS, fine."
"THANKS, FINE."
~~~

We have following short forms:

- The filter `gsub($re; f; $flags)` is equivalent to `sub($re; f; "g" + $flags)`.
- The filter `gsub($re; f)` is equivalent to `gsub($re; f; "")`.
- The filter  `sub($re; f)` is equivalent to  `sub($re; f; "")`.


## I/O

### `input`, `inputs` {#inputs}

The filter `inputs` yields all the inputs in the current input file.
For example, `jaq -n '[inputs]' <<< 1 2 3` yields `[1, 2, 3]`.
This can be useful to fold over large (potentially infinite) amounts of values;
for example, to create a cumulative sum over all input integers, you can use
`jaq -n 'foreach inputs as $x (0; .+$x)'`.

The filter `input` yields the next input in the current input file.

::: Compatibility

When there is no more input value left,
in `jq`, `input` yields an error, whereas in jaq, it yields no output value.
That is, in jaq, `input` is equivalent to `first(inputs)`.

:::

::: Advanced

Both `input` and `inputs` have a *side effect*, i.e. they advance the input stream.
That means that unlike most jq filters, `inputs` is not referentially transparent.
It is advised to use it sparingly and with caution,
lest you are devoured by the evil dragons of evaluation order.

:::


## Unsupported

This section lists filters present in `jq`, but not in jaq.

### SQL-style

jaq supports none of jq's SQL-style operators
`INDEX`,
`JOIN`, and
`IN`,
mostly for aesthetic reasons (uppercase-names) and because jq is not SQL.

### Streaming

jaq does not support `jq`'s `--stream` option;
therefore, it also does not implement the related filters
`truncate_stream`,
`fromstream`, and
`tostream`.

