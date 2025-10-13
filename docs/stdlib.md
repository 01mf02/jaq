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

### `keys`, `keys_unsorted` {#keys}

### `map(f)`, `map_values(f)` {#map}

### `to_entries`, `from_entries`, `with_entries(f)` {#entries}

### `not`

### `type`


## Stream consumers

### `first`, `first(f)`, `last`, `last(f)` {#first-last}

### `limit($n; f)` {#limit}

Yields the first `$n$` outputs of `f`, or no outputs if `$n <= 0`.
For example:

- `limit( 3; 1, 2      ) --> 1 2`
- `limit( 3; 1, 2, 3, 4) --> 1 2 3`
- `limit(-1; 1, 2      ) -->` (no output)

::: Compatibility

When `$n < 0`, `jq` yields an error instead.

:::

### `nth($i)`, `nth($i; f)` {#nth}


## Stream generators

### `range`

### `recurse`


## Selection

### `select(p)` {#select}

Yields its input if `p` yields a `true` value for it.
For example,
`(0, 1, -1, 2, -2) | select(. >= 0) --> 0 1 2`.

### `nulls`, `booleans`, `numbers`, `strings`, `arrays`, `objects`

Yields its input if it is of the given type.

### `normals`, `finites`

### `values`, `iterables`, `scalars`

### `isnull`, `isboolean`, `isnumber`, `isstring`, `isarray`, `isobject`

### `isnormal`, `isfinite`

### `isnan`, `isinfinite`


## Membership

### `contains($x)`

### `indices($x)`

### `index($x)`, `rindex($x)`

### `inside($x)`

### `has($k)`, `in($x)`


## Path expressions

### `path(f)`

### `del(f)`

### `pick(f)`


## Paths


## Reduction

### `add`, `add(f)`

### `any`, `any(p)`

### `all`, `all(p)`, `all(f; p)`


## Numbers

### `abs`

### `floor`, `round`, `ceil` {#round}

### `infinite`, `nan`

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

### `flatten`, `flatten($depth)`

### `bsearch($x)`


## Strings

### `utf8bytelength`

### `startswith($s)`, `endswith($s)`

### `trim`, `ltrim`, `rtrim`

### `ltrimstr($s)`, `rtrimstr($s)`

### `explode`, `implode`

### `split($s)`

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

## Recursion

### `walk`


## Serialisation & Deserialisation

### `tostring`

### `tonumber`

### `toboolean`

### `fromjson`, `tojson`


## Date functions

### `now`

### `fromdateiso8601`, `todateiso8601`

### `fromdate`, `todate`

### `strptime`, `strftime`, `strflocaltime`, `mktime`, `gmtime`, `localtime`


## SQL-style

jaq supports none of jq's SQL-style operators
`INDEX`,
`JOIN`, and
`IN`,
mostly for aesthetic reasons (uppercase-names) and because jq is not SQL.


## Streaming

jaq does not support `jq`'s `--stream` option;
therefore, it also does not implement the related filters
`truncate_stream`,
`fromstream`, and
`tostream`.


## Regular expressions

### `test`

### `scan`

### `match`

### `capture`

### `splits`

### `sub`, `gsub`


## I/O

### `input`, `inputs` {#inputs}

When there is no more input value left,
in `jq`, `input` yields an error, whereas in jaq, it yields no output value.

