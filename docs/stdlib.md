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
"Dear ☀️" + ([255] | tobytes | tostring) | explode -->
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

## Recursion

### `walk`


## Serialisation & Deserialisation

### `tostring`

### `tonumber`

### `toboolean`

### `fromjson`, `tojson`


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

- `0 | strftime("%T %z (%Z)") --> "00:00:00 +0000 (GMT)"`
- `[1970, 0, 1, 0, 0, 0] | strftime("%T %z (%Z)") --> "00:00:00 +0000 (GMT)"`
- `0 | strflocaltime("%T %z (%Z)")` yields `"01:00:00 +0100 (CET)"`
- `[1970, 0, 1, 0, 0, 0] | strflocaltime("%T %z (%Z)")` yields `"00:00:00 +0100 (CET)"`

### `strptime($fmt)`

The filter `strptime($fmt)` takes a string and parses it using the format `$fmt`,
yielding a BDT array.
If no time zone is inferred from the input (e.g. via `%Z`), it is assumed to be UTC.
For example:

- `"1970-01-01 00:00:00"     | strptime("%F %T"   ) --> [1970, 0, 1, 0, 0, 0, 4, 0]`
- `"1970-01-01 00:00:00 GMT" | strptime("%F %T %Z") --> [1970, 0, 1, 0, 0, 0, 4, 0]`

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

### `sub`, `gsub`


## I/O

### `input`, `inputs` {#inputs}

When there is no more input value left,
in `jq`, `input` yields an error, whereas in jaq, it yields no output value.
