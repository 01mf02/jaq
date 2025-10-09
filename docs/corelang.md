# Core filters {#jq-lang}

Filters are the basic building blocks of jq programs.
A *filter* is a function that
takes an input value and
yields a stream of output values.

The stream of output values can be infinite; for example, the jq filter
`repeat("Hi")` yields an infinite sequence of strings `"Hi"`.

The following sections document all filters with built-in syntax in jq.
Examples are written like `1 + 2, true or false --> 3 true`, which means that
running `jaq -n '1 + 2, true or false'` yields the outputs `3` and `true`.


## Values

This section lists all potential values that jq filters can process,
and how to produce them.

::: Compatibility

jaq extends the set of values that jq can process by
[byte strings](#byte-strings) and
[objects with non-string values](#objects).
Where
 jq reads and writes values by default as [JSON](#json),
jaq reads and writes values by default as [XJON](#xjon),
which is an extension of JSON.
See those sections for how jaq serialises values.

:::

### `null`

The filter `null` returns the `null` value.

The `null` value can be also obtained in various other ways,
such as indexing a non-existing key in an array or object, e.g.
`[] | .[0] --> null` or
`{} | .a   --> null`.

### Booleans

The filters `true` and `false` return the boolean values `true` and `false`.
Booleans can also be produced by comparison operations, e.g.
`0  ==  0 --> true` or
`[] == {} --> false`.

### Numbers

Numbers corresponding to the regular expression
`[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?` are
filters that return their corresponding value, e.g.
`0 --> 0`,
`3.14 --> 3.14`, and
`2.99e6 --> 2.99e6`.
Negative numbers can be constructed by
applying the [negation] operator to a number, e.g. `-1 --> -1`.

Internally, jaq distinguishes
integers, floating-point numbers, and decimal numbers:

- A number without a dot (`.`) and without an exponent (`e`/`E`)
  is losslessly stored as integer.
  jaq can store and calculate with integers of arbitrary size, e.g.
  `340282366920938463463374607431768211456` (`2^128`).
- Any non-integer number is stored initially as decimal number,
  which is a string representation of the number.
  That means that jaq losslessly preserves
  any number corresponding to the regular expression above,
  such as `1.0e500`, if it occurs in a JSON file or jq filter.
- When calculating with a decimal number,
  jaq converts it transparently to a 64-bit IEEE-754 floating-point number.
  For example, `1.0e500 + 1 --> Infinity`, because jaq converts
  `1.0e500` to the closest floating-point number, which is `Infinity`.

::: Compatibility

`jq` prints NaN as `null`; e.g. `nan | tojson` yields `null`.
In contrast, jaq prints NaN as `NaN`; e.g. `nan | tojson --> "NaN"`.

As a result, in jaq, any value can be round-tripped with `tojson | fromjson`; e.g.
`nan | tojson | fromjson | isnan --> true`.
In `jq`, `tojson | fromjson` does not round-trip, because of
`NaN` and `Infinity`.

:::

### Text strings

A text string is an array of bytes that can be constructed using the syntax `"..."`.
Here, `...` may contain any UTF-8 characters
in the range from `U+0020` to `U+10FFFF`, excluding `'"'` and `'\'`.
For example,
`"Hello 東京!" --> "Hello 東京!"`.

Furthermore, `...` may contain escape sequences, which
start with `'\'` and are followed by one of:

- `b`, `f`, `n`, `r`, or `t`
- `'"'` or `'\'`
- `uHHHH`, where `HHHH` is a hexadecimal number
- `(f)`, where `f` is a jq filter (*string interpolation*)

Most characters below `U+0020` can only be produced via a Unicode escape sequence, i.e.
`"NUL = \u0000" --> "NUL = \u0000"`.

A string containing an interpolated filter, such as
`"...\(f)..."`, is equivalent to
`"..." + (f | tostring) + "..."`.
For example,
`"Hello \("Alice", "Bob") 😀!\n" -->
"Hello Alice 😀!\n" "Hello Bob 😀!\n"`.

A string is *identifier-like* if it matches the regular expression
`[a-zA-Z_][a-zA-Z_0-9]*`.

Strings may be prefixed with `@x`, where `x` is an identifier; e.g. `@uri`.
In such a case, the filters of interpolated strings are
piped through `@x` instead of `tostring`, i.e.
`@x "...\(f)..."` is equivalent to
`@x "..." + (f | @x) + @x "..."`.
When `"..."` does not contain any interpolated filter, then
`@x "..."` is equivalent to `"..."`.
For example,
`"-[]?" | @uri "https://gedenkt.at/jaq/?q=\(.)" -->
"https://gedenkt.at/jaq/?q=-%5B%5D%3F"`.

### Byte strings

A byte string is an array of bytes that is *not* interpreted as (UTF-8) text.
It can be produced from a text string via the filter [`tobytes`](#tobytes), e.g.
`"Hello, world! 🙂" | tobytes --> b"Hello, world! \xf0\x9f\x99\x82"`.
Currently, there is no native syntax in jaq to produce a byte string directly.

Byte strings differ from text strings in a few regards; in particular,
they can be [indexed](#indexing) and [sliced](#slicing) in constant time.
That makes byte strings interesting e.g. for parsing binary formats.

For compatibility reasons, jaq considers
both text strings and byte strings as strings.
That means that `"Hello" | isstring and (tobytes | isstring) --> true`.
Furthermore, a text string and a byte string that
contain equivalent bytes are considered equal, e.g.
`"Hello" | . == tobytes --> true`.

::: Compatibility

Byte strings do not exist in `jq`; however, they exist in `fq`.

:::



### Arrays

An array is a finite sequence of values.
An empty array can be constructed with the filter
`[]`, which is a short form for
`[empty] --> []`.
An array of values can be constructed using the syntax
`[f]`, where `f` is a filter.
The filter `[f]` passes its input to `f` and runs it,
returning an array containing all outputs of `f`.
If `f` throws an error, then `[f]` returns that error instead of an array.

This syntax allows constructing arrays such as `[1, 2, 3]`.
Here, `1, 2, 3` is just a filter that yields three numbers,
see [concatenation](#concatenation).
There is no dedicated array syntax `[x1, ..., xn]`.
That means that you can use arbitrary filters in `[f]`; for example,
`limit(3; repeat(0)) --> 0 0 0`.
You can use the input passed to `[f]` inside `f`;
for example, we can write the previous filter equivalently as
`{count: 3, elem: 0} | [limit(.count; repeat(.elem))] --> [0, 0, 0]`.

### Objects

An object is a mapping from keys to values.
In jq, keys must be strings, whereas
in jaq, keys can be arbitrary values.

An empty object can be constructed with the filter `{}`.
An object with a single key and value can be constructed by `{(k): v}`,
where `k` and `v` are both filters.
For example, `{("a"): 1} --> {"a": 1}`.

If `k` is a [text string filter](#strings) such as `"a"` or a variable such as `$k`,
then you can omit the parentheses, e.g. `{"a": 1}` or `{$k: 1}`.
If the string is identifier-like,
then you can also omit the quotes, e.g. `{a: 1}`.

To construct an object with multiple key-value pairs,
you can add multiple objects; e.g.
`{(k1): v1} + ... + {(kn): vn}`.
You can write this more compactly as
`{(k1): v1, ..., (kn): vn}`.
For example,
`{a: 1, b: 2} --> {"a": 1, "b": 2}`.

Instead of `{k: .k}` (see [indexing](#indexing)),
you can also write `{k}`; e.g.,
`{a: 1, b: 2} | {a} --> {"a": 1}`.
Instead of `{k: $k}`,
you can also write `{$k}`; e.g.,
`1 as $k | {$k} --> {"k": 1}`.

The filter `{(k): v}` is equivalent to `k as $k | v as $v | {$k: $v}`.
That means that when either `k` or `v` yield multiple output values,
an object is produced for every output combination; for example,
`{("a", "b"): (1, 2)} --> {"a": 1} {"a": 2} {"b": 1} {"b": 2}`.
Note that here, it is necessary to surround `1, 2` with parentheses,
in order to clarify that `,` does not start a new key-value pair.

::: Compatibility

Because object keys can be arbitrary values in jaq, you can write e.g.:

~~~
{(0): 1, "2": 3, ([4]): 5, ({}): 6} -->
{ 0 : 1, "2": 3,  [4] : 5,  {} : 6}
~~~

This yields an error in `jq`.

:::


## Path operators

jq's path operators
`.[x]` ([indexing](#indexing)),
`.[x:y]` ([slicing](#slicing)), and
`.[]` ([iterating](#iterating))
retrieve parts of their input.

### Indexing

The indexing operator `.[x]` yields the `x`-th element of the input.
For example:

- `[1, 2, 3] | .[1] --> 2`
- `{a: 1, b: 2} | .["a"] --> 1`

If there is no element at that index in the input, then this operator returns `null`.
For example:

- `[1, 2, 3] | .[3] --> null`
- `{a: 1, b: 2} | .["c"] --> null`

This operator treats byte string input like an array of numbers in the range 0..256.
For example,
`"@ABC" | tobytes | .[0] --> 64`,
because the character `'@'` is encoded in UTF-8 as single byte `64` (`0x40`).

This operator yields an error either
if the input is neither an array or an object, or
if the input is an array and the index is not an integer.

For identifier-like strings like `name`,
you can write `.name` as short form for `.["name"]`.

For array input, you can also use negative numbers as index,
which will be interpreted as index counting from the end of the array.
For example,
`[1, 2, 3] | .[-1] --> 3`.

::: Compatibility

`jq` accepts floating-point numbers as array indices; e.g.
`[1, 2] | .[1.5]` yields `2`.
In jaq, indexing an array with a floating-point number yields an error.
The same applies to the [slicing](#slicing) operator below.

:::

### Slicing

The slicing operator `.[x:y]` yields a slice of a string or an array,
from the `x`-th element to (excluding) the `y`-th element.
For example,
`[1, 2, 3] | .[1:3] --> [2, 3]`, and
`"Hello World!" | .[6:11] --> "World"`.

When slicing a text string, the `x`-th element refers to the `x`-th *UTF-8 character*.
For example,
`"老虎" | .[1:2] --> "虎"`.

Like the indexing operator, the slicing operator
treats byte string input like an array of numbers and
interprets negative indices as indices counting from the end.

The short form `.[x:]` creates a slice from the index `x` to the end, and
the short form `.[:y]` creates a slice from the beginning to (excluding) the index `x`.
For example, `"Hello World!" | .[:5], .[6:] --> "Hello" "World!"`.

### Iterating

The iteration operator `.[]` yields all values `v1 ... vn` when given
an array `[v1, ..., vn]` or
an object `{k1: v1, ..., kn: vn}`.
For example,
`[1, 2, 3] | .[] --> 1 2 3` and
`{a: 1, b: 2} | .[] --> 1 2`.

::: Advanced

It holds that `.[]` has the same effect as `.[keys[]]`.
For example,
`[1, 2, 3] | .[keys[]] --> 1 2 3`.

:::

### Chaining

For each of the filters in this section,
you can employ any atomic filter instead of the leading `.` (identity).
For example, you can write `explode[]` as short form for `explode | .[]`.

You can also terminate any filter in this section with a `?`,
which silences errors from that operator.
For example, `.[]` yields an error if the input is neither an array nor an object,
but `.[]?` silences such errors, yielding no output instead.

You can chain together an arbitrary number of these operators;
for example, `.[0][]?[:-1]` is the same as `.[0] | .[]? | .[:-1]`.

::: Advanced

When you combine the techniques above, be aware that
the filters `x` and `y` in `.[x]` and `.[x:y]`
are executed on the *original* input and
are *not* influenced by error suppression with `?`.
That means that `f[][x]?[y:z]` is equivalent to
`f as $f | x as $x | y as $y | z as $z | $f | .[] | .[$x]? | .[$y:$z]`.

:::


## Nullary

A nullary filter does not take any arguments.
All nullary filters are atomic.

### Identity

The filter `.` yields its input as single output.
For example,
`1 | . --> 1`.

### Recursion

The filter `..` yields its input and all recursively contained values.
For example:

~~~
{"a": 1, "b": [2, ["3"]]} | .. -->
{"a": 1, "b": [2, ["3"]]}
1
[2, ["3"]]
2
["3"]
"3"
~~~

## Unary

A unary filter takes a single argument.
All unary filters are atomic.

### Negation

The prefix operator `-f` runs the atomic filter `f` and negates its outputs.
For example,
`-1 --> -1` and
`-(1, 2) --> -1 -2`.

### Error suppression (`?`)

The postfix operator `f?` runs the atomic filter `f` and
returns all its outputs until (excluding) the first error.
For example,
`(1, error, 2)? --> 1`.

This operator is equivalent to `try f` (see [`try-catch`](#try-catch)).
For example,
`try (1, error, 2) --> 1`.

::: Advanced

We can see that error suppression has a higher precedence than negation by
`try -[]? catch -1 --> -1`, which shows us that
`-[]?` yields the same as `-([]?)`, which is equivalent to
`-[]` and yields an error.
If negation would have a higher precedence,
then `-[]?` would be equivalent to `(-[])?`;
however, that filter yields no output, as we can see by
`(-[])? --> `.

:::



## Binary (complex)

A binary filter takes two arguments.
All binary filters are non-atomic.

This section lists all binary infix filters sorted by increasing precedence.

### Composition

The filter `f | g` runs `f`, and
for every output `y` of `f`, it runs `g` with `y` as input and yields its outputs.
For example, `(1, 2) | (., 3) --> 1 3 2 3`.

If either `f` or `g` yields an error, then
`f | g` yields that error, followed by nothing.
For example, `(1, 2) | (., error)` yields the same as `1, error`.

### Variable binding

The filter `f as $x | g` runs `f`, and
for every output `x` of `f`, it binds the value of `x` to the *variable* `$x`.
Here, `x` is an identifier.
It then runs `g` *on the original input*,
replacing any reference to `$x` in `g` by the value `x`.
For example,
`"Hello" | length as $x | . + " has length \($x)" --> "Hello has length 5"`.

### Concatenation

The filter `f, g` yields the concatenation of the outputs of `f` and `g`.
For example, `1, 2 --> 1 2`.

### Plain assignment

The filter `f = g` runs `g` with its input, and
for every output `y` of `g`, it
returns a copy of the input whose
values at the positions given by `f` are replaced by `y`.
For example:

~~~
[1, 2, 3] | .[0] = (length, 4) -->
[3, 2, 3]
[4, 2, 3]
~~~

### Update assignment

The filter `f |= g` returns a copy of its input where
each value `v` at a position given by `f` is replaced by
the output of `g` run with input `v`.
For example,
`[1, 2, 3] | .[] |= .*2 --> [2, 4, 6]`.

When `g` yields *no* outputs, then the value at the position is deleted;
for example,
`[1, 2, 3] | .[0] |= empty --> [2, 3]` and
`{a: 1, b: 2} | .a |= empty --> {"b": 2}`.

::: Advanced

When `g` yields *multiple* outputs, then it depends on
the input type and on `f` whether more than one output of `g` is considered.
For example, the following updates consider multiple outputs:

- `1 | . |= (., .*2) --> 1 2`
- `[1, 2, 3] |  .[ ] |= (., .*2) --> [1, 2, 2, 4, 3, 6]`

On the other hand, the following updates consider only the first output:

- `[1, 2, 3] |  .[0] |= (., .*2) --> [1, 2, 3]`
- `{a: 1, b: 2} | .a |= (., .*2) --> {"a": 1, "b": 2}`

:::

### Arithmetic update assignment

The filters
`f += g`,
`f -= g`,
`f *= g`,
`f /= g`,
`f %= g`
are short-hand forms for
`f = . + g`, ...
For example:

~~~
[1, 2, 3] | .[0] += (length, 4) -->
[4, 2, 3]
[5, 2, 3]
~~~

### Alternation

The filter `f // g` runs `f` and yields
all its outputs that are neither `null` nor `false`.
If `f` yields no such outputs, then this filter yields the outputs of `g`.
For example:

- `(null, 1, false, 2) // (3, 4) --> 1 2`
- `(null, false) // (3, 4) --> 3 4`
- `empty // 3 --> 3`

### Logic

The filter `f or g` (disjunction) evaluates `f` and
returns `true` if its boolean value is `true`, else it
returns the boolean values of `g`.
The filter `f and g` (conjunction) evaluates `f` and
returns `false` if its boolean value is `false`, else it
returns the boolean values of `g`.

For example:

~~~
(true, false) or (true, false) -->
true
true
false
~~~

~~~
(true, false) and (true, false) -->
true
false
false
~~~

The filter `f and g` has higher precedence than `f or g`.

::: Advanced

We can see the higher precedence of `and` by
`false and true or true --> true`, which yields the same as
`(false and true) or true --> true`, but not the same as
`false and (true or true) --> false`.

To find this formula, I used the following program:

~~~
def bool: true, false;
{x: bool, y: bool, z: bool} | select(
  ((.x and  .y) or .z ) !=
  ( .x and (.y  or .z))
) -->
{
  "x": false,
  "y": true,
  "z": true
}
{
  "x": false,
  "y": false,
  "z": true
}
~~~

:::

It holds that `f or g` is equivalent to `f as $x | $x or g`;
similar for `and`.


## Binary (simple)

Every simple binary operator such as `+` in this section fulfills the property that
`f + g` is equivalent to
`f as $x | g as $y | $x + $y`.
For example,
`(1, 2) + (1, 3) --> 2 4 3 5` and
`(1, 2) * (1, 3) --> 1 3 2 6`.
This property does not hold for the complex binary filters above.

::: Compatibility

In `jq`, a slightly different property holds, namely that
`f + g` is equivalent to
`g as $y | f as $x | $x + $y`.
That means that in `jq`,
`(1, 2) + (1, 3)` yields `2 3 4 5`.

:::

### Equality

The operator `$x == $y` returns `true` if the two values `$x` and `$y` are equal, else `false`.
Similarly, `$x != $y` returns the negation of `$x == $y`, i.e. `$x == $y | not`.

Interesting cases include:

- NaN does not equal any value, including itself; i.e.
  `nan == nan --> false`
- Integer values equal equivalent floating-point values; i.e.
  `1 == 1.0 --> true`
- Objects are equal if they have the same keys and for every key,
  the associated value is equal; i.e.
  `{a: 1, b: 2} == {b: 2, a: 1} --> true`

::: Advanced

There are values that are equal,
yet yield different results when fed to the same filter.
For example:

~~~
{a: 1, b: 2} as $x |
{b: 2, a: 1} as $y |
$x == $y, ($x | tojson) == ($y | tojson) -->
true false
~~~

:::

### Ordering

The operator `$x < $y` returns `true` if `$x` is smaller than `$y`.
Similarly:

- `$x >  $y` is equivalent to `$y < $x`,
- `$x <= $y` is equivalent to `$x < $y or $x == $y`, and
- `$x >= $y` is equivalent to `$x > $y or $x == $y`.

Values are ordered as follows:

- `null`
- Booleans: `false < true --> true`
- Numbers:
    - `NaN` is smaller than any other number, including itself; i.e.
      `nan < nan --> true`
    - `-Infinity`, e.g. `-infinite < -99999999999999999999999999 --> true`
    - Finite numbers, e.g. `0 < 1 --> true`
    - `Infinity`, e.g. `infinite > 99999999999999999999999999 --> true`
- Strings: lexicographic ordering by underlying bytes, e.g.
  `"Hello" < "Hello World" --> true` and
  `"@B" < "A" --> true`.
- Arrays: lexicographic ordering, e.g.
  `[1, 2] < [1, 2, 3] --> true` and
  `[0, 2] < [1] --> true`.
- Objects: An object `$x` is smaller than an object `$y` either if:
  - the keys of `$x` are smaller than the keys of `$y` or
  - the keys of `$x` are equal to the keys of `$y` and
    the values of `$x` are smaller than the values of `$y`.

::: Advanced

More precisely, an object `$x` is smaller than an object `$y` if:

~~~
($x | to_entries | sort_by(.key)) as $ex |
($y | to_entries | sort_by(.key)) as $ey |
[$ex[].key]   < [$ey[].key] or
[$ex[].key]  == [$ey[].key] and
[$ex[].value] < [$ey[].value]
~~~

:::

### Addition / subtraction

The filter `$x + $y` adds two values as follows:

- `null + $x` and `$x + null` yields `$x`.
- Adding numbers yields their sum, which is
  integer if both numbers are integer, else a floating-point number.
  For example,
  `1 + 2 --> 3` and
  `1 + 2.0 --> 3.0`.
- Adding strings or arrays concatenates them, i.e.
  `"Hello, " + "World!" --> "Hello, World!"` and
  `[1, 2] + [3, 4] --> [1, 2, 3, 4]`.
- Adding objects yields their *union*.
  If a key is present in both objects, then the resulting object
  will contain the key with the value of the object on the *right*; i.e.
  `{a: 1, b: 2} + {b: 3, c: 4} --> {"a": 1, "b": 3, "c": 4}`.
- Adding anything else yields an error.

The filter `$x - $y` subtracts `$y` from `$x` as follows:

- Subtracting numbers yields their difference, similar to addition.
- Subtracting arrays yields the
  left array with all elements contained in the right array removed; i.e.
  `[1, 2, 3, 4] - [2, 4] --> [1, 3]`.
- Subtracting anything else yields an error.

### Multiplication / division

The filter `$x * $y` multiplies two values as follows:

- Multiplying numbers yields their product, similar to addition.
- Multiplying a string with an integer `$n` yields
  the `$n`-fold concatenation of the string, i.e.
  `"abc" * 3 --> "abcabcabc"`.
  If `$n <= 0`, then this yields `null`, i.e.
  `0 * "abc" --> null`.
- Multiplying two objects merges them recursively.
  In particular,
  `$x * {k: v, ...}` yields
  `($x + {k: $x[k] * v}) * {...}` if both `v` and `$x[k]` are objects, else
  `($x + {k: v}) * {...}`.
  For example,
  `{a: {b: 0, c: 2}, e: 4} * {a: {b: 1, d: 3}, f: 5} -->
  {"a": {"b": 1, "c": 2, "d": 3}, "e": 4, "f": 5}`.
- Multiplying anything else yields an error.

### Modulus

The filter `$x % $y` calculates the modulus of two numbers,
and fails for anything else.
For example,
`5 % 2 --> 1`.
Any of the two numbers can also be a floating-point number;
however, the result of this may be unexpected.
For example,
`5.1 % 2 --> 1.0999999999999996` and
`5.5 % 2 --> 1.5`.



## Keywords

This section lists all filters that start with a reserved keyword.
These filters are all atomic.

### if-then-else

### try-catch

### label-break

### reduce / foreach

### def

The filter `def x: f; g` binds the filter `f` to a filter with the name `x`.
Here, `x` is an identifier.
The filter `g` can contain calls to the filter `x`, and
any such calls will be replaced by the filter `f`.

The filter `def x(x1; ...; xn): f; g` binds the filter `f` to
a filter with the name `x` and the arity `n`.
Here, `x1` to `xn` are identifiers that are called *arguments* of `x`, and
`f` can contain references to these arguments.
The filter `g` can contain calls of the shape `x(g1; ...; gn)`,
where `g1` to `gn` are filters.
Any such calls will be replaced by the filter `f`, where
every argument `xi` is replaced by its corresponding filter `gi`.

Filters can be defined recursively; for example,
`def f: 0, f; f` yields an infinite sequence of `0` values.
