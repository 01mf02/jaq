# jq language {#jq-lang}

The jq language is a lazy, functional streaming programming language.
A program written in the jq language is called a jq program or _filter_.
jq programs can be executed with several interpreters, including
`jq`, `gojq`, `fq`, and `jaq`.
The jq language is Turing-complete and can therefore be used to write
any program that can be written in any other programming language.

jaq aims to execute jq programs similarly to `jq`,
yet jaq diverges from `jq` in some situations.
This manual tries to point out such occasions.


## Filters

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

### Strings

Strings can be constructed using the syntax `"..."`.
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

### Arrays

Arrays can be constructed using the syntax `[f]`, where `f` is a filter.
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
`{count: 3, elem: 0} | [limit(.count; repeat(.elem))]`.

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

Because object keys can be arbitrary values in jaq, you can write e.g.
`{(0): 1, "2": 3, ([4]): 5, ({}): 6} -->
{ 0 : 1, "2": 3,  [4] : 5,  {} : 6}`.


## Nullary filters

A nullary filter does not take any arguments.
All nullary filters are atomic.

### Identity

The filter `.` yields its input as single output.
For example,
`1 | . --> 1`.

### Recursion

The filter `..` yields its input and all recursively contained values.
For example,
`{"a": 1, "b": [2, ["3"]]} | .. -->
{"a": 1, "b": [2, ["3"]]}
1
[2, ["3"]]
2
["3"]
"3"`.

## Unary filters

A unary filter takes a single argument.
All unary filters are atomic.

### Error suppression (`?`)

The postfix operator `f?` runs the atomic filter `f` and
returns all its outputs until (excluding) the first error.
For example,
`(1, error, 2)? --> 1`.

### Negation

The prefix operator `-f` runs the atomic filter `f` and negates its outputs.
For example,
`-1 --> -1` and
`-(1, 2) --> -1 -2`.

We can see that negation has a higher precedence than error suppression by
`-[]? --> ` (no output), which is the same as
`(-[])? --> `.
If negation would have a lower precedence, then `-[]?` would be equivalent to
`-([]?)`; however, that filter yields an error, as we can see by
`try -([]?) catch -1 --> -1`.


## Binary filters

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
For example,
`[1, 2, 3] | .[0] = (length, 4) -->
[3, 2, 3]
[4, 2, 3]`.

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

When `g` yields *multiple* outputs, then it depends on
the input type and on `f` whether more than one output of `g` is considered.
For example,
`1 | . |= (., .*2) --> 1 2` and
`[1, 2, 3] |  .[ ] |= (., .*2) --> [1, 2, 2, 4, 3, 6]` consider multiple outputs, but
`[1, 2, 3] |  .[0] |= (., .*2) --> [1, 2, 3]` and
`{a: 1, b: 2} | .a |= (., .*2) --> {"a": 1, "b": 2}` consider only the first output.

### Arithmetic update assignment

The filters
`f += g`,
`f -= g`,
`f *= g`,
`f /= g`,
`f %= g`
are short-hand forms for
`f = . + g`, ...
For example,
`[1, 2, 3] | .[0] += (length, 4) -->
[4, 2, 3]
[5, 2, 3]`.

### Alternation

The filter `f // g` runs `f` and yields
all its outputs that are neither `null` nor `false`.
If `f` yields no such outputs, then this filter yields the outputs of `g`.
For example,
`(null, 1, false, 2) // (3, 4) --> 1 2`,
`(null, false) // (3, 4) --> 3 4`, and
`empty // 3 --> 3`.

### Logic

`or`, `and`

## Value filters

### Equality

`.a == .b`

### Comparison

`.a < .b`

### Addition / subtraction

`+`, `-`

### Multiplication / division

`*`, `/`

### Modulus

`%`


## Path operators

jq's path operators `.[x]`, `.[x:y]` and `.[]` retrieve parts of their input.

### Indexing

The indexing operator `.[x]` yields the `x`-th element of the input.
For example,
`[1, 2, 3] | .[1] --> 2` and
`{a: 1, b: 2} | .["a"] --> 1`.

If there is no element at that index in the input, then this operator returns `null`.
For example,
`[1, 2, 3] | .[3]` and
`{a: 1, b: 2} | .["c"]` both yield `null`.

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

It holds that `.[]` has the same effect as `.[keys[]]`.
For example,
`[1, 2, 3] | .[keys[]] --> 1 2 3`.

### Advanced use

For each of the filters in this section,
you can employ any atomic filter instead of the leading `.` (identity).
For example, you can write `explode[]` as short form for `explode | .[]`.

You can also terminate any filter in this section with a `?`,
which silences errors from that operator.
For example, `.[]` yields an error if the input is neither an array nor an object,
but `.[]?` silences such errors, yielding no output instead.

You can chain together an arbitrary number of these operators;
for example, `.[0][]?[:-1]` is the same as `.[0] | .[]? | .[:-1]`.

When you combine the techniques above, be aware that
the filters `x` and `y` in `.[x]` and `.[x:y]`
are executed on the *original* input and
are *not* influenced by error suppression with `?`.
That means that `f[][x]?[y:z]` is equivalent to
`f as $f | x as $x | y as $y | z as $z | $f | .[] | .[$x]? | .[$y:$z]`.


## Atomic keyword filters

This section lists all atomic filters that start with a reserved keyword.

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
