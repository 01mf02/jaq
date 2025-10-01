---
title: JAQ(1)
---



# Name

`jaq` --- Command-line JSON processor



# Synopsis

`jaq`\ \[_OPTION_\]...\ \[_FILTER_\]\ \[_FILE_\]...



# Description

`jaq` is an interpreter for the jq language originally designed by Stephen Dolan.
It is designed to be usable as a drop-in replacement for the `jq` program,
which is the reference interpreter for the jq language written in C.

Written in Rust, jaq focuses on correctness, high performance, and simplicity.
In addition, jaq adds some functionality not present in jq:

- Support for multiple file formats, including JSON, YAML, CBOR, TOML, XML;
  see [`--from`] and [`--to`]
- Support for invalid UTF-8 code units in JSON
- Byte strings; see [`tobytes`]
- Objects with non-string keys, such as `{0: 1, [2]: 3}`
- In-place replacement of input files; see [`--in-place`]

[`--from`]: #--from
[`--to`]: #--to
[`tobytes`]: #tobytes
[`--in-place`]: #--in-place



# Command-line interface

Running
`jaq`\ \[_OPTION_\]...\ \[_FILTER_\]\ \[_FILE_\]...
performs the following steps:

- Parse _FILTER_ as jq program; see [jq language](#jq-lang)
- For each _FILE_:
    - Parse _FILE_ to a stream of values
    - For each input value in the file:
        - Run _FILTER_ on the input value and print its output values as JSON

For example, `jaq '.name?' persons.json`
parses the filter `.name?`, then
reads all values in `persons.json` one-by-one.
It then executes the filter `.name?` on each of the values and
prints the outputs of the filter as JSON.

There are a few rules:

- If no _FILTER_ is given, jaq uses `.` (the [identity] filter) as filter.
- If no _FILE_ is given, jaq reads from standard input.
- jaq determines the format to parse a _FILE_ as follows:
    - If [`--from`] _FORMAT_ is used, jaq uses that format.
    - Otherwise, if _FILE_ has a file extension known by jaq, such as
      `.json`, `.yaml`, `.cbor`, `.toml`, `.xml`,
      jaq uses the corresponding format.
    - Otherwise, jaq assumes JSON.
- If an error is encountered at any point, jaq stops.


## Input options

### `--from` _FORMAT_ {#--from}

Interpret all input files as _FORMAT_.
For example,
`jaq --from yaml . myfile.yml`
parses `myfile.yml` as YAML.
Possible values of _FORMAT_ include:
`raw`, `json`, `yaml`, `cbor`, `toml`, `xml`.

jaq automatically chooses the corresponding input format for
files with the extensions
`.json`, `.yaml`, `.cbor`, `.toml`, `.xml`, `.xhtml`.
That means that
`jaq --from cbor . myfile.cbor` is equivalent to
`jaq . myfile.cbor`.

### `-n`, `--null-input`

Feed `null` as input to the main program, ignoring any input files.
For example,
`yes | jaq -n` yields `null`,
which shows that this does indeed not read any input.

The inputs can still be obtained via the [`inputs`] filter; for example,
`yes true | jaq -n 'first(inputs)'` yields `true`.
This can be useful to fold over all inputs with [`reduce`] or [`foreach`].

### `-R`, `--raw-input`

Read lines of the input as sequence of strings.
For example,
`echo -e "Hello\nWorld" | jaq -R` yields two outputs; `"Hello"` and `"World"`.
When combined with `-s` (`--slurp`), this yields the whole input as a single string.
For example,
`echo -e "Hello\nWorld" | jaq -Rs` yields `"Hello\nWorld\n"`.

This is equivalent to `--from raw`.

### `-s`, `--slurp`

Read (slurp) all input values into one array.
For example,
`echo "1 2 3" | jaq -s` yields a single output, namely the array `[1, 2, 3]`, whereas
`echo "1 2 3" | jaq` yields three outputs, namely `1`, `2`, and `3`.


## Output options

### `--to` _FORMAT_ {#--to}

Print all output values in the given _FORMAT_.
Any _FORMAT_ accepted by [`--from`] can be used here.

Note that not every value can be printed in every format.
For example, TOML requires that the root value is an object, so
`jaq --to toml <<< []`
yields an error.

### `-c`, `--compact-output`

Print JSON compactly, omitting whitespace.
For example, `jaq -c <<< '[1, 2, 3]'` yields the output `[1,2,3]`.

### `-r`, `--raw-output`

Write strings without escaping them and without surrounding them with quotes.
For example,
`jaq -r <<< '"Hello\nWorld"'` outputs two lines; `Hello` and `World`, whereas 
`jaq <<< '"Hello\nWorld"'` outputs a single line; `"Hello\nWorld"`.

This does not impact strings contained inside other values, i.e. arrays and objects.
For example,
`jaq -r <<< '["Hello\nWorld"]'` outputs `["Hello\nWorld"]`.

This is equivalent to `--to raw`.

### `-j`, `--join-output`

Do not print a newline after each value.
For example,
`jaq -j <<< 'true false'` yields the output `truefalse`, without trailing newline.

This is particularly useful in combination with `--raw-output` (`-r`); for example,
`jaq -jr <<< '"Hello" " " "World" "\n"'` yields the output `Hello World`
(with trailing newline).

### `-i`, `--in-place` {#--in-place}

Overwrite input file with its output.
For example,
`jaq -i . myfile.json` reads the file `myfile.json` and
overwrites it with a formatted version of it.
Note that the input file is overwritten only
once there is no more output and
if there has not been any error.

### `-S`, `--sort-keys`

Print objects sorted by their keys.
For example,
`jaq -Sc <<< '{"b": {"d": 3, "c": 2}, "a": 1}'` yields
`{"a":1,"b":{"c":2,"d":3}}`, whereas
`jaq  -c <<< '{"b": {"d": 3, "c": 2}, "a": 1}'` yields
`{"b":{"d":3,"c":2},"a":1}`.

### `-C`, `--color-output`

Always color output, even if jaq does not print to a terminal.
For example,
`jaq -C <<< '{}' | jaq --from raw tobytes` yields the byte string
`b"\x1b[1m{\x1b[0m\x1b[1m}\x1b[0m"`, containing ANSI color sequences, whereas
`jaq    <<< '{}' | jaq --from raw tobytes` yields
`b"{}"`.
(Here, `jaq --from raw tobytes` prints a byte representation of its input.)

### `-M`, `--monochrome-output`

Do not color output.

### `--tab`

Use tabs for indentation rather than spaces.

For example,
`jaq --tab <<< '[1, [2]]' | jaq -Rs` yields
`"[\n\t1,\n\t[\n\t\t2\n\t]\n]\n"`, whereas
`jaq      <<< '[1, [2]]' | jaq -Rs` yields
`"[\n  1,\n  [\n    2\n  ]\n]\n"`.

### `--indent` _N_

Use _N_ spaces for indentation (default: 2).


## Compilation options

-f, --from-file           Read filter from a file given by filter argument
-L, --library-path <_DIR_>  Search for modules and data in given directory


## Variable options

--arg       <A> <V>   Set variable `$A` to string `V`
--argjson   <A> <V>   Set variable `$A` to JSON value `V`
--slurpfile <A> <F>   Set variable `$A` to array containing the JSON values in file `F`
--rawfile   <A> <F>   Set variable `$A` to string containing the contents of file `F`
--args                Collect remaining positional arguments into `$ARGS.positional`



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
An equivalent UNIX command is `yes "Hi"`.

Following the UNIX philosophy,
you can pipe the outputs of one filter to another filter; for example,
`add(limit(10; repeat("Hi\n")) | length)` is a jq filter that
produces 10 newline-terminated `"Hi"` strings,
calculates the `length` of each of them, then
`add`s the computed lengths.
It counts the number of characters of 10 "Hi" lines, yielding the final result `30`.
Its UNIX equivalent would be `yes "Hi" | head -10 | wc -c`.

While UNIX programs traditionally operate on strings,
jq filters traditionally operate on JSON values.
That gives jq an advantage when processing more complex data,
such as numbers, arrays, objects.
For example, suppose that we have a JSON array that contains
an object for each file in the current directory.
Then we can get the largest MP4 file in the directory as follows:

~~~
$ jaq 'first(sort_by(-.size)[] | .name | select(endswith(".mp4")))' << EOF
[ {"name": "Benson, Arizona.mp4", "size": 10893113},
  {"name": "Michel Delpech - Pour un flirt.webm", "size": 23131654},
  {"name": "Rainhard Fendrich - I am from Austria.mp4", "size": 11615456}
]
EOF
"Rainhard Fendrich - I am from Austria.mp4"
~~~

The following sections document how to construct filters.


## Values

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

A string is *identifier-like* if it matches the regular expression
`[a-zA-Z_][a-zA-Z_0-9]*`.

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


## Basic filters

### Identity

The filter `.` yields its input as single output.

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

### Concatenation

The filter `f, g` yields the concatenation of the outputs of `f` and `g`.
For example, `1, 2 --> 1 2`.

### Composition

The filter `f | g` runs `f`, and
for every output `y` of `f`, it runs `g` with `y` as input and yields its outputs.
For example, `(1, 2) | (., 3) --> 1 3 2 3`.

If either `f` or `g` yields an error, then
`f | g` yields that error, followed by nothing.
For example, `(1, 2) | (., error)` yields the same as `1, error`.

### Variable binding

The filter `f as $y | g` runs `f`, and
for every output `y` of `f`, it binds the value of `y` to the *variable* `$y`,
then runs `g` *on the original input*,
replacing any reference to `$y` in `g` by the value `y`.
For example,
`"Hello" | length as $x | . + " has length \($x)" --> "Hello has length 5"`.

### Filter definition

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
`"@ABC" | tobytes | .[0] --> 64`.

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

## More filters

### if-then-else

### try-catch

### label-break

### reduce/foreach



# Standard library

## I/O

### `inputs`

## Byte strings

### `tobytes`

### `byteoffset`





# Author

Michael Färber

# See also
