---
title: JAQ(1)
---



# Name

`jaq` --- Command-line JSON processor



# Synopsis

`jaq` \[_OPTION_\]... \[_FILTER_\] \[_FILE_\]...



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

# Options


## Input options

### `--from` {#--from}

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

### `-s`, `--slurp`

Read (slurp) all input values into one array.
For example,
`echo "1 2 3" | jaq -s` yields a single output, namely the array `[1, 2, 3]`, whereas
`echo "1 2 3" | jaq` yields three outputs, namely `1`, `2`, and `3`.


## Output options

### `--to` {#--to}

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
once there is no more output and if there has not been any error.

-S, --sort-keys           Print objects sorted by their keys
-C, --color-output        Always color output
-M, --monochrome-output   Do not color output
--tab                 Use tabs for indentation rather than spaces
--indent <N>          Use N spaces for indentation [default: 2]


## Compilation options

-f, --from-file           Read filter from a file given by filter argument
-L, --library-path <_DIR_>  Search for modules and data in given directory


## Variable options

--arg       <A> <V>   Set variable `$A` to string `V`
--argjson   <A> <V>   Set variable `$A` to JSON value `V`
--slurpfile <A> <F>   Set variable `$A` to array containing the JSON values in file `F`
--rawfile   <A> <F>   Set variable `$A` to string containing the contents of file `F`
--args                Collect remaining positional arguments into `$ARGS.positional`


# Core language

## Concatenation

`f, g`: Return the concatenation of the outputs of `f` and `g`.

::: Examples

~~~
.a, length
{"a": 0, "b": 1}
0
2
~~~

:::

## Composition

`f | g`: For each output of `f`, apply the output to g and return all its outputs.

# Standard library

## I/O

### `inputs`

## Byte strings

### `tobytes`

### `byteoffset`





# Author

Michael Färber

# See also
