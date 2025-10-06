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

When passing filters directly as _FILTER_ argument on the command-line,
care has to be taken to properly escape the filter.
How to do this depends from platform to platform, but on Unixoid systems,
surrounding the filter with single quotes (`'`) and
replacing occurrences of `'` in filters by `'\''` suffices.
For example, to run the filter `"'"` that
produces a string containing a single quote, you can use
`jaq -n '"'\''"'`.

Running filters that start with the [negation operator](#negation),
such as `jaq '-1'`, fails because `-` is interpreted as
start of a command-line switch rather than negation.
You can remedy this by using
`jaq -- '-1'` instead, or by surrounding the filter in parentheses, i.e.
`jaq '(-1)'`.


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

### `-R`, `--raw-input` {#--raw-input}

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
