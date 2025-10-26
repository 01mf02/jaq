{#cli}
# Command-line interface

Running
`jaq` \[_OPTION_\]... \[_FILTER_\] \[_FILE_\]...
performs the following steps:

- Parse _FILTER_ as jq program; see [jq language](#corelang)
- For each _FILE_:

    - Parse _FILE_ to a stream of values
    - For each input value in the file:

        - Run _FILTER_ on the input value and print its output values

For example, `jaq '.name?' persons.json`
parses the filter `.name?`, then
reads all values in `persons.json` one-by-one.
It then executes the filter `.name?` on each of the values and
prints the outputs of the filter as JSON.

There are a few rules:

- If no _FILTER_ is given, jaq uses `.` (the [identity] filter) as filter.
- If no _FILE_ is given, jaq reads from standard input.
- jaq determines the format to parse a _FILE_ as follows:

    - If [`--from`](#--from) _FORMAT_ is used, jaq uses that format.
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

{#--from}
### `--from` _FORMAT_

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

::: Compatibility
`jq` does not have this option.
:::

### `-n`, `--null-input`

Feed `null` as input to the main program, ignoring any input files.
For example,
`yes | jaq -n` yields `null`,
which shows that this does indeed not read any input.

The inputs can still be obtained via the [`inputs`](#inputs) filter; for example,
`yes true | jaq -n 'first(inputs)'` yields `true`.
This can be useful to fold over all inputs with [`reduce` / `foreach`](#reduce-foreach).

{#--raw-input}
### `-R`, `--raw-input`

Read lines of the input as sequence of strings.
For example,
`echo -e "Hello\nWorld" | jaq -R` yields two outputs; `"Hello"` and `"World"`.
When combined with `-s` (`--slurp`), this yields the whole input as a single string.
For example,
`echo -e "Hello\nWorld" | jaq -Rs` yields `"Hello\nWorld\n"`.

This is equivalent to `--from raw`.

::: Advanced
When using `-Rs` to load a file (as opposed to standard input),
jaq loads this file in constant time (if it can be memory-mapped).
This is because unlike jq, jaq does not validate that strings are valid UTF-8.
That permits loading arbitrary binary files;
these can be processed as byte strings via [`tobytes`](#tobytes).
:::

### `-s`, `--slurp`

Read (slurp) all input values into one array.
For example,
`echo "1 2 3" | jaq -s` yields a single output, namely the array `[1, 2, 3]`, whereas
`echo "1 2 3" | jaq` yields three outputs, namely `1`, `2`, and `3`.

::: Compatibility

When multiple files are slurped in,
`jq` combines the inputs of all files into one single array, whereas
jaq yields an array for every file.
This is motivated by jaq's [`--in-place`](#--in-place) option,
which could not work with the behaviour implemented by `jq`.
The behaviour of `jq` can be approximated in jaq;
for example, to achieve the output of
`jq -s . a b`, you may use
`jaq -s . <(cat a b)`.

:::


## Output options

{#--to}
### `--to` _FORMAT_

Print all output values in the given _FORMAT_.
Any _FORMAT_ accepted by [`--from`](#--from) can be used here.

Note that not every value can be printed in every format.
For example, TOML requires that the root value is an object, so
`jaq --to toml <<< []`
yields an error.

::: Compatibility

`jq` does not have this option.

:::

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

{#--in-place}
### `-i`, `--in-place`

Overwrite input file with its output.
For example,
`jaq -i . myfile.json` reads the file `myfile.json` and
overwrites it with a formatted version of it.
Note that the input file is overwritten only
once there is no more output and
if there has not been any error.

::: Compatibility

`jq` does not have this option.

:::

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

{#--from-file}
### `-f`, `--from-file`

Read filter from a file given by filter argument.

With this option, jaq interprets the _FILTER_ argument as
name of a file containing the filter.
Note that the file name may not directly succeed this option.
For example,
`jaq --from-file -n script.jq`
uses the contents of the file `script.jq` as filter.

### `-L`, `--library-path` _DIR_

Search for [modules](#modules) and data in given directory.

jaq searches for modules and data in a set of directories called "search paths".
Using `--library-path` adds a new directory to the search paths.
In these search paths,
`~` is substituted with the user's home directory
(given by the environment variable `HOME` on Linux and `USERPROFILE` on Windows) and
`$ORIGIN` is substituted by the directory in which the `jaq` executable resides.

For example,
`jaq -L . -L .. 'include "script"; foo'`
looks for `script.jq` first in the current directory, then in the parent directory.

If `--library-path` is not given, the following default search paths are used:

- `~/.jq`
- `$ORIGIN/../lib/jq`
- `$ORIGIN/../lib`


## Variable options

### `--arg` _A_ _V_

Set variable `$A` to string _V_.

For example,
`jaq --arg name "John Doe" -n '"Welcome, " + $name'` yields `"Welcome, John Doe"`.

### `--argjson` _A_ _V_

Set variable `$A` to JSON value _V_.

For example,
`jaq --argjson song '{"name": "One of Us", "artist": "ABBA", "year": 1981}' -n '"Currently playing: \($song.name) (\($song.year))"'`
yields
`"Currently playing: One of Us (1981)"`.

If _V_ contains more than a single value, e.g. `1 2`, then jaq yields an error.

### `--slurpfile` _A_ _F_

Set variable `$A` to array containing the JSON values in file _F_.

For example, if `values.json` contains `1 2 3`, then
`jaq --slurpfile xs values.json -n '$xs'` yields `[1, 2, 3]`.

### `--rawfile` _A_ _F_

Set variable `$A` to string containing the contents of file _F_.

### `--args`

Collect remaining positional arguments into `$ARGS.positional`.

If this option is given, then all further arguments that
would have been interpreted as input files are
instead collected into an array at `$ARGS.positional`.

For example, if the file `input.json` exists, then
`jaq '$ARGS.positional' input.json --args foo -n bar -- baz -c qux` yields
`["foo", "bar", "baz", "-c", "qux"]`.
Note that here, `input.json` and `-n` are *not* collected into the array ---
the former because it comes *before* `--args`, and
the latter because it would not have been interpreted as input file.
However, `-c` is collected into the array because it comes after `--`,
which leads every argument after it to be interpreted as input file.

