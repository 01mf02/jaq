jaq playground
==============

The [jaq playground](https://gedenkt.at/jaq/)
allows executing jq programs with the jq interpreter _jaq_.
The jaq playground processes all data on the machine of its user.
This has a few advantages:

- *Privacy*: The jaq playground sends no data to the Internet.
- *Speed*: Because your own computer does the processing,
  you get answers in milliseconds, not seconds.
- *Offline*: You can use the jaq playground even without an internet connection.
- *Streaming*:
  The jaq playground shows you outputs as soon as they arrive, whereas
  the jq  playground shows you outputs only once there are no more new outputs.
  This means that you can process infinite data streams with the jaq playground.


## Usage

The playground is divided into three main sections:

- Filter: the jq program that should be executed
- Input: the input that is given to the jq program (by default in JSON)
- Output: the output of the jq program

When clicking on the "Run" button,
the jq program is executed with the given input, and its output is shown.

To try it, you can use the filter `.[]`
(which destructures a value into its components) and the following input:

~~~ json
["Hello", "World"]
{"x": 1, "y": 2}
~~~

Running this should yield the following outputs:

~~~ json
"Hello"
"World"
1
2
~~~

To show that you can yield an infinite output stream, you can use the filter
`range(0; infinite)` which yields the stream of natural numbers (0, 1, 2, ...).
Because there are infinitely many of those, the execution will never terminate.
For that reason, during program execution,
the "Run" button turns into a "Stop" button,
which you can use to terminate execution.


## Settings

You can configure the execution of jq programs with the "Settings" dialogue.
It has several settings, which influence how inputs are processed and
how the outputs should be displayed.

### Input Settings

- raw: Reads every line as string instead of as JSON value.
- slurp: Collects all input values into an array.
  When combined with "raw", reads the whole input as a single string.
- null: Yields only the `null` value as input.
  This is particularly useful in conjunction with the
  [`input`](https://jqlang.github.io/jq/manual/#input) filter.

For example, with the filter `.` and the input `[1, 2] "x"`,
no option yields `[1, 2] "x"`,
"raw" yields `"[1, 2] \"x\""`,
"slurp" yields `[[1, 2], "x"]`, and
"null" yields just `null`.

### Output Settings

- raw: Prints output strings without quotes and escaping.
- compact: Prints output values without intermediate spaces.
- tab: Indent with tab characters. This has precedence over "indent".
- indent: Indent with the given amount of white-space characters.

For example, with the filter `.` and the input `[1, 2] "x"`,
"raw" yields `"[     1,     2 ] x"`,
"compact" yields `[1,2] "x"`,
"tab" yields `[ 	1, 	2 ] "x"`, and
"indent" set to 2 yields `[   1,   2 ] "x"`.
(To make the output more compact, I replaced newline with space.)


## Building

First install `wasm-pack`:

    cargo install wasm-pack

Compile the WASM binaries (use `--release` instead of `--dev` for better performance):

    wasm-pack build --target web --no-typescript --no-pack --dev

To serve:

    python3 -m http.server --bind 127.0.0.1
