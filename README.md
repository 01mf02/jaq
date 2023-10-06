# jaq

![Build status](https://github.com/01mf02/jaq/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/jaq-interpret.svg)](https://crates.io/crates/jaq-interpret)
[![Documentation](https://docs.rs/jaq-interpret/badge.svg)](https://docs.rs/jaq-interpret)
[![Rust 1.62+](https://img.shields.io/badge/rust-1.62+-orange.svg)](https://www.rust-lang.org)

jaq (pronounced like *Jacques*[^jacques]) is a clone of the JSON data processing tool [jq].
jaq aims to support a large subset of jq's syntax and operations.

jaq focusses on three goals:

* **Correctness**:
  jaq aims to provide a more correct and predictable implementation of jq,
  while preserving compatibility with jq in most cases.
  <details><summary>Examples of surprising jq behaviour</summary>

  * `nan > nan` is false, while `nan < nan` is true.
  * `[[]] | implode` crashes jq, and this was not fixed at the time of writing despite
    [being known since five years](https://github.com/jqlang/jq/issues/1160).
  * The [jq manual] claims that `limit(n; exp)` "extracts up to `n` outputs from `exp`".
    This holds for values of `n > 1`, e.g. `jq -n '[limit(2; 1, 2, 3)]'` yields
    `[1, 2]`, but when `n == 0`, `jq -n '[limit(0; 1, 2, 3)]'` yields `[1]` instead of `[]`.
    And perhaps even worse, when `n < 0`, then `limit` yields *all* outputs from `exp`,
    which is not documented.

  </details>
* **Performance**:
  I created jaq originally because I was bothered by
  [jq's long start-up time](https://github.com/jqlang/jq/issues/1411),
  which amounts to about 50ms on my machine.
  This can particularly show when processing a large number of small files.
  jaq starts up about 30 times faster than jq and
  [outperforms jq also on many other benchmarks](#performance).
* **Simplicity**:
  jaq aims to have a simple and small implementation, in order to
  reduce the potential for bugs and to
  facilitate contributions.

I drew inspiration from another Rust program, namely [jql].
However, unlike jql, jaq aims to closely imitate jq's syntax and semantics.
This should allow users proficient in jq to easily use jaq.

[jq]: https://jqlang.github.io/jq/
[jql]: https://github.com/yamafaktory/jql

[^jacques]: I wanted to create a tool that should be discreet and obliging, like a good waiter.
  And when I think of a typical name for a (French) waiter, to my mind comes "Jacques".
  Later, I found out about the old French word *jacquet*, meaning "squirrel",
  which makes for a nice *ex post* inspiration for the name.



# Installation


## From Source

To compile jaq, you need a Rust toolchain.
See <https://rustup.rs/> for instructions.
(Note that Rust compilers shipped with Linux distributions
may be too outdated to compile jaq.)

Any of the following commands install jaq:

    $ cargo install --locked jaq
    $ cargo install --locked --git https://github.com/01mf02/jaq # latest development version

On my system, both commands place the executable at `~/.cargo/bin/jaq`.

If you have cloned this repository, you can also build jaq by executing one of the commands in the cloned repository:

    $ cargo build --release # places binary into target/release/jaq
    $ cargo install --locked --path jaq # installs binary

jaq should work on any system supported by Rust.
If it does not, please file an issue.


## Binaries

You may also install jaq using [homebrew](https://formulae.brew.sh/formula/jaq) on macOS or Linux:

    $ brew install jaq
    $ brew install --HEAD jaq # latest development version



# Examples

The following examples should give an impression of what jaq can currently do.
You should obtain the same outputs by replacing jaq with jq.
If not, your filing an issue would be appreciated. :)
The syntax is documented in the [jq manual].

[jq manual]: https://jqlang.github.io/jq/manual/v1.6/

Access a field:

    $ echo '{"a": 1, "b": 2}' | jaq '.a'
    1

Add values:

    $ echo '{"a": 1, "b": 2}' | jaq 'add'
    3

Construct an array from an object in two ways and show that they are equal:

    $ echo '{"a": 1, "b": 2}' | jaq '[.a, .b] == [.[]]'
    true

Apply a filter to all elements of an array and filter the results:

    $ echo '[0, 1, 2, 3]' | jaq 'map(.*2) | [.[] | select(. < 5)]'
    [0, 2, 4]

Read (slurp) input values into an array and get the average of its elements:

    $ echo '1 2 3 4' | jaq -s 'add / length'
    2.5

Repeatedly apply a filter to itself and output the intermediate results:

    $ echo '0' | jaq '[recurse(.+1; . < 3)]'
    [0, 1, 2]

Lazily fold over inputs and output intermediate results:

    $ seq 1000 | jaq -n 'foreach inputs as $x (0; . + $x)'
    1 3 6 10 15 [...]



# Performance

The following evaluation consists of several benchmarks that
allow comparing the performance of jaq, jq, and [gojq].
The `empty` benchmark runs `n` times the filter `empty` with null input,
serving to measure the startup time.
The `bf-fib` benchmark runs a Brainfuck interpreter written in jq,
interpreting a Brainfuck script that produces `n` Fibonacci numbers.
The other benchmarks evaluate various filters with `n` as input;
see [`bench.sh`](bench.sh) for details.

I generated the benchmark data with
`bench.sh target/release/jaq jq-1.7 gojq-0.12.13 jq-1.6 | tee bench.json`
on a Linux system with an AMD Ryzen 5 5500U.[^binaries]
I then processed the results with a "one-liner" (stretching the term and the line a bit):

    jq -rs '.[] | "|`\(.name)`|\(.n)|" + ([.time[] | min | (.*1000|round)? // "N/A"] | min as $total_min | map(if . == $total_min then "**\(.)**" else "\(.)" end) | join("|"))' bench.json

(Of course, you can also use jaq here instead of jq.)
Finally, I concatenated the table header with the output and piped it through `pandoc -t gfm`.

[^binaries]:
  The binaries for jq-1.7 and gojq-0.12.13 were retrieved from their GitHub release pages,
  the binary for jq-1.6 was installed from the standard Ubuntu repository.

Table: Evaluation results in milliseconds ("N/A" if more than 10 seconds).

| Benchmark      |       n | jaq-1.0 |  jq-1.7 | gojq-0.12.13 | jq-1.6 |
| -------------- | ------: | ------: | ------: | -----------: | -----: |
| `empty`        |     512 | **550** |     610 |          570 |   8450 |
| `bf-fib`       |      13 | **380** |    1290 |         1050 |   1430 |
| `reverse`      | 1048576 |  **20** |     690 |          320 |    650 |
| `sort`         | 1048576 | **100** |     540 |          550 |    660 |
| `group-by`     | 1048576 | **380** |    1880 |         1690 |   2790 |
| `min-max`      | 1048576 | **180** |     340 |          300 |    350 |
| `add`          | 1048576 | **460** |     640 |         1430 |    740 |
| `kv`           |  131072 | **140** | **140** |          240 |    200 |
| `kv-update`    |  131072 | **160** |     550 |          550 |    N/A |
| `kv-entries`   |  131072 | **550** |    1160 |          780 |   1110 |
| `ex-implode`   | 1048576 |     730 |    1130 |      **690** |   1100 |
| `reduce`       | 1048576 | **700** |     890 |          N/A |    850 |
| `try-catch`    | 1048576 | **150** |     330 |          530 |    680 |
| `tree-flatten` |      17 |     430 |     360 |        **0** |    480 |
| `tree-update`  |      17 | **270** |     970 |         1830 |   1180 |
| `tree-paths`   |      17 |    1420 | **360** |          890 |    480 |
| `to-fromjson`  |   65536 |  **30** |     370 |          110 |    390 |
| `ack`          |       7 | **500** |     690 |         1070 |    620 |

The results show that
jaq-1.0 is fastest on 15 benchmarks, whereas
jq-1.7 is fastest on 2 benchmarks and
gojq-0.12.13 is fastest on 2 benchmarks.
jq-1.6 is slowest on all benchmarks.
gojq is much faster on `tree-flatten` because it implements the filter `flatten` natively instead of by definition.

[gojq]: https://github.com/itchyny/gojq



# Features

Here is an overview that summarises:

* [x] features already implemented, and
* [ ] features not yet implemented.

[Contributions to extend jaq are highly welcome.](#contributing)


## Basics

- [x] Identity (`.`)
- [x] Recursion (`..`)
- [x] Basic data types (null, boolean, number, string, array, object)
- [x] if-then-else (`if .a < .b then .a else .b end`)
- [x] Folding (`reduce .[] as $x (0; . + $x)`, `foreach .[] as $x (0; . + $x; . + .)`)
- [x] Error handling (`try ... catch ...`) (see the [differences from jq](#error-handling))
- [x] String interpolation (`"The successor of \(.) is \(.+1)."`)
- [x] Format strings (`@json`, `@text`, `@csv`, `@tsv`, `@html`, `@sh`, `@base64`, `@base64d`)


## Paths

- [x] Indexing of arrays/objects (`.[0]`, `.a`, `.["a"]`)
- [x] Iterating over arrays/objects (`.[]`)
- [x] Optional indexing/iteration (`.a?`, `.[]?`)
- [x] Array slices (`.[3:7]`, `.[0:-1]`)
- [x] String slices


## Operators

- [x] Composition (`|`)
- [x] Binding (`. as $x | $x`)
- [x] Concatenation (`,`)
- [x] Plain assignment (`=`)
- [x] Update assignment (`|=`, `+=`, `-=`)
- [x] Alternation (`//`)
- [x] Logic (`or`, `and`)
- [x] Equality and comparison (`.a == .b`, `.a < .b`)
- [x] Arithmetic (`+`, `-`, `*`, `/`, `%`)
- [x] Negation (`-`)
- [x] Error suppression (`?`)


## Definitions

- [x] Basic definitions (`def map(f): [.[] | f];`)
- [x] Recursive definitions (`def r: r; r`)


## Core filters

- [x] Empty (`empty`)
- [x] Errors (`error`)
- [x] Input (`inputs`)
- [x] Length (`length`, `utf8bytelength`)
- [x] Rounding (`floor`, `round`, `ceil`)
- [x] String <-> JSON (`fromjson`, `tojson`)
- [x] String <-> integers (`explode`, `implode`)
- [x] String normalisation (`ascii_downcase`, `ascii_upcase`)
- [x] String prefix/postfix (`startswith`, `endswith`, `ltrimstr`, `rtrimstr`)
- [x] String splitting (`split("foo")`)
- [x] Array filters (`reverse`, `sort`, `sort_by(-.)`, `group_by`, `min_by`, `max_by`)
- [x] Stream consumers (`first`, `last`, `range`, `fold`)
- [x] Stream generators (`range`, `recurse`)
- [x] Time (`now`, `fromdateiso8601`, `todateiso8601`)
- [x] More numeric filters (`sqrt`, `sin`, `log`, `pow`, ...) ([list of numeric filters](#numeric-filters))
- [ ] More time filters (`strptime`, `strftime`, `strflocaltime`, `mktime`, `gmtime`, and `localtime`)

## Standard filters

These filters are defined via more basic filters.
Their definitions are at [`std.jq`](jaq-std/src/std.jq).

- [x] Undefined (`null`)
- [x] Booleans (`true`, `false`, `not`)
- [x] Special numbers (`nan`, `infinite`, `isnan`, `isinfinite`, `isfinite`, `isnormal`)
- [x] Type (`type`)
- [x] Filtering (`select(. >= 0)`)
- [x] Selection (`values`, `nulls`, `booleans`, `numbers`, `strings`, `arrays`, `objects`, `iterables`, `scalars`)
- [x] Conversion (`tostring`, `tonumber`)
- [x] Iterable filters (`map(.+1)`, `map_values(.+1)`, `add`, `join("a")`)
- [x] Array filters (`transpose`, `first`, `last`, `nth(10)`, `flatten`, `min`, `max`)
- [x] Object-array conversion (`to_entries`, `from_entries`, `with_entries`)
- [x] Universal/existential (`all`, `any`)
- [x] Recursion (`walk`)
- [x] I/O (`input`)
- [x] Regular expressions (`test`, `scan`, `match`, `capture`, `splits`, `sub`, `gsub`)
- [x] Time (`fromdate`, `todate`)

## Numeric filters

jaq imports many filters from [libm](https://crates.io/crates/libm)
and follows their type signature.

<details><summary>Full list of numeric filters defined in jaq</summary>

Zero-argument filters:

- [x] `acos`
- [x] `acosh`
- [x] `asin`
- [x] `asinh`
- [x] `atan`
- [x] `atanh`
- [x] `cbrt`
- [x] `cos`
- [x] `cosh`
- [x] `erf`
- [x] `erfc`
- [x] `exp`
- [x] `exp10`
- [x] `exp2`
- [x] `expm1`
- [x] `fabs`
- [x] `frexp`, which returns pairs of (float, integer).
- [x] `ilogb`, which returns integers.
- [x] `j0`
- [x] `j1`
- [x] `lgamma`
- [x] `log`
- [x] `log10`
- [x] `log1p`
- [x] `log2`
- [x] `logb`
- [x] `modf`, which returns pairs of (float, float).
- [x] `nearbyint`
- [x] `pow10`
- [x] `rint`
- [x] `significand`
- [x] `sin`
- [x] `sinh`
- [x] `sqrt`
- [x] `tan`
- [x] `tanh`
- [x] `tgamma`
- [x] `trunc`
- [x] `y0`
- [x] `y1`

Two-argument filters that ignore `.`:

- [x] `atan2`
- [x] `copysign`
- [x] `drem`
- [x] `fdim`
- [x] `fmax`
- [x] `fmin`
- [x] `fmod`
- [x] `hypot`
- [x] `jn`, which takes an integer as first argument.
- [x] `ldexp`, which takes an integer as second argument.
- [x] `nextafter`
- [x] `nexttoward`
- [x] `pow`
- [x] `remainder`
- [x] `scalb`
- [x] `scalbln`, which takes as integer as second argument.
- [x] `yn`, which takes an integer as first argument.

Three-argument filters that ignore `.`:

- [x] `fma`

</details>

## Advanced features

jaq currently does *not* aim to support several features of jq, such as:

- Modules
- SQL-style operators
- Streaming



# Differences between jq and jaq


## Numbers

jq uses 64-bit floating-point numbers (floats) for any number.
By contrast, jaq interprets
numbers such as 0   or -42 as machine-sized integers and
numbers such as 0.0 or 3e8 as 64-bit floats.
Many operations in jaq, such as array indexing,
check whether the passed numbers are indeed integer.
The motivation behind this is to avoid
rounding errors that may silently lead to wrong results.
For example:

    $ jq  -n '[0, 1, 2] | .[1.0000000000000001]'
    1
    $ jaq -n '[0, 1, 2] | .[1.0000000000000001]'
    Error: cannot use 1.0 as integer
    $ jaq -n '[0, 1, 2] | .[1]'
    1

The rules of jaq are:

* The sum, difference, product, and remainder of two integers is integer.
* Any other operation between two numbers yields a float.

Examples:

    $ jaq -n '1 + 2'
    3
    $ jaq -n '10 / 2'
    5.0
    $ jaq -n '1.0 + 2'
    3.0

You can convert an integer to a floating-point number e.g.
by adding 0.0, by multiplying with 1.0, or by dividing with 1.
You can convert a floating-point number to an integer by
`round`, `floor`, or `ceil`:

    $ jaq -n '1.2 | [floor, round, ceil]'
    [1, 1, 2]

### NaN and infinity

In jq, division by 0 has some surprising properties; for example,
`0 / 0` yields `nan`, whereas
`0 as $n | $n / 0` yields an error.
In jaq, `n / 0` yields `nan` if `n == 0`, `infinite` if `n > 0`, and `-infinite` if `n < 0`.
jaq's behaviour is closer to the IEEE standard for floating-point arithmetic (IEEE 754).

jaq implements a total ordering on floating-point numbers to allow sorting values.
Therefore, it unfortunately has to enforce that `nan == nan`.
(jq gets around this by enforcing `nan < nan`, which breaks basic laws about total orders.)

Like jq, jaq prints `nan` and `infinite` as `null` in JSON,
because JSON does not support encoding these values as numbers.

### Preservation of fractional numbers

jaq preserves fractional numbers coming from JSON data perfectly
(as long as they are not used in some arithmetic operation),
whereas jq 1.6 may silently convert to 64-bit floating-point numbers:

    $ echo '1e500' | jq '.'
    1.7976931348623157e+308
    $ echo '1e500' | jaq '.'
    1e500

Therefore, unlike jq 1.6, jaq satisfies the following paragraph in the [jq manual]:

> An important point about the identity filter is that
> it guarantees to preserve the literal decimal representation of values.
> This is particularly important when dealing with numbers which can't be
> losslessly converted to an IEEE754 double precision representation.

Please note that newer versions of jq, e.g. 1.7,
seem to preserve the literal decimal representation as well.


## Assignments

Like jq, jaq allows for assignments of the form `p |= f`.
However, jaq interprets these assignments differently.
Fortunately, in most cases, the result is the same.

In jq, an assignment `p |= f` first constructs paths to all values that match `p`.
*Only then*, it applies the filter `f` to these values.

In jaq, an assignment `p |= f` applies `f` *immediately* to any value matching `p`.
Unlike in jq, assignment does not explicitly construct paths.

jaq's implementation of assignment likely yields higher performance,
because it does not construct paths.
Furthermore, this also prevents several bugs in jq "by design".
For example, given the filter `[0, 1, 2, 3] | .[] |= empty`,
jq  yields `[1, 3]`, whereas
jaq yields `[]`.
What happens here?

jq first constructs the paths corresponding to `.[]`, which are `.0, .1, .2, .3`.
Then, it removes the element at each of these paths.
However, each of these removals *changes* the value that the remaining paths refer to.
That is, after removing `.0` (value 0), `.1` does not refer to value 1, but value 2!
That is also why value 1 (and in consequence also value 3) is not removed.

There is more weirdness ahead in jq;
for example, `0 | 0 |= .+1` yields `1` in jq,
although `0` is not a valid path expression.
However, `1 | 0 |= .+1` yields an error.
In jaq, any such assignment yields an error.

jaq attempts to use multiple outputs of the right-hand side, whereas
jq uses only the first.
For example, `0 | (., .) |= (., .+1)` yields `0 1 1 2` in jaq,
whereas it yields only `0` in jq.
However, `{a: 1} | .a |= (2, 3)` yields `{"a": 2}` in both jaq and jq,
because an object can only associate a single value with any given key,
so we cannot use multiple outputs in a meaningful way here.

Because jaq does not construct paths,
it does not allow some filters on the left-hand side of assignments,
for example `first`, `last`, `limit`:
For example, `[1, 2, 3] | first(.[]) |= .-1`
yields `[0, 2, 3]` in jq, but is invalid in jaq.
Similarly, `[1, 2, 3] | limit(2; .[]) |= .-1`
yields `[0, 1, 3]` in jq, but is invalid in jaq.
(Inconsequentially, jq also does not allow for `last`.)


## Definitions

Like jq, jaq allows for the definition of filters, such as:

    def map(f): [.[] | f];

Arguments can also be passed *by value*, such as:

    def cartesian($f; $g): [$f, $g];

Filter definitions can be nested and recursive, i.e. refer to themselves.
That is, a filter such as `recurse` can be defined in jaq:

    def recurse(f): def r: ., (f | r); r;

However, note that unlike jq, jaq does not optimise tail calls.
Therefore, using the above definition of `recurse`, e.g. by `last(recurse(.))`,
grows the stack in jaq (leading to a stack overflow), while it does not in jq.
As a remedy, jaq provides `recurse` as core filter,
which tries to avoid growing the stack if possible.

Compared to jq, jaq imposes an important syntactic restriction on recursive filters,
namely that *recursive filters may only have variable arguments*.
That is, in jaq, we cannot define a filter like:

    def f(a): a, f(1+a);

Recursive filters with non-variable arguments can yield surprising effects;
for example, a call `f(0)` builds up calls of the shape `f(1+(..(1+0)...))`,
which leads to exponential execution times.

Recursive filters with non-variable arguments can
very frequently be alternatively implemented by either:

* A nested filter: for example, instead of
  `def walk(f): (.[]? |= walk(f)) | f;`, you can use
  `def walk(f): def rec: (.[]? |= rec) | f; rec;`.
* A filter with variable arguments: for example, instead of
  `def f(a): a, f(1+a);`, you can equally well write
  `def f($a): $a, f(1+$a);`.
* A filter with `recurse`: for example, you may write
  `def f(a): a | recurse(1+.);`.
  If you expect your filter to recurse deeply,
  it is advised to implement it using `recurse`,
  because jaq has an optimised implementation of `recurse`.

All of these options are supported by jaq.


## Arguments

Like jq, jaq allows to define arguments via the command line,
in particular by the options `--arg`, `--rawfile`, `--slurpfile`.
This binds variables to values, and
for every variable `$x` bound to `v` this way,
`$ARGS.named` contains an entry with key `x` and value `v`.
For example:

~~~
$ jaq -n --arg x 1 --arg y 2 '$x, $y, $ARGS.named'
"1"
"2"
{
  "x": "1",
  "y": "2"
}
~~~


## Folding

jq and jaq provide filters
`reduce xs as $x (init; f)` and
`foreach xs as $x (init; f)`.

In jaq, the output of these filters is defined very simply:
Assuming that `xs` evaluates to `x0`, `x1`, ..., `xn`,
`reduce xs as $x (init; f)` evaluates to

~~~
init
| x0 as $x | f
| ...
| xn as $x | f
~~~

and `foreach xs as $x (init; f)` evaluates to

~~~ text
init
| x0 as $x | f | (.,
| ...
| xn as $x | f | (.,
empty)...)
~~~

Additionally, jaq provides the filter `for xs as $x (init; f)` that evaluates to

~~~ text
init
| ., (x0 as $x | f
| ...
| ., (xn as $x | f
)...)
~~~

The difference between `foreach` and `for` is that
`for` yields the output of `init`, whereas `foreach` omits it.
For example,
`foreach (1, 2, 3) as $x (0; .+$x)` yields `1, 3, 6`, whereas
`for (1, 2, 3) as $x (0; .+$x)` yields `0, 1, 3, 6`.

The interpretation of `reduce`/`foreach` in jaq has the following advantages over jq:

* It deals very naturally with filters that yield multiple outputs.
  In contrast, jq discriminates outputs of `f`,
  because it recurses only on the last of them,
  although it outputs all of them.
  <details><summary>Example</summary>
  `foreach (5, 10) as $x (1; .+$x, -.)` yields
  `6, -1, 9, 1` in jq, whereas it yields
  `6, 16, -6, -1, 9, 1` in jaq.
  We can see that both jq and jaq yield the values `6` and `-1`
  resulting from the first iteration (where `$x` is 5), namely
  `1 | 5 as $x | (.+$x, -.)`.
  However, jq performs the second iteration (where `$x` is 10)
  *only on the last value* returned from the first iteration, namely `-1`,
  yielding the values `9` and `1` resulting from
  `-1 | 10 as $x | (.+$x, -.)`.
  jaq yields these values too, but it also performs the second iteration
  on all other values returned from the first iteration, namely `6`,
  yielding the values `16` and `-6` that result from
  ` 6 | 10 as $x | (.+$x, -.)`.
  </details>
* It makes the implementation of `reduce` and `foreach`
  special cases of the same code, reducing the potential for bugs.

Compared to `foreach ...`, the filter `for ...`
(where `...` refers to `xs as $x (init; f)`)
has a stronger relationship with `reduce`.
In particular,
the values yielded by `reduce ...` are a subset of
the values yielded by `for ...`.
This does not hold if you replace `for` by `foreach`.
<details>
As an example, if we set `...` to `empty as $x (0; .+$x)`, then
`foreach ...` yields no value, whereas
`for ...` and `reduce ...` yield `0`.
</details>

Furthermore, jq provides the filter
`foreach xs as $x (init; f; proj)` (`foreach/3`) and interprets
`foreach xs as $x (init; f)` (`foreach/2`) as
`foreach xs as $x (init; f; .)`, whereas
jaq does *not* provide `foreach/3` because
it requires completely separate logic from `foreach/2` and `reduce`
in both the parser and the interpreter.


## Error handling

In jq, the `try f catch g` expression breaks out of the `f` stream as
soon as an error occurs, ceding control to `g` after that. This is
mentioned in its manual as a possible mechanism for breaking out of
loops
([here](https://jqlang.github.io/jq/manual/#breaking-out-of-control-structures)). jaq
however doesn't interrupt the `f` stream, but instead sends _each_
error value emitted to the `g` filter; the result is a stream of
values emitted from `f` with values emitted from `g` interspersed
where errors ocurred.

Consider the following example: this expression is `true` in jq,
because the first `error(2)` interrupts the stream:

```jq
[try (1, error(2), 3, error(4)) catch .] == [1, 2]
```

In jaq however, this holds:

```jq
[try (1, error(2), 3, error(4)) catch .] == [1, 2, 3, 4]
```


## Miscellaneous

* Slurping: When files are slurped in (via the `-s` / `--slurp` option),
  jq combines the inputs of all files into one single array, whereas
  jaq yields an array for every file.
  The behaviour of jq can be approximated in jaq;
  for example, to achieve the output of
  `jq -s . a b`, you may use
  `jaq -s . <(cat a b)`.
* Cartesian products:
  In jq, `[(1,2) * (3,4)]` yields `[3, 6, 4, 8]`, whereas
  `[{a: (1,2), b: (3,4)} | .a * .b]` yields `[3, 4, 6, 8]`.
  jaq yields `[3, 4, 6, 8]` in both cases.
* List updating:
  In jq, `[0, 1] | .[3] = 3` yields `[0, 1, null, 3]`; that is,
  jq fills up the list with `null`s if we update beyond its size.
  In contrast, jaq fails with an out-of-bounds error in such a case.
* Input reading:
  When there is no more input value left,
  in jq, `input` yields an error, whereas in jaq, it yields no output value.
* Joining:
  When given an array `[x0, x1, ..., xn]`,
  in jq, `join(x)` converts all elements of the input array to strings and intersperses them with `x`, whereas
  in jaq, `join(x)` simply calculates `x0 + x + x1 + x + ... + xn`.
  When all elements of the input array and `x` are strings, jq and jaq yield the same output.
* Ranges:
  The filter `range(m; n)` constructs a sequence of numbers `m, m+1, ...`,
  where any number must be smaller than `n`.
  In jq,  `m` and `n` can be floating-point numbers, whereas
  in jaq, `m` and `n` must be integers.
  This is to avoid potential numerical stability problems.
  That means that unlike in jq, you cannot use
  `range(m; infinite)` to generate the infinite sequence `m, m+1, ...`.
  However, you can use `m | recurse(.+1)` to achieve the same in jaq.



# Contributing

Contributions to jaq are welcome.
Please make sure that after your change, `cargo test` runs successfully.



# Acknowledgements

jaq has profited tremendously from:

* [serde_json] to read and [colored_json] to output JSON,
* [chumsky] to parse and [ariadne] to pretty-print parse errors,
* [mimalloc] to boost the performance of memory allocation, and
* the Rust standard library, in particular its awesome [Iterator],
  which builds the rock-solid base of jaq's filter execution

[serde_json]: https://docs.rs/serde_json/
[colored_json]: https://docs.rs/colored_json/
[chumsky]: https://docs.rs/chumsky/
[ariadne]: https://docs.rs/ariadne/
[mimalloc]: https://docs.rs/mimalloc/
[Iterator]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
