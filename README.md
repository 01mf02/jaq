% jaq


jaq is a clone of the JSON data processing tool [jq].
jaq aims to support a large subset of jq's syntax and operations.

jaq focusses on three goals:

* **Correctness**:
  jaq aims to provide a more correct and predictable implementation of jq,
  while preserving compatibility with jq in most cases.
  <details><summary>Examples of surprising jq behaviour</summary>

  * `nan > nan` is false, while `nan < nan` is true.
  * `[[]] | implode` crashes jq, and this was not fixed at the time of writing despite
    [being known since five years](https://github.com/stedolan/jq/issues/1160).
  * The [jq manual] claims that `limit(n; exp)` "extracts up to `n` outputs from `exp`".
    This holds for values of `n > 1`, e.g. `jq -n '[limit(2; 1, 2, 3)]'` yields
    `[1, 2]`, but when `n == 0`, `jq -n '[limit(0; 1, 2, 3)]'` yields `[1]` instead of `[]`.
    And perhaps even worse, when `n < 0`, then `limit` yields *all* outputs from `exp`,
    which is not documented.

  </details>
* **Performance**:
  I created jaq originally because I was bothered by
  [jq's long start-up time](https://github.com/stedolan/jq/issues/1411),
  which amounts to about 50ms on my machine.
  This can particularly show when processing of a large number of small files.
  jaq starts up about 30 times faster than jq and
  [outperforms jq also on many other benchmarks](#performance).
* **Simplicity**:
  jaq aims to have a simple and small implementation, in order to
  reduce the potential for bugs and to
  facilitate contributions.

I drew inspiration from another Rust program, namely [jql].
However, unlike jql, jaq aims to closely imitate jq's syntax and semantics.
This should allow users proficient in jq to easily use jaq.

[jq]: https://stedolan.github.io/jq/
[jql]: https://github.com/yamafaktory/jql



# Installation

To compile jaq, you need a Rust toolchain.
See <https://rustup.rs/> for instructions.
(Note that Rust compilers shipped with Linux distributions
may be too outdated to compile jaq. I use Rust 1.59.)

The following command installs the latest stable jaq:

    cargo install jaq

And the latest development version:

    cargo install --branch main --git https://github.com/01mf02/jaq

On my system, both commands place the executable at `~/.cargo/bin/jaq`.
jaq should work on any system supported by Rust.
If it does not, please file an issue.



# Examples

The following examples should give an impression of what jaq can currently do.
You should obtain the same outputs by replacing jaq with jq.
If not, your filing an issue would be appreciated. :)
The syntax is documented in the [jq manual].

[jq manual]: https://stedolan.github.io/jq/manual/

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



# Performance

The following benchmark compares the performance of jaq and jq 1.6.
Each command is run via `jq -n '<CMD>'` and `jaq -n '<CMD>'`, respectively.

| Command                                                                            | jaq \[s\] | jq \[s\] |
| ---------------------------------------------------------------------------------- | --------: | -------: |
| `empty` (128 iterations)                                                           |  **0.21** |    6.46  |
| `[range(1000000)] \| reverse \| length`                                            |  **0.03** |    1.32  |
| `[range(1000000) \| -.] \| sort \| length`                                         |  **0.15** |    1.44  |
| `[range(1000000) \| [.]] \| add \| length`                                         |  **0.62** |    1.44  |
| `[range(100000) \| {(tostring): .}] \| add \| length`                              |  **0.21** |    0.37  |
| `[range( 5000) \| {(tostring): .}] \| add \| .[] += 1 \| length`                   |  **0.02** |    2.94  |
| `[range(100000) \| {(tostring): .}] \| add \| with_entries(.value += 1) \| length` |  **0.67** |    1.68  |
| `[limit(1000000; repeat("a"))] \| add \| explode \| implode \| length`             |  **0.84** |    2.11  |
| `def trees: recurse([., .]); 0 \| nth(16; trees) \| flatten \| length`             |  **0.35** |    0.54  |
| `"[" + ([range(100000) \| tojson] \| join(",")) + "]" \| fromjson \| add`          |    5.69   | **3.16** |

I generated the benchmark data with `bench.sh`, followed by `pandoc -t gfm`.



# Features

Here is an overview of the features
already implemented and not yet implemented.
[Contributions to extend jaq are highly welcome.](#contributing)


## Basic features

- [x] Identity (`.`)
- [x] Basic data types (null, boolean, number, string, array, object)
- [x] Logical operators (`and`, `or`)
- [x] Equality and comparison operators (`.a == .b`, `.a < .b`)
- [x] Arithmetic operations on numbers (`+`, `-`, `*`, `/`, `%`)
- [x] Arithmetic operations on non-numbers (e.g., strings, arrays, objects)
- [ ] String interpolation
- [ ] Format strings (`@csv`, `@html`, `@json`)


## Paths

- [x] Indexing of arrays/objects (`.[0]`, `.a`, `.["a"]`)
- [x] Iterating over arrays/objects (`.[]`)
- [x] Optional indexing/iteration (`.a?`, `.[]?`)
- [x] Array slices (`.[3:7]`, `.[0:-1]`)
- [x] String slices


## Filter combinators

- [x] Composition (`|`)
- [x] Binding (`. as $x | $x`)
- [x] Concatenation (`,`)
- [x] Plain assignment (`=`)
- [x] Update assignment (`|=`)
- [x] Alternation (`//`)
- [x] if-then-else (`if .a < .b then .a else .b end`)
- [x] Reduction (`reduce .[] as $x (0, . + $x)`)
- [ ] Error handling (`try ... catch`)


## Definitions

- [x] Basic definitions (`def map(f): [.[] | f];`)
- [ ] Recursive definitions (`def r: r; r`)


## Core filters

- [x] Errors (`error`)
- [x] Length (`length`)
- [x] Rounding (`floor`, `round`, `ceil`)
- [x] String <-> JSON (`fromjson`, `tojson`)
- [x] String <-> integers (`explode`, `implode`)
- [x] String normalisation (`ascii_downcase`, `ascii_upcase`)
- [x] String splitting (`split("foo")`)
- [x] Array filters (`reverse`, `sort`, `sort_by(-.)`)
- [x] Stream consumers (`first`, `last`, `range`, `fold`)
- [x] Stream generators (`range`, `recurse`)
- [ ] More numeric filters (`sqrt`, `sin`, `log`, `pow`, ...)
- [ ] More string filters (`startswith`, `ltrimstr`, ...)
- [ ] More array filters (`group_by`, `min_by`, `max_by`, ...)


## Standard filters

These filters are defined via more basic filters.
Their definitions are at [`std.jq`](jaq-std/src/std.jq).

- [x] Undefined/Empty (`null`, `empty`)
- [x] Booleans (`true`, `false`, `not`)
- [x] Special numbers (`nan`, `infinite`, `isnan`, `isinfinite`, `isfinite`, `isnormal`)
- [x] Type (`type`)
- [x] Filtering (`select(. >= 0)`)
- [x] Selection (`values`, `nulls`, `booleans`, `numbers`, `strings`, `arrays`, `objects`, `iterables`, `scalars`)
- [x] Conversion (`tostring`, `tonumber`)
- [x] Iterable filters (`map(.+1)`, `map_values(.+1)`, `add`, `join("a")`, `min`, `max`)
- [x] Array filters (`transpose`, `first`, `last`, `nth(10)`, `flatten`)
- [x] Object-array conversion (`to_entries`, `from_entries`, `with_entries`)
- [x] Universal/existential (`all`, `any`)


## Advanced features

jaq currently does *not* aim to support the advanced features of jq, such as:

- Modules
- I/O
- Dates
- Regular expressions
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
    Error: cannot use 1 as integer
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

In jq, `n / 0` yields `nan` (not a number) if `n == 0` and fails otherwise.
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
whereas jq may silently convert to 64-bit floating-point numbers:

    $ echo '1e500' | jq '.'
    1.7976931348623157e+308
    $ echo '1e500' | jaq '.'
    1e500

Therefore, unlike jq, jaq satisfies the following paragraph in the [jq manual]:

> An important point about the identity filter is that
> it guarantees to preserve the literal decimal representation of values.
> This is particularly important when dealing with numbers which can't be
> losslessly converted to an IEEE754 double precision representation.


## Assignments

jaq does not allow so-called "complex assignments" of the form `p |= f`,
where `p` is a filter that is not a path expression.

For example:

    $ jq -n '[0, 1, 2] | (.[] | select(.<2)) |= .+1'
    [1, 2, 2]

This is not accepted in jaq, because `.[] | select(.<2)` is not a path expression.
A slightly more verbose version is allowed in jaq:

    $ jaq -n '[0, 1, 2] | .[] |= if .<2 then .+1 else . end'
    [1, 2, 2]


## Definitions

Like jq, jaq allows for the defininition of filters, such as:

    def map(f): [.[] | f];

However, unlike in jq, such filters in jaq cannot refer to themselves.
Furthermore, jaq does not support nested filters.
That is, a filter such as `recurse` cannot be defined in jaq:

    def recurse(f): def r: ., (f | r); r;

Note that while `recurse` cannot be defined manually in jaq,
jaq provides `recurse` as core filter.



# Contributing

Contributions to jaq are welcome.
In particular, implementing various filters of jq in jaq
is a relatively low-hanging fruit.

To add a new core filter (such as `group_by`), it suffices to:

1. Implement the filter in [the `filter` module](jaq-core/src/filter.rs).
2. Add a test with the filter name to [`tests.rs`](jaq-core/tests/named.rs),
   and check whether jq yields the same results.
3. Add derived filters to [the standard library](jaq-std/src/std.jq).

Voilà!

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
