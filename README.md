% jaq


jaq is a clone of the JSON data processing tool [jq].
jaq aims to support a large subset of jq's syntax and operations.

I created jaq primarily because I was bothered by
[jq's long start-up time](https://github.com/stedolan/jq/issues/1411),
which amounts to about 50ms on my machine.
This can particularly show when processing of a large number of small files.
I have written more about my motivation
[here](https://github.com/01mf02/adam-notes#2020-12-04).

I drew inspiration from another Rust program, namely [jql].
However, unlike jql, jaq aims to closely imitate jq's syntax and semantics.
This should allow users proficient in jq to easily use jaq.

[jq]: https://stedolan.github.io/jq/
[jql]: https://github.com/yamafaktory/jql


# Installation

To use jaq, you need a Rust toolchain.
See <https://rustup.rs/> for instructions.
(Note that Rust compilers shipped with Linux distributions
may be too outdated to compile jaq. I use Rust 1.51.)

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


# Features

Here is an overview of the features
already implemented and not yet implemented.
Contributions to extend jaq are highly welcome, see below.

## Basic features

- [x] Basic data types (null, boolean, number, string, array, object)
- [x] Logical operators (`and`, `or`)
- [x] Equality and comparison operators (`.a == .b`, `.a < .b`)
- [x] Arithmetic operations on numbers (`+`, `-`, `*`, `/`, `%`)
- [x] Arithmetic operations on non-numbers (e.g., strings, arrays, objects)

## Paths

- [x] Identity (`.`)
- [x] Indexing of arrays/objects (`.[0]`, `.a`, `.["a"]`)
- [x] Iterating over arrays/objects (`.[]`)
- [x] Optional indexing/iteration (`.a?`, `.[]?`)
- [x] Array slices (`.[3:7]`, `.[0:-1]`)
- [x] String slices

## Assignment

- [x] Plain assignment (`=`)
- [x] Update assignment (`|=`)

## Filter combinators

- [x] Composition (`|`)
- [x] Concatenation (`,`)
- [x] if-then-else (`if .a < .b then .a else .b end`)

## Core filters

- [x] Length (`length`)
- [x] Type (`type`)
- [x] Stream consumers (`first`, `last`, `range`, `fold`)
- [x] Stream generators (`range`, `recurse`)
- [ ] More object filters (`to_entries`, `from_entries`, `with_entries`)
- [ ] More numeric filters (`sqrt`, `floor`, ...)
- [ ] More string filters (`explode`, `split`, `join`, ...)
- [ ] More array filters (`sort_by`, `group_by` ...)

## Standard filters

These filters are defined via more basic filters.
Their definitions are at [`std.jq`](jaq-core/src/std.jq).

- [x] Negation (`not`)
- [x] Filtering (`select(. >= 0)`
- [x] Iterable filters (`add`, `map(.+1)`, `map_values(.+1)`)
- [x] Array filters (`first`, `last`, `nth(10)`, `reverse`, `min`, `max`)
- [x] Universal/existential (`all`, `any`)

## Advanced features

jaq currently does *not* aim to support the advanced features of jq, such as:

- Variables
- Modules
- I/O
- Dates
- Regular expressions
- String interpolation
- try-catch
- SQL-style operators
- Streaming


# Differences between jq and jaq

## Numbers

jq uses 64-bit floating-point numbers (floats) for any number.
By contrast, jaq interprets
numbers such as 0   or -42 as 64-bit integers and
numbers such as 0.0 or 3e8 as 64-bit floats.
Many operations in jaq, such as array indexing,
check whether the passed numbers are indeed integer.
The motivation behind this is to avoid
rounding errors that may silently lead to wrong results.
For example:

    $ jq  -n '[0, 1, 2] | .[1.0000000000000001]'
    1
    $ jaq -n '[0, 1, 2] | .[1.0000000000000001]'
    Error: cannot use number (1) as (signed) integer
    $ jaq -n '[0, 1, 2] | .[1]'
    1

The rules of jaq are:

* The sum, difference, product, and remainder of two integers is integer.
* The quotient of two integers is integer if the remainder of the integers is zero.
* Any other operation between two numbers yields a float.

Examples:

    $ jaq -n '1 + 2'
    3
    $ jaq -n '10 / 2'
    5
    $ jaq -n '11 / 2'
    5.5
    $ jaq -n '1.0 + 2'
    3.0

## Reduce

jq has special syntax to fold over a sequence.
For example, to add a sequence of numbers, you may use:

    reduce .[] as $item (0; . + $item)

jaq does not have the `reduce ... as` syntax.
As substitute, it provides a `fold` filter, in which the above can be written as:

    fold(.[]; 0; .acc + .x)

Note that `fold` is likely less efficient than `reduce ... as`,
because it constructs a new object `{acc, x}` at every step.

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

To add a new builtin filter (such as `sort`), it suffices to:

1. Implement the filter in [the `functions` module](src/functions.rs).
2. Add a test with the filter name to [`tests.rs`](tests/tests.rs),
   and check whether jq yields the same results.
3. Add derived filters to [the standard library](jaq-core/src/std.jq).

Voilà!

Please make sure that after your change, `cargo test` runs successfully.
