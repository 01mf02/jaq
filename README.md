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


# How to install

To use jaq, you need a Rust toolchain.
See <https://rustup.rs/> for instructions.
(Note that Rust compilers shipped with Linux distributions
may be too outdated to compile jaq. I use Rust 1.48.)

The following commands will install jaq (on my system to `~/.cargo/bin/jaq`):

    git clone https://github.com/01mf02/jaq
    cd jaq
    cargo install --path .


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

Apply a function to all elements of an array and filter the results:

    $ echo '[0, 1, 2, 3]' | jaq 'map(.*2) | [.[] | select(. < 5)]'
    [0, 2, 4]

Read (slurp) input values into an array and get the average of its elements:

    $ echo '1 2 3 4' | jaq -s 'add / length'
    2.5

Repeatedly apply a filter to itself and output the intermediate results:

    $ echo '1' | jaq '[recurse(if . < 3 then .+1 else empty end)]'
    [1, 2, 3]


# Features

Here is an overview of the features
already implemented and not yet implemented.
Contributions to extend jaq are highly welcome, see below.

## Basic features

- [x] Basic data types (null, boolean, number, string, array, object)
- [x] Logical operators (`and`, `or`)
- [x] Equality and comparison operators (`.a == .b`, `.a < .b`)
- [x] Arithmetic operations on numbers (`+`, `-`, `*`, `/`, `%`)
- [ ] Arithmetic operations on non-numbers (e.g. strings, objects)

## Paths

- [x] Identity (`.`)
- [x] Indexing of arrays/objects (`.[0]`, `.a`, `.["a"]`)
- [x] Iterating over arrays/objects (`.[]`)
- [ ] Optional indexing/iteration (`.a?`, `.[]?`)
- [x] Array slices (`.[3:7]`, `.[0:-1]`)
- [ ] String slices

## Filter combinators

- [x] Composition (`|`)
- [x] Concatenation (`,`)
- [x] if-then-else (`if .a < .b then .a else .b end`)

## Functions

- [x] Negation (`not`)
- [x] Length (`length`)
- [x] Filtering (`select(. >= 0)`
- [x] Mapping (`map(.+1)`)
- [x] Universal/existential (`all`, `any`)
- [x] Summation (`add`)
- [x] Recursion (`recurse(.)`)
- [ ] More numeric functions (`sqrt`, `floor`, ...)
- [ ] More string functions (`explode`, `split`, `join`, ...)
- [ ] More array functions (`sort`, `min`, `unique`, `reverse`, ...)

## Assignment

- [ ] Plain assignment (`=`)
- [ ] Update assignment (`|=`)

## Advanced features

jaq currently does *not* aim to support the advanced features of jq, such as:

- Variables
- User-defined functions
- Modules
- I/O
- Dates
- Regular expressions
- String interpolation
- try-catch
- SQL-style operators
- Streaming


# Contributing

Contributions to jaq are welcome.
In particular, implementing various [functions](#functions) of jq in jaq
is a relatively low-hanging fruit.

To add a new function (such as `sort`), it suffices to:

1. Add a new constructor to [the `Function` enum](src/functions.rs).
2. Provide an implementation in `Function::run`.
3. Parse the function in [`Function::from`](src/parse.rs).
4. Add a test with the function name to [`tests.rs`](tests/tests.rs).

Voilà!

Please make sure that after your change, `cargo test` runs successfully.
