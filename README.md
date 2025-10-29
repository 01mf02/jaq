# jaq

![Build status](https://github.com/01mf02/jaq/actions/workflows/check.yml/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/jaq-core.svg)](https://crates.io/crates/jaq-core)
[![Documentation](https://docs.rs/jaq-core/badge.svg)](https://docs.rs/jaq-core)
[![Rust 1.69+](https://img.shields.io/badge/rust-1.69+-orange.svg)](https://www.rust-lang.org)

jaq (pronounced /ʒaːk/, like *Jacques*[^jacques]) is a clone of the JSON data processing tool [jq].
jaq aims to support a large subset of jq's syntax and operations.

jaq is two things at a time:

- A command-line program, `jaq`, that can be used as drop-in replacement for `jq`.
- A library, [`jaq-core`](https://docs.rs/jaq-core/),
  that can be used to compile and run jq programs inside of Rust programs.
  Compared to the `jq` API, `jaq-core`
  is thoroughly documented,
  supports [arbitrary data types beyond JSON](https://docs.rs/jaq-core/latest/jaq_core/val/trait.ValT.html), and
  can be safely used in multi-threaded environments.

jaq has an own [manual](https://gedenkt.at/jaq/manual/).
You can try jaq online on the [jaq playground](https://gedenkt.at/jaq/).
Instructions for the playground can be found [here](jaq-play/).

jaq focuses on three goals:

* **Correctness**:
  jaq aims to provide a more correct and predictable implementation of jq,
  while preserving compatibility with jq in most cases.
* **Performance**:
  I created jaq originally because I was bothered by
  [the long start-up time of jq 1.6](https://github.com/jqlang/jq/issues/1411),
  which amounts to about 50ms on my machine.
  This can be particularly seen when processing a large number of small files.
  Although the startup time has been vastly improved in jq 1.7,
  jaq is still faster than jq on many other [benchmarks](#performance).
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
  And finally, the
  [Jacquard machine](https://en.wikipedia.org/wiki/Jacquard_machine)
  was an important predecessor of the modern computer,
  automating weaving with punched cards as early as 1804.



# Installation


## Binaries

You can download binaries for Linux, Mac, and Windows on the [releases page](https://github.com/01mf02/jaq/releases).
On a Linux system, you can download it using the following commands:

    $ curl -fsSL https://github.com/01mf02/jaq/releases/latest/download/jaq-$(uname -m)-unknown-linux-musl -o jaq && chmod +x jaq

You may also install jaq using [homebrew](https://formulae.brew.sh/formula/jaq) on macOS or Linux:

    $ brew install jaq
    $ brew install --HEAD jaq # latest development version

[![Packaging status](https://repology.org/badge/vertical-allrepos/jaq.svg)](https://repology.org/project/jaq/versions)


## From Source

To compile jaq, you need a Rust toolchain.
See <https://rustup.rs/> for instructions.

Any of the following commands install jaq:

    $ cargo install --locked jaq
    $ cargo install --locked --git https://github.com/01mf02/jaq # latest development version

On my system, both commands place the executable at `~/.cargo/bin/jaq`.

If you have cloned this repository, you can also build jaq by executing one of the commands in the cloned repository:

    $ cargo build --release # places binary into target/release/jaq
    $ cargo install --locked --path jaq # installs binary

jaq should work on any system supported by Rust.
If it does not, please file an issue.



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
`bench.sh target/release/jaq jq-1.8.1 gojq-0.12.17 | tee bench.json`
on a Linux system with an AMD Ryzen 5 5500U.[^binaries]
I then processed the results with a "one-liner" (stretching the term and the line a bit):

    jq -rs '.[] | "|`\(.name)`|\(.n)|" + ([.time[] | min | (.*1000|round)? // "N/A"] | min as $total_min | map(if . == $total_min then "**\(.)**" else "\(.)" end) | join("|"))' bench.json

(Of course, you can also use jaq here instead of jq.)
Finally, I concatenated the table header with the output and piped it through `pandoc -t gfm`.

[^binaries]: The binaries for jq-1.8.1 and gojq-0.12.17 were retrieved from their GitHub release pages.

Table: Evaluation results in milliseconds ("N/A" if error or more than 10 seconds).

| Benchmark       |       n | jaq-2.3 | jq-1.8.1 | gojq-0.12.17 |
|-----------------|--------:|--------:|---------:|-------------:|
| `empty`         |     512 |     330 |      440 |      **290** |
| `bf-fib`        |      13 | **430** |     1110 |          540 |
| `defs`          |  100000 |  **60** |      N/A |         1000 |
| `upto`          |    8192 |   **0** |      470 |          450 |
| `reduce-update` |   16384 |  **10** |      490 |         1200 |
| `reverse`       | 1048576 |  **30** |      500 |          270 |
| `sort`          | 1048576 | **100** |      450 |          540 |
| `group-by`      | 1048576 | **340** |     1750 |         1540 |
| `min-max`       | 1048576 |     190 |  **170** |          260 |
| `add`           | 1048576 | **440** |      570 |         1150 |
| `kv`            |  131072 | **100** |      140 |          270 |
| `kv-update`     |  131072 | **110** |      480 |          480 |
| `kv-entries`    |  131072 | **520** |     1050 |          800 |
| `ex-implode`    | 1048576 | **470** |     1010 |          590 |
| `reduce`        | 1048576 | **720** |      850 |          N/A |
| `try-catch`     | 1048576 | **170** |      220 |          370 |
| `repeat`        | 1048576 | **140** |      690 |          530 |
| `from`          | 1048576 | **280** |      800 |          550 |
| `last`          | 1048576 |  **40** |      160 |          110 |
| `pyramid`       |  524288 |     310 |  **270** |          480 |
| `tree-contains` |      23 |  **60** |      590 |          220 |
| `tree-flatten`  |      17 |     750 |      340 |        **0** |
| `tree-update`   |      17 | **470** |      970 |         1300 |
| `tree-paths`    |      17 | **130** |      250 |          770 |
| `to-fromjson`   |   65536 |  **40** |      370 |          100 |
| `ack`           |       7 | **510** |      540 |         1090 |
| `range-prop`    |     128 |     350 |      270 |      **230** |
| `cumsum`        | 1048576 | **250** |      260 |          460 |
| `cumsum-xy`     | 1048576 |     400 |  **350** |          680 |

The results show that
jaq-2.3 is fastest on 23 benchmarks, whereas
jq-1.8.1 is fastest on 3 benchmark and
gojq-0.12.17 is fastest on 3 benchmarks.
gojq is much faster on `tree-flatten` because it implements the filter `flatten` natively instead of by definition.

[gojq]: https://github.com/itchyny/gojq


# Security

jaq's core has been audited by
[Radically Open Security](https://www.radicallyopensecurity.com/)
as part of an [NLnet](https://nlnet.nl/) grant ---
thanks to both organisations for their support!
The [security audit](https://github.com/01mf02/jaq/releases/download/v2.2.0/jaq.penetration.test.report.2025.1.0.pdf) found
one low severity issue and three issues that are likely not exploitable at all.
As a result of this security audit, all issues were addressed and
several fuzzing targets for jaq were added at `jaq-core/fuzz`.
Before that, jaq's JSON parser [hifijson](https://github.com/01mf02/hifijson/)
already disposed of a fuzzing target.
Finally, jaq disposes of a carefully crafted test suite of more than 500 tests
that is checked at every commit.



# User Testimonials

> `jaq` is a well-built library that gave me a massive leg up compared to implementing `jq` support on my own. Extensibility through the `ValT` trait made adding `jq` support to my own types a breeze.
>
> @jobarr-amzn (<https://github.com/amazon-ion/ion-cli/pull/193#pullrequestreview-2696367084>, <https://github.com/01mf02/jaq/issues/355#issuecomment-3457076847>)

> My Rust program \[using jaq\] can execute *all queries* over *all files* **three times** while Python is busy executing *one* query across all files using the `jq` PyPI crate and a Python loop.
>
> @I-Al-Istannen (<https://github.com/01mf02/jaq/issues/323#issuecomment-3282176968>)

> jaq is very impressive! Running my [wsjq](https://github.com/thaliaarchi/wsjq) interpreter with it is significantly faster than with any other jq implementation and its emphasis on correctness is very admirable.
> \[On [wsjq benchmarks](https://github.com/01mf02/jaq/issues/294#issuecomment-3046707389), jaq is between 5 and 10 times faster than jq and between 15 and 196 times faster than gojq.\]
>
> @thaliaarchi (<https://github.com/01mf02/jaq/issues/355#issuecomment-3460028137>)

> I had been parsing data from certificate transparency logs using certstream-server. It gives a *lot* of data and piping it into jq was causing me issues. I switched to jaq and the faster startup time meant it could easily keep up on the low end VM I was using. Thank you for your work.
>
> Oliver (via e-mail)

Add your own testimonials via <https://github.com/01mf02/jaq/issues/355>.



# Contributing

Contributions to jaq are welcome.
Please make sure that after your change, `cargo test` runs successfully.



# Acknowledgements

[This project](https://nlnet.nl/project/jaq/) was funded through the
<a href="https://nlnet.nl/entrust">NGI0 Entrust</a> Fund, a fund established by
<a href="https://nlnet.nl">NLnet</a> with financial support from the
European Commission's <a href="https://ngi.eu">Next Generation Internet</a>
programme, under the aegis of <a href="https://commission.europa.eu/about-european-commission/departments-and-executive-agencies/communications-networks-content-and-technology_en">DG Communications Networks, Content and Technology</a> under grant agreement N<sup>o</sup> 101069594.
