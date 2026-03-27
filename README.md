# jaq

![Build status](https://github.com/01mf02/jaq/actions/workflows/check.yml/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/jaq-core.svg)](https://crates.io/crates/jaq-core)
[![Documentation](https://docs.rs/jaq-core/badge.svg)](https://docs.rs/jaq-core)
[![Rust 1.69+](https://img.shields.io/badge/rust-1.69+-orange.svg)](https://www.rust-lang.org)

jaq (pronounced /ʒaːk/, like *Jacques*[^jacques]) is a clone of
the JSON data processing tool [`jq`](https://jqlang.github.io/jq/).
It has a few features not present in `jq`, such as
support for the data formats YAML, CBOR, TOML, and XML.
jaq has an own [manual](https://gedenkt.at/jaq/manual/).
You can try jaq on the [playground](https://gedenkt.at/jaq/).

jaq is two things at a time:

- A command-line program, `jaq`, that can be used as drop-in replacement for `jq`.
- A library, [`jaq-core`](https://docs.rs/jaq-core/),
  that can be used to compile and run jq programs inside of Rust programs.
  Compared to the `jq` API, `jaq-core`
  can be safely used in multi-threaded environments and
  supports [arbitrary data types beyond JSON](https://docs.rs/jaq-core/latest/jaq_core/val/trait.ValT.html).

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

[^binaries]: jq-1.8.1 was installed from the official Arch Linux package and gojq-0.12.18 was retrieved from its GitHub release page.

Table: Evaluation results in milliseconds ("N/A" if error or more than 10 seconds).

| Benchmark       |       n | jaq-3.0 | jq-1.8.1 | gojq-0.12.18 |
|-----------------|--------:|--------:|---------:|-------------:|
| `empty`         |     512 |     410 |      430 |      **270** |
| `bf-fib`        |      13 | **530** |     1030 |      **530** |
| `defs`          |  100000 |  **50** |      N/A |          960 |
| `upto`          |    8192 |   **0** |      440 |          450 |
| `reduce-update` |   16384 |   **0** |      720 |         1320 |
| `reverse`       | 1048576 |  **30** |      440 |          300 |
| `sort`          | 1048576 |  **90** |      430 |          550 |
| `group-by`      | 1048576 | **260** |     1790 |         1580 |
| `min-max`       | 1048576 | **180** |      200 |          300 |
| `add`           | 1048576 | **390** |      520 |         1290 |
| `kv`            |  131072 | **110** |      120 |          280 |
| `kv-update`     |  131072 | **130** |      440 |          550 |
| `kv-entries`    |  131072 | **520** |      980 |          870 |
| `ex-implode`    | 1048576 |     600 |      890 |      **560** |
| `reduce`        | 1048576 |     740 |  **700** |          N/A |
| `try-catch`     | 1048576 |     220 |  **200** |          390 |
| `repeat`        | 1048576 | **170** |      610 |          500 |
| `from`          | 1048576 | **340** |      730 |          550 |
| `last`          | 1048576 |  **20** |      150 |          170 |
| `pyramid`       |  524288 |     300 |  **250** |          470 |
| `tree-contains` |      23 |  **90** |      830 |          230 |
| `tree-flatten`  |      17 |     700 |      330 |       **10** |
| `tree-update`   |      17 | **430** |      910 |         1830 |
| `tree-paths`    |      17 | **140** |      230 |          780 |
| `to-fromjson`   |   65536 |  **50** |      350 |          100 |
| `ack`           |       7 |     570 |  **490** |          540 |
| `range-prop`    |     128 |     370 |      250 |      **210** |
| `cumsum`        | 1048576 | **240** |      250 |          510 |
| `cumsum-xy`     | 1048576 |     380 |  **350** |          750 |
| `str-slice`     |    8192 |     170 |      650 |      **120** |

The results show that
jaq-3.0 is fastest on 20 benchmarks, whereas
jq-1.8.1 is fastest on 5 benchmarks and
gojq-0.12.18 is fastest on 6 benchmarks.
gojq is much faster on `tree-flatten` because it implements the filter `flatten` natively instead of by definition.

[gojq]: https://github.com/itchyny/gojq


# Security

jaq tries to guarantee that:

- It does not panic (except in cases of resource exhaustion, see below).
- It does not corrupt memory, i.e. it is memory-safe.
- It does not allow input data and jq filters to initiate I/O operations
  (except for reading files by jq filters before filter execution).

Any case where such a guarantee is broken is a bug and should be reported.

On the other hand,
jaq does not take countermeasures against any kind of resource exhaustion.
That means that jaq may take unlimited time, memory, or stack space.
For example, this may lead to stack overflows when:

- Reading input data: `jaq -nr 'repeat("[")' | jaq`
- Running jq filters: `jaq -n 'def f: 1+f; f'`

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



# Acknowledgements

[This project](https://nlnet.nl/project/jaq/) was funded through the
[NGI0 Entrust](https://nlnet.nl/entrust) and
[NGI0 Commons](https://nlnet.nl/commonsfund) funds established by
[NLnet](https://nlnet.nl) with financial support from the
European Commission's [Next Generation Internet](https://ngi.eu)
programme, under the aegis of [DG Communications Networks, Content and Technology](https://commission.europa.eu/about-european-commission/departments-and-executive-agencies/communications-networks-content-and-technology_en) under
grant agreements
№ [101069594](https://cordis.europa.eu/project/id/101069594) and
№ [101135429](https://cordis.europa.eu/project/id/101135429).
Additional funding is made available by the
[Swiss State Secretariat for Education, Research and Innovation](https://www.sbfi.admin.ch/sbfi/en/home.html) (SERI).
