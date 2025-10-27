jaq is an interpreter for the [jq programming language](#corelang).
It is designed to be usable as a drop-in replacement for the `jq` program,
which is the reference interpreter for the jq language written in C.

Written in Rust, jaq focuses on correctness, high performance, and simplicity.
In addition, jaq adds some functionality not present in `jq`:

- Support for multiple file [formats](#formats), including JSON, YAML, CBOR, TOML, XML
- Support for invalid UTF-8 code units in [text strings](#text-strings)
- [Byte strings](#byte-strings)
- [Objects](#objects) with non-string keys, such as `{0: 1, [2]: 3}`
- [In-place replacement of input files](#--in-place)

This manual tries to provide a full overview of
[jaq's command-line interface](#cli),
[jq's core language](#corelang), and
[jq's standard library](#stdlib).
It aims to cover the same concepts as the
[jq manual](https://jqlang.org/manual/).

::: Compatibility
This manual points out occasions where jaq diverges from `jq`.
:::

::: Advanced
For an even deeper understanding of jq semantics,
feel free to read my jq language specification at
<https://github.com/01mf02/jq-lang-spec/>.

Feel free to skip these "advanced" blocks if you do not seek enlightenment.
:::

