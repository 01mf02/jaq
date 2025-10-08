The jq language is a lazy, functional streaming programming language
originally designed by Stephen Dolan.
A program written in the jq language is called a jq program or _filter_.
The jq language is Turing-complete and can therefore be used to write
any program that can be written in any other programming language.

jq programs can be executed with several interpreters, including
`jq`, `gojq`, `fq`, and `jaq`.
jaq is designed to be usable as a drop-in replacement for the `jq` program,
which is the reference interpreter for the jq language written in C.
This manual tries to point out occasions where jaq diverges from `jq`.

Written in Rust, jaq focuses on correctness, high performance, and simplicity.
In addition, jaq adds some functionality not present in jq:

- Support for multiple file formats, including JSON, YAML, CBOR, TOML, XML;
  see [`--from`] and [`--to`]
- Support for invalid UTF-8 code units in JSON
- Byte strings; see [`tobytes`]
- Objects with non-string keys, such as `{0: 1, [2]: 3}`
- In-place replacement of input files; see [`--in-place`]

[`--from`]: #--from
[`--to`]: #--to
[`tobytes`]: #tobytes
[`--in-place`]: #--in-place
