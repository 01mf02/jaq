`jaq` is an interpreter for the jq language originally designed by Stephen Dolan.
It is designed to be usable as a drop-in replacement for the `jq` program,
which is the reference interpreter for the jq language written in C.

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