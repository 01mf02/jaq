This directory contains the source code of the jaq manual.
The manual is written using the [Djot](https://djot.net/) file format,
which is a unambiguously defined markup syntax that
allows for much faster parsing than Markdown.

This requires `jotdown`. To install it:

```
cargo install jotdown@0.8.1 --locked
```

To generate HTML and man versions of the manual, run `make -j`.
You can also selectively build parts of the manual via:

- `make MANUAL.xhtml`
- `make jaq.1`
