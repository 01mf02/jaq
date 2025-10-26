This directory contains the source code of the jaq manual.

To generate HTML and man versions of the manual, run `make -j`.
This requires:

- `jotdown` for tests and the HTML version, and
- `pandoc` for the man page

You can also selectively build parts of the manual via:

- `make MANUAL.xhtml`
- `make jaq.1`

To install `jotdown`:

```
cargo install jotdown@0.8.1 --locked
```

This was tested with Pandoc 3.4.
