# Fuzz targets for jaq

This directory contains fuzz targets laid out for easy use with cargo-fuzz.
These fuzz targets fit the libFuzzer harness interface and
can therefore be utilized by a variety of fuzzing engines if desired.

## Quick start to fuzzing

1. `rustup toolchain install nightly`
2. `cargo +nightly install cargo-fuzz`
3. `cargo +nightly fuzz run load_and_compile`

(Instead of `cargo +nightly`, you can also use `RUSTC_BOOTSTRAP=1 cargo`.
That way, you also do not have to install a nightly toolchain.)

Congratulations, you are now fuzzing jaq's core with libFuzzer!

This fuzzing process will keep going until it
hits a bug, runs out of memory, or the process is terminated.
The challenge is now to figure out ways to
get to bugs quicker and define more buggy conditions for the fuzzer to find.
In the `corpus` directory you will find
interesting inputs the fuzzer has generated, and saved as useful,
because they reach a new part of the program.
You can add your own inputs here (e.g. valid jaq programs) as a starting corpus
to bootstrap your fuzzing efforts deeper in jaq's core.


## Targets

### load_and_compile

This target loads and compiles fuzzer generated jaq programs,
but does not execute the programs.
The target is interested in
whether the program compiles, not whether it is correct when it runs.
The target becomes slower as it gets deeper into jaq.
This has been productive for finding shallower bugs.

To get an interesting initial input corpus, run:

    jaq-core/fuzz/init_corpus.sh

### parse_tokens

This target bypasses the lexer by directly creating
tokens to be parsed as terms or definitions.
Skipping the lexer makes this target relatively fast.
