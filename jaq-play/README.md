jaq playground
==============

First install `wasm-pack`:

    cargo install wasm-pack

Compile the WASM binaries (use `--release` instead of `--dev` for better performance):

    wasm-pack build --target web --dev

To serve:

    python3 -m http.server --bind 127.0.0.1
