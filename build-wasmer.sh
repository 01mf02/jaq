#!/bin/bash

# You'll need to have Cargo WASIX installed (`cargo install cargo-wasix`)
cargo wasix build --release --no-default-features

# Then, for publishing
# wasmer publish
