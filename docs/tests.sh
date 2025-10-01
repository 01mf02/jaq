#!/bin/bash
pandoc MANUAL.md --lua-filter tests.lua | cargo run -- --run-tests
