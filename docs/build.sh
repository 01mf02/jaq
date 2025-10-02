#!/bin/sh
# use `man -l jaq.1` to read generated man page
FILES="intro.md cli.md corelang.md stdlib.md"
pandoc man-prologue.md $FILES man-epilogue.md --lua-filter filter.lua -s -o jaq.1
pandoc $FILES --lua-filter filter.lua -s -o MANUAL.html
pandoc $FILES --lua-filter tests.lua | cargo run -- --run-tests
