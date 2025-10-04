#!/bin/sh
# use `man -l jaq.1` to read generated man page
FILES="intro.md cli.md corelang.md stdlib.md"
pandoc $FILES --to filter.lua | cargo run -- --run-tests
pandoc $FILES --lua-filter filter.lua -s -o MANUAL.html
pandoc man-prologue.md $FILES man-epilogue.md --lua-filter filter.lua -s -o jaq.1
