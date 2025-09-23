#!/bin/sh
# use `man -l jaq.1` to read generated man page
pandoc MANUAL.md --lua-filter filter.lua -s -o jaq.1
pandoc MANUAL.md --lua-filter filter.lua -s -o MANUAL.html
