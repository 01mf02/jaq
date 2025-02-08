---
title: JAQ(1)
---

# Name

jaq --- Command-line JSON processor

# Synopsis

**`jaq`** `[`_OPTION_`]`... `[`_FILTER_`]` `[`_FILE_`]`...

# Description

# Basic filters

## Concatenation

`f, g`: Return the concatenation of the outputs of `f` and `g`.

::: Examples

~~~
.a, length
{"a": 0, "b": 1}
0
2
~~~

:::

## Composition

`f | g`: For each output of `f`, apply the output to g and return all its outputs.

# Author

Michael Färber

# See also
