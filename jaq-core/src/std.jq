# Selection
def select(f): if f then . else empty end;
def values:    select(. != null);
def nulls:     select(. == null);
def booleans:  select(type == "boolean");
def numbers:   select(type == "number");
def strings:   select(type == "string");
def arrays:    select(type == "array");
def objects:   select(type == "object");
def iterables: select(type | . == "array" or  . == "object");
def scalars:   select(type | . != "array" and . != "object");

# Generators
def range(x): range(0; x);
def recurse(f; cond): recurse(f | select(cond));

# Iterators
def map(f): [.[] | f];
def map_values(f): .[] |= f;

# List functions
def min: fold(.[]; .[0]; if .x < .acc then .x else .acc end);
def max: fold(.[]; .[0]; if .x > .acc then .x else .acc end);
def reverse: [.[length - 1 - range(length)]];
def transpose: [{ i: range([.[] | length] | max), a: . } | [.a[][.i]]];
def first:  .[ 0];
def last:   .[-1];
def nth(n): .[ n];

def nth(n; g): last(limit(n + 1; g));

# Predicates
def isempty(g): first((g | false), true);
def all(g; cond): isempty(g | cond and empty);
def any(g; cond): isempty(g | cond  or empty) | not;
def all(cond): all(.[]; cond);
def any(cond): any(.[]; cond);
def all: all(.[]; .);
def any: any(.[]; .);
