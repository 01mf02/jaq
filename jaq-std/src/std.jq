def null:  [] | .[0];
def empty: [] | .[ ];

def error(f): f | error;

# Booleans
def true:  0 == 0;
def false: 0 != 0;
def not: if . then false else true end;

# Numbers
def nan:      0. / 0.;
def infinite: 1. / 0.;
def isnan:      . == nan;
def isinfinite: . == infinite or -. == infinite;
def isfinite: isinfinite | not;
def isnormal: isnan or isinfinite | not;

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

# Conversion
def tostring: if type == "string" then . else   tojson end;
def tonumber: if type == "number" then . else fromjson end;

# Generators
def range(x): range(0; x);
def repeat(g): [g] | recurse(.) | .[];
def recurse: recurse(.[]?);
def recurse(f; cond): recurse(f | select(cond));

# Iterators
def map(f): [.[] | f];
def map_values(f): .[] |= f;
def add: reduce .[] as $x (null; . + $x);
def join(x): reduce .[] as $x (null; if . == null then $x else . + x + $x end);
def min: reduce .[] as $x (.[0]; if $x < . then $x else . end);
def max: reduce .[] as $x (.[0]; if $x > . then $x else . end);

# Arrays
def reverse: [.[length - 1 - range(length)]];
def transpose: [range([.[] | length] | max) as $i | [.[][$i]]];
def first:  .[ 0];
def last:   .[-1];
def nth(n): .[ n];

def nth(n; g): last(limit(n + 1; g));

# Objects <-> Arrays
def   to_entries: [keys as $k | { key: $k, value: .[$k] }];
def from_entries: map({ (.key): .value }) | add;
def with_entries(f): to_entries | map(f) | from_entries;

# Predicates
def isempty(g): first((g | false), true);
def all(g; cond): isempty(g | cond and empty);
def any(g; cond): isempty(g | cond  or empty) | not;
def all(cond): all(.[]; cond);
def any(cond): any(.[]; cond);
def all: all(.[]; .);
def any: any(.[]; .);
def in(xs)    : . as $x | xs | has     ($x);
def inside(xs): . as $x | xs | contains($x);
