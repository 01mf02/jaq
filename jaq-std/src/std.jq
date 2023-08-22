def null:  [] | .[0];

def error(f): f | error;

# Booleans
def true:  0 == 0;
def false: 0 != 0;
def not: if . then false else true end;

# Not defined in jq!
def isboolean: . == true or . == false;
def isnumber:  . > true and . < "";
def isstring:  . >= ""  and . < [];
def isarray:   . >= []  and . < {};
def isobject:  . >= {};

# Numbers
def nan:      0 / 0;
def infinite: 1 / 0;
def isnan:      . == nan;
def isinfinite: . == infinite or  . == -infinite;
def isfinite:   isnumber and (isinfinite | not);
def isnormal:   isnumber and ((. == 0 or isnan or isinfinite) | not);

# Math
def logb:
    if . == 0.0 then -infinite
  elif isinfinite then infinite
  elif isnan then .
  else ilogb | . + 0.0 end;
def significand:
    if isinfinite or isnan then .
  elif . == 0.0 then 0.0
  else scalbln(.; ilogb | -1 * .) end;
def pow10:            pow(10.0; .);
def drem($l; r):      remainder($l; r) | if . == 0 then copysign(.; $l) end;
def nexttoward(x; y): nextafter(x; y);
def scalb(x; e):      x * pow(2.0; e);

# Type
def type:
    if . == null then "null"
  elif isboolean then "boolean"
  elif . < "" then "number"
  elif . < [] then "string"
  elif . < {} then "array"
  else             "object" end;

# Selection
def select(f): if f then . else empty end;
def values:    select(. != null);
def nulls:     select(. == null);
def booleans:  select(isboolean);
def numbers:   select(isnumber);
def finites:   select(isfinite);
def normals:   select(isnormal);
def strings:   select(isstring);
def arrays:    select(isarray);
def objects:   select(isobject);
def iterables: select(. >= []);
def scalars:   select(. <  []);

# Conversion
def tostring: if isstring then . else   tojson end;
def tonumber: if isnumber then . else fromjson end;

# Generators
def range(x): range(0; x);
def repeat(g): [g] | recurse(.) | .[];
def recurse: recurse(.[]?);
def recurse(f; cond): recurse(f | select(cond));
def while(cond; update): recurse_inner(if cond then update else empty end);
def until(cond; update): recurse_outer(if cond then empty else update end);

# Iterators
def map(f): [.[] | f];
def map_values(f): .[] |= f;
def add: reduce .[] as $x (null; . + $x);
def join(x): .[:-1][] += x | add;
def min: min_by(.);
def max: max_by(.);
def unique_by(f): [group_by(f)[] | .[0]];
def unique: unique_by(.);

def del(f): f |= empty;

# Arrays
def transpose: [range([.[] | length] | max) as $i | [.[][$i]]];
def first:  .[ 0];
def last:   .[-1];
def nth(n): .[ n];

def nth(n; g): last(limit(n + 1; g));

# Objects <-> Arrays
def keys: keys_unsorted | sort;
def   to_entries: [keys[] as $k | { key: $k, value: .[$k] }];
def from_entries: map({ (.key): .value }) | add + {};
def with_entries(f): to_entries | map(f) | from_entries;

# Paths
def paths: def rec: [(keys?)[] as $k | [$k], [$k] + (.[$k] | rec[])]; rec[];

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

# Walking
def walk(f): def rec: (.[]? |= rec) | f; rec;

def flatten: [recurse(arrays | .[]) | select(isarray | not)];
def flatten($d): if $d > 0 then map(if isarray then flatten($d-1) else [.] end) | add end;

# Regular expressions
def capture_of_match: map(select(.name) | { (.name): .string} ) | add + {};

def    test(re; flags): matches(re; flags) | length > 0;
def    scan(re; flags): matches(re; flags)[] | .[0].string;
def   match(re; flags): matches(re; flags)[] | .[0] + { captures: .[1:] };
def capture(re; flags): matches(re; flags)[] | capture_of_match;

def split (re; flags): split_(re; flags + "g");
def splits(re; flags): split(re; flags)[];

def sub(re; f; flags): split_matches(re; flags) |
  map(if isstring then . else capture_of_match | f end) | add;
def gsub(re; f; flags): sub(re; f; "g" + flags);

def    test(re):    test(re; "");
def    scan(re):    scan(re; "");
def   match(re):   match(re; "");
def capture(re): capture(re; "");
def  splits(re):  splits(re; "");
def  sub(re; f): sub(re; f;  "");
def gsub(re; f): sub(re; f; "g");

# I/O
def input: first(inputs);

# Date
def   todate:   todateiso8601;
def fromdate: fromdateiso8601;
