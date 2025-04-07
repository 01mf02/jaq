# Conversion
def tonumber: if isnumber then . else fromjson end;

# Arrays
def transpose: [range([.[] | length] | max) as $i | [.[][$i]]];

# Objects <-> Arrays
def keys: keys_unsorted | sort;
def   to_entries: [keys_unsorted[] as $k | { key: $k, value: .[$k] }];
def from_entries: map({ (.key): .value }) | add + {};
def with_entries(f): to_entries | map(f) | from_entries;

# Paths
def paths   : path_values[0];
def paths(f): path_values | if .[0] | f then .[1] else empty end;
def getpath($p): if $p != [] then .[$p[0]] | getpath($p[1:]) end;

# Indexing
def in(xs)    : . as $x | xs | has     ($x);
def inside(xs): . as $x | xs | contains($x);
def  index($i): indices($i)[ 0];
def rindex($i): indices($i)[-1];

# Formatting
def @json: tojson;

