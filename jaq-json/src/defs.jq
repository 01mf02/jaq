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
def paths(f): path_values | if .[1] | f then .[0] else empty end;
def getpath($path): reduce $path[] as $p (.; .[$p]);

# Indexing
def in(xs)    : . as $x | xs | has     ($x);
def inside(xs): . as $x | xs | contains($x);
def  index($i): indices($i)[ 0];
def rindex($i): indices($i)[-1];

# Formatting
def @json: tojson;

