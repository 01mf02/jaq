# Conversion
def tonumber: if isnumber then . else fromjson end;

# Arrays
def transpose: [range([.[] | length] | max) as $i | [.[][$i]]];

# Indexing
def in(xs)    : . as $x | xs | has     ($x);
def inside(xs): . as $x | xs | contains($x);
def  index($i): indices($i)[ 0];
def rindex($i): indices($i)[-1];

# Formatting
def @json: tojson;

