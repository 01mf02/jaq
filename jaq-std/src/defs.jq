def empty: {}[] as $x | .;
def null:  [][0];

def debug(msgs): ((msgs | debug) as $x | empty), .;
def error(msgs): ((msgs | error) as $x | empty), .;

def halt: halt_error(5);

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
def abs: if . < 0 then - . end;
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
def tostring: "\(.)";

# Generators
def range(from; to): range(from; to; 1);
def range(to): range(0; to);
def repeat(f): def rec: f, rec; rec;
def recurse(f): def rec: ., (f | rec); rec;
def recurse: recurse(.[]?);
def recurse(f; cond): recurse(f | select(cond));
def while(cond; update): def rec: if cond then ., (update | rec) else empty end; rec;
def until(cond; update): def rec: if cond then . else update | rec end; rec;

# Iterators
def map(f): [.[] | f];
def map_values(f): .[] |= f;
def add(f): reduce f as $x (null; . + $x);
def add: add(.[]);
def join(x): .[:-1][] += x | add;
def min_by(f): reduce min_by_or_empty(f) as $x (null; $x);
def max_by(f): reduce max_by_or_empty(f) as $x (null; $x);
def min: min_by(.);
def max: max_by(.);
def unique_by(f): [group_by(f)[] | .[0]];
def unique: unique_by(.);

def del(f): f |= empty;

# Arrays
def first:  .[ 0];
def last:   .[-1];
def nth(n): .[ n];

def last(g): (reduce g as $item ([]; [$item]))[];
def nth(n; g): last(limit(n + 1; g));

# Predicates
def isempty(g): first((g | false), true);
def all(g; cond): isempty(g | cond and empty);
def any(g; cond): isempty(g | cond  or empty) | not;
def all(cond): all(.[]; cond);
def any(cond): any(.[]; cond);
def all: all(.[]; .);
def any: any(.[]; .);

# Walking
def walk(f): def rec: (.[]? |= rec) | f; rec;

def flatten: [recurse(arrays[]) | select(isarray | not)];
def flatten($d): if $d > 0 then map(if isarray then flatten($d-1) else [.] end) | add end;

# Regular expressions
def capture_of_match: map(select(.name) | { (.name): .string} ) | add + {};

def    test(re; flags): matches(re; flags) | any;
def    scan(re; flags): matches(re; flags)[] | .[0].string;
def   match(re; flags): matches(re; flags)[] | .[0] + { captures: .[1:] };
def capture(re; flags): matches(re; flags)[] | capture_of_match;

def split($sep):
  if isstring and ($sep | isstring) then . / $sep
  else error("split input and separator must be strings") end;
def split (re; flags): split_(re; flags + "g");
def splits(re; flags): split(re; flags)[];

def sub(re; f; flags):
  def handle: if isarray then capture_of_match | f end;
  reduce split_matches(re; flags)[] as $x (""; . + ($x | handle));

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

# Formatting
def fmt_row(n; s): if . >= "" then s elif . == null then n else "\(.)" end;
def @csv: .[] |= fmt_row(""; "\"\(escape_csv)\"") | join("," );
def @tsv: .[] |= fmt_row("";      escape_tsv    ) | join("\t");
def @sh: [if isarray then .[] end | fmt_row("null"; "'\(escape_sh)'")] | join(" ");
def @text: "\(.)";
def @html   : tostring | escape_html;
def @uri    : tostring | encode_uri;
def @base64 : tostring | encode_base64;
def @base64d: tostring | decode_base64;
