def null:  [][0];

def stderr:      (       stderr_empty  as $x | .), .;
def debug:       (        debug_empty  as $x | .), .;
def debug(msgs): ((msgs | debug_empty) as $x | .), .;

def halt: halt(0);
def halt_error($exit_code): stderr_empty, halt($exit_code);
def halt_error: halt_error(5);

# Not defined in jq!
def isboolean: . == true or . == false;
def isnumber:  . > true and . < "";
def isstring:  . >= ""  and . < [];
def isarray:   . >= []  and . < {};
def isobject:  . >= {};

# Numbers
def nan:      0 / 0;
def infinite: 1 / 0;
def isnan:      . < nan and nan < .;
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
def gamma: tgamma;

# Type
def type:
    if . == null then "null"
  elif isboolean then "boolean"
  elif . < "" then "number"
  elif . < [] then "string"
  elif . < {} then "array"
  else             "object" end;

# Selection
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

# Iterators
def add(f): reduce f as $x (null; . + $x);
def add: add(.[]);

# Arrays
def min_by(f): reduce min_by_or_empty(f) as $x (null; $x);
def max_by(f): reduce max_by_or_empty(f) as $x (null; $x);
def min: min_by(.);
def max: max_by(.);
def unique_by(f): [group_by(f)[] | .[0]];
def unique: unique_by(.);

# Paths
def pick(f):
  reduce path_value(f) as [$path, $value] ({}; . *
    reduce ($path | reverse[]) as $p ($value; {($p): .})
  );

def keys: keys_unsorted | sort;

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
def @htmld  : tostring | unescape_html;
def @uri    : tostring | encode_uri;
def @urid   : tostring | decode_uri;
def @base64 : tostring | encode_base64;
def @base64d: tostring | decode_base64;
