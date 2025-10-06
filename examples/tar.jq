# TAR decoder, by Michael Färber
#
# To test this, you can create a 1GB TAR file with `examples/tar-1G.sh > 1G.tar`.
# Then run:
#
#     jaq -sR -L examples 'include "tar"; decode_tar | .o.name' 1G.tar
#
# This should print a list of files contained in the TAR.
# We can also easily verify that all files in the TAR have the same size
# (write the following instead of `decode_tar | .o.name` above):
#
#     all(decode_tar | .o.data; length == 1048576)
#
# For each parsed part of a TAR, such as the file name,
# this decoder stores *where* that data came from.
# That means that we can obtain e.g. the byte offset of each file header:
#
#     decode_tar | {name: .o.name, offset: .i.off}
#
# All these commands take about 280 milliseconds on my machine.
# For comparison, `tar --list --file 1G.tar` takes about 10 milliseconds.
#
#
# How to write a decoder?
# =======================
#
# Decoders take slices as input and output so-called I/O objects.
#
# A slice is an object `{str, off, len}` that
# represents the byte string `.str[.off:][:.len]`.
#
# An I/O object is either a byte string or an object {i, o}, where
# i is the consumed input and
# o is the produced output.
#
# Additionally, an I/O object {i, o} may contain a key "errs" that
# stores an array of objects {path, desc}, where
# path is an array to the source of the error, and
# desc is a description of the error (a string)
#
# A decoder that fails should add an error to errs (with empty path),
# then throw {i, errs} as error.
# This allows errors to be detected easily.

def slice: .str[.off:][:.len];
def slice_from ($i): .off += $i | .len -= $i;
def slice_until($i): .len = $i;

def has_errs: isobject and has("errs");
def catch_errs(f): try f catch (if has_errs then . else error end);

def assert_len($i; $w):
  ($i | slice | length) as $len |
  if $len != $w then
    .errs += [{desc: "expected \($w) bytes, found \($len)"}] | error
  end;

def update_output($o; $k; entry):
  ($o | has_errs) as $has_errs |
  if $has_errs then .errs += ($o.errs | (.[].path += [$k])) end |
  .o += entry |
  if $has_errs then error end;

# Take an I/O object, consume exactly $w bytes from its remaining input and
# add a field $k to its output with the consumed bytes fed to f.
#
# If f yields an error, this adds the error to the I/O object
# with the appropriate path, and rethrows the I/O object as error.
def take($k; $w; f):
  (.i | slice_until($w)) as $i | assert_len($i; $w) | .i |= slice_from($w) |
  catch_errs($i | f) as $o |
  update_output($o; $k; {($k): $o});

def rtrim_nul: index(0 | tobytes) // length as $i | .[:$i];

def parse_oct:
  . as $i |
  # the ASCII encoding of '0' is 48
  reduce (slice | rtrim_nul | tostring | trim | explode[] - 48) as $d (0;
    if $d < 0 or $d > 7 then
      {$i, errs: [{desc: "expected octal digit, found byte \($d + 48)"}]} | error
    end |
    . * 8 + $d
  ) |
  {$i, o: .};

def oct($k; $w): take($k; $w; parse_oct);
def str($k; $w): take($k; $w; slice | rtrim_nul);
def raw($k; $w): take($k; $w; slice);

def BLOCK_BYTES: 512;

def decode_entry:
  . as $input |
  # we use i as remaining input, which we later use to infer consumed input
  {i: .} |
  def consumed: .i.off - $input.off;
  def offset: (BLOCK_BYTES - (consumed % BLOCK_BYTES)) % BLOCK_BYTES;
  str("name"; 100) |
  oct("mode"; 8) |
  oct("uid" ; 8) |
  oct("gid" ; 8) |
  oct("size"; 12) |
  oct("mtime"; 12) |
  oct("chksum"; 8) | 
  str("typeflag"; 1) |
  str("linkname"; 100) |
  raw("header_block_padding"; offset) |
  raw("data"; .o.size.o) |
  raw("data_block_padding"; offset)
;

# Translate from i denoting remaining input to i denoting consumed input.
def set_consumed($input): .i |= {off, len: .off - $input.off};

def decode_tar: tobytes | {str: ., off: 0, len: length} |
  ([limit(BLOCK_BYTES*2; repeat(0))] | tobytes) as $END_MARKER |
  def rec: 
    . as $input |
    if slice | . == "" or startswith($END_MARKER)
    then empty # we're done
    else decode_entry | set_consumed($input), (.i | rec)
  end;
  # do not explode in the user's face
  catch_errs(rec)
;
