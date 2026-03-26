# Convert between rows/tables and objects/arrays of objects.
#
# A row is an array of values and a table is an array of rows.
# The first row of a table is usually a *header*, containing field names.
# 
# Properties:
# - [from_table] | to_table === .
# - $keys | from_row(rows) | to_row($keys) === rows
#
# Examples:
# - ["first", "last"] as $keys | $keys | from_row([1, 2]) | to_row($keys) --> [1, 2]
# - [["first", "last"], [1, 2]] | [from_table] | to_table --> ["first", "last"], [1, 2]

# Take a header as input and a row as argument, yield an object.
def from_row($row): to_entries | .[] |= {(.value): $row[.key]} | add;
# Take a table as input, yield objects.
def from_table: .[1:][] as $row | .[0] | from_row($row);

# Take an object as input and a header as argument, yield a row.
def to_row($keys): [.[$keys[]]];
# Take an array of objects as input and a header as argument, yield a table.
def to_table($keys): $keys, (.[] | to_row($keys));
# Take an array of objects as input, yield a table.
def to_table: to_table(.[0] | keys_unsorted);

# Take a table as input and a function from array of objects to array of objects as argument. Yield table, including header.
def with_table(f): [from_table] | f | to_table;
