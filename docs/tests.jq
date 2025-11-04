# Extract documentation tests from the manual.

# get contents of all code tags without attributes
.. | select(.t? == "code") | select(has("a") | not) | .c[] |
@htmld |
split("-->") |
select(length > 1) |
# remove comments
.[0] |= gsub("#[^\n]*"; "") |
# remove newlines
.[ ] |= (gsub("\n"; "") | trim) |
.[0], "null", .[1], ""
