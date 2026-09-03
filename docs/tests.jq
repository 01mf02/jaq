# Extract documentation tests from the manual.

# get contents of all code tags without attributes
.. | select(.t? == "code") | select(has("a") | not) | .c[] |
@htmld |
split("-->") |
select(length > 1) |
{filter: .[0], output: .[1]}
