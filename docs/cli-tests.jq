# Extract CLI tests from the manual.

# get contents of all code tags without attributes
.. | select(.t? == "code") | select(has("a") | not) | .c[] |
@htmld |
select(startswith("$ "))
