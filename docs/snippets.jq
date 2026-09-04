# Extract code snippets from the manual.

# get contents of all code tags without attributes
.. | select(.t? == "code") | select(has("a") | not) | .c[] |
@htmld |
split("-->") |
select(length > 1) |
{jq: .[0], xjon: .[1]}
