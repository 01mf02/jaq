# Extract code snippets from the manual.

# get contents of all code tags without attributes

def jq: select(has("a") | not) | .c[] |
@htmld |
split("-->") |
select(length > 1) |
{jq: .[0], xjon: .[1]};

def shell: .c[] |
select(startswith("$ ")) |
@htmld |
{shell: .};

.. | select(.t? == "code") | jq // shell
