# Extract CLI tests from the manual.

.. | select(.t? == "code") | .c[] |
select(startswith("$ ")) |
@htmld
