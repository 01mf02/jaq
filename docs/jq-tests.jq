# remove comments
.jq |= gsub("#[^\n]*"; "") |
# remove newlines (from both filter and output)
.[ ] |= (gsub("\n"; "") | trim) |
.jq, "null", .xjon, ""
