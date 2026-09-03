# remove comments
.filter |= gsub("#[^\n]*"; "") |
# remove newlines (from both filter and output)
.[ ] |= (gsub("\n"; "") | trim) |
.filter, "null", .output, ""
