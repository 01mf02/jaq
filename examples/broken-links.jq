# Yield a list of references to non-existing IDs in an XHTML document.
#
# Example usage:
#
#     jaq --from xml -s -f examples/broken-links.jq docs/MANUAL.html

[.. | .a?.href // empty | select(startswith("#"))] as $hrefs |
[.. | .a?.id   // empty | "#" + .] as $ids |
$hrefs - $ids
