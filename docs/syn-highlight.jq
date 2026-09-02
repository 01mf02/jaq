def rec:
    if isobject then {t: "span", a: {"class": .t}, c: .c | rec}
  elif isarray then .[] |= rec
  elif isstring then @html end;
$manual | fromxml |
(.. | select(.t? == "code" and .a.lang == "jq") | .c[]) |= (first(inputs) | rec)
