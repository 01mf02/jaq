# 99 Bottles of Beer, by Michael Färber.
# Run it with: `jaq -nr -f bottles.jq` (jq yields the same output)
def bottles:
  if . == 0 then "no more" else tostring end + " bottle" +
  if . == 1 then "" else "s" end;
range(99; 0; -1) |
  "\(bottles) of beer on the wall, \(bottles) of beer.",
  "Take one down & pass it around, now there's \(. - 1 | bottles) on the wall!"
