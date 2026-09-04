import "hl-jq"    as $jq;
import "hl-xjon"  as $xjon;
import "hl-shell" as $shell;

foreach (inputs | to_entries[]) as {key: $lang, value: $code} (
  {queues: {$jq, $xjon, $shell}};
  .output = {$lang, $code, ast: .queues[$lang][0]} |
  .queues[$lang] |= .[1:];
  .output
)
