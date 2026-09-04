import "hl-jq"   as $hl_jq;
import "hl-xjon" as $hl_xjon;

foreach (inputs | to_entries[]) as {key: $lang, value: $code} (
  {queues: {jq: $hl_jq, xjon: $hl_xjon}};
  .output = {$lang, $code, ast: .queues[$lang][0]} |
  .queues[$lang] |= .[1:];
  .output
)
