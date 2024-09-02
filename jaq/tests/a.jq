include "b" {search: "."};
import "c" as c {search: "mods"};
import "data" as $data {search: "."};
def a: b + c::c;
def data: $data;
