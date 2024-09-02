import "d" as d1 {search: "."};
import "d" as d2 {search: "../mods"};
import "d" as d3 {search: ["foo", "."]};
def c: "c" + d1::d + d2::d + d3::d;
