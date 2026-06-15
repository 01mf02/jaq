def esc_str:
  gsub("\\\\"; "\\[rs]") |
  gsub("'"   ; "\\[aq]") |
  gsub("\""  ; "\\[dq]") |
  gsub("&lt;" ; "<") |
  gsub("&gt;" ; ">") |
  gsub("&amp;"; "&") |
  gsub("\\."; "\\&.") |
  gsub(  "-"; "\\-" );

def conv_link:
  if .a.href | startswith("#") then .c[]
  else "\\c\n.UR \(.a.href)\n", .c[], "\n.UE \\c\n"
  end;

def rec_tags: .. | select(isobject and has("t"));

rec_tags |= (.c[] | select(isstring)) |= esc_str |
rec_tags |= if .t == "pre" then ".IP\n", ".EX\n", .c[].c[], ".EE\n" end |
rec_tags |=
    if .t == "section" then .c[]
  elif .t == "h1" then ".SH " , (.c[] | ascii_upcase), "\n"
  elif .t == "h2" then ".SS " , (.c[] | ascii_upcase), "\n"
  elif .t == "h3" then ".SS " , .c[], "\n"
  elif .t == "p"  then ".PP\n", .c[], "\n"
  elif .t == "a" then conv_link
  elif .t == "ul" or .t == "ol" then ".RS 2\n", .c[], ".RE"
  elif .t == "li" then ".IP \\[bu] 2", .c[]
  elif .t == "em"     then "\\f[I]" , .c[], "\\f[R]"
  elif .t == "strong" then "\\f[B]" , .c[], "\\f[R]"
  elif .t == "code"   then "\\f[CB]", .c[], "\\f[R]"
  elif .t == "div" and (.a.class | . == "Compatibility" or . == "Advanced") then empty
  elif .t == "blockquote" then ".RS\n", .c[], ".RE\n"
  elif .t == "header" then empty
  # TODO: find solution for @html description
  elif .t == "table"  then empty
  end
|
.c[]
