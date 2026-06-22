# Convert HTML to man page.

def esc_str:
  gsub("\\\\"  ; "\\[rs]") |
  gsub("'"     ; "\\[aq]") |
  gsub("\""    ; "\\[dq]") |
  gsub("\\."   ; "\\&.") |
  gsub(  "-"   ; "\\-" ) |
  gsub("&nbsp;"; "\\ " ) |
  @htmld;

def conv_link:
  if .a.href | startswith("#") then .c[]
  else "\\c\n.UR \(.a.href)\n", .c[], "\n.UE \\c\n"
  end;

def rec_tags: .. | select(isobject and has("t"));

rec_tags |= if .t == "div" and .a.class == "Advanced" or .t == "header" then empty end |
rec_tags |= (.c[] | select(isstring)) |= esc_str |
rec_tags |= if .t == "code" then .c[] |= sub("^ "; "\\ ") end |
rec_tags |= if .t == "pre" then ".IP\n", ".EX\n", .c[].c[], ".EE\n" end |
rec_tags |=
    if .t == "p"  then ".PP\n", .c[], "\n"
  elif .t == "a" then conv_link
  elif .t == "ul" or .t == "ol" then ".RS 2\n", .c[], ".RE"
  elif .t == "li" then ".IP \\[bu] 2", .c[]
  elif .t == "em"     then "\\f[I]" , .c[], "\\f[R]"
  elif .t == "strong" then "\\f[B]" , .c[], "\\f[R]"
  elif .t == "code"   then "\\f[CB]", .c[], "\\f[R]"
  elif .t == "blockquote" then ".RS\n", .c[], ".RE\n"
  elif .t == "h1" then ".SH ", .c[], "\n" | ascii_upcase
  elif .t == "h2" then ".SS ", .c[], "\n" | ascii_upcase
  elif .t == "h3" then ".SS ", .c[], "\n"
  elif .t == "section" or .t == "body" then .c[]
  elif .t == "div" then
    # strip away first paragraph to avoid ugly empty line after header
    if .c[:2] == ["\n", ".PP\n"] then .c |= .[2:] else error end |
    ".PP\n", "\\f[I]", .a.class, "\\f[R]", "\n",
    ".RS\n", .c[], ".RE\n"
  else error
  end
