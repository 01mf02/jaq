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
rec_tags |=
    if .t == "code" then .c[] |= sub("^ "; "\\ ")
  elif .t == "pre" then ".IP\n", ".EX\n", .c[].c[], ".EE\n"
  elif .t == "div" then {t: "p", c: [
    {t: "em", c: [.a.class]}, "\n",
    {t: "blockquote", c:
      # strip away first paragraph to avoid ugly empty line after header
      if .c[0] == "\n" and .c[1].t == "p" then .c[1].c + .c[2:] else error end
    }]}
  end |
rec_tags |=
    if .t == "p"  then ".PP\n", .c[]
  elif .t == "a" then conv_link
  elif .t == "ul" or .t == "ol" then ".RS 2\n", .c[], ".RE\n"
  elif .t == "li" then ".IP \\[bu] 2", .c[]
  elif .t == "em"     then "\\f[I]" , .c[], "\\f[R]"
  elif .t == "strong" then "\\f[B]" , .c[], "\\f[R]"
  elif .t == "code"   then "\\f[CB]", .c[], "\\f[R]"
  elif .t == "blockquote" then ".RS\n", .c[], ".RE\n"
  elif .t == "h1" then ".SH ", .c[], "\n" | ascii_upcase
  elif .t == "h2" then ".SS ", .c[], "\n" | ascii_upcase
  elif .t == "h3" then ".SS ", .c[], "\n"
  elif .t == "section" or .t == "body" then .c[]
  else error
  end
