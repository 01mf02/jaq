function Header(el)
  -- uppercase top-level headings for man pages
  if FORMAT == "man" and el.level <= 2 then
    return el:walk{Str = function(el) return pandoc.Str(string.upper(el.text)) end}
  end
end

-- Map "Advanced" and "Compatibility" sections to definition lists in man page
function Div(el)
  local class = el.classes[1]
  if FORMAT == "man" and class ~= "section" then
    return pandoc.DefinitionList({{pandoc.Emph(class), el.content}})
  end
end

function Code(code)
  if FORMAT == "man" then
    return pandoc.Strong(code)
  end
end
