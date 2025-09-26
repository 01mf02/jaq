function Header(el)
  -- uppercase top-level headings for man pages
  if FORMAT == "man" and el.level <= 2 then
    return el:walk{Str = function(el) return pandoc.Str(string.upper(el.text)) end}
  end
end

function Code(code)
  -- inline code is assumed to be in jq
  --code.classes[1] = "jq"
  if FORMAT == "man" then
    return pandoc.Strong(code)
  end
  --return code
end

-- code blocks are assumed to be in jq if no other language is given
function CodeBlock(block)
  if next(block.classes) == nil then
    block.classes[1] = "jq"
    return block
  end
end

function Div(el)
  local class = el.classes[1]
  if class == "Examples" then
    el = el:walk{CodeBlock = function(block) return example(block.text) end}

    if FORMAT == "html" then
      local summary = pandoc.Plain{
        pandoc.RawInline("html",  '<summary>'), pandoc.Str 'Examples',
        pandoc.RawInline("html", "</summary>")
      }
      return {
        pandoc.RawBlock("html",  '<details>'), summary, el,
        pandoc.RawBlock("html", '</details>')
      }
    end
  end
  if FORMAT == "man" then
    el = pandoc.DefinitionList({{pandoc.Emph(class), el.content}})
  end
  return el
end

function example(test)
  local _, _, filter, input, output = test:find("([^\n]+)\n([^\n]+)\n(.*)")

  if FORMAT == "man" then
    filter = "'" .. filter .. "'"
    input = "'" .. input .. "'"
    return pandoc.CodeBlock("$ jaq " .. filter .. " \\\n  <<< " .. input .. "\n" .. output)
  end
end
