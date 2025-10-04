function Header(el)
  -- uppercase top-level headings for man pages
  if FORMAT == "man" and el.level <= 2 then
    return el:walk{Str = function(el) return pandoc.Str(string.upper(el.text)) end}
  end
end

function Code(code)
  -- jq is unfortunately currently not a language with highlighting support
  --code.classes[1] = "jq"
  if FORMAT == "man" then
    return pandoc.Strong(code)
  else
    local filter, output = codeTest(code.text)
    if filter ~= nil then
      local input = "null"
      local url = "https://gedenkt.at/jaq/?q=" .. encodeUrl(filter) .. "&j=" .. encodeUrl(input)

      code.text = filter .. " ⟼ " .. output
      local run_attrs = {target = "_blank", rel = "noopener"}
      return {code, " ", pandoc.Link("▶️", url, "Run example", run_attrs)}
    end
  end
end

-- code blocks are assumed to be in jq if no other language is given
function CodeBlock(block)
  if next(block.classes) == nil then
    block.classes[1] = "jq"
    return block
  end
end

function encodeUrl(str)
  str = string.gsub(str, "\n", "\r\n")
  str = string.gsub(str, "([^%w%.%- ])", function(c) return string.format("%%%02X", string.byte(c)) end)
  str = string.gsub(str, " ", "+")
  return str
end

-- Return filter and expected output if given code is a unit test
function codeTest(code)
  local startIndex, endIndex = string.find(code, " --> ", 1, true)
  if startIndex ~= nil then
    local filter = string.sub(code, 1, startIndex - 1)
    local output = string.sub(code, endIndex + 1)
    return filter, output
  end
end

-- Print unit tests when running `pandoc --from filter.lua`
function Writer(doc, opts)
  doc.blocks:walk{Code = function(code)
    local filter, output = codeTest(code.text)
    if filter ~= nil then
      print(filter)
      print("null") -- input
      print(output)
      print() -- end of test
    end
  end}
  os.exit(0)
end
