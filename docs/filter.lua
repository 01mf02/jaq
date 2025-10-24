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
  -- jq is unfortunately currently not a language with highlighting support
  --code.classes[1] = "jq"
  if FORMAT == "man" then
    return pandoc.Strong(code)
  else
    local filter, output = code_test(code)
    if filter ~= nil then
      code.text = filter .. " ⟼ " .. output
      local link = play_link(filter, output)
      return pandoc.Span({code, " ", link}, {class = "example"})
    end
  end
end

function CodeBlock(code)
  if FORMAT == "man" then
    return code
  else
    local filter, output = code_test(code)
    if filter ~= nil then
      code.text = filter .. " ⟼ " .. output
      local link = play_link(filter, output)
      return pandoc.Div({code, link}, {class = "example block"})
    end
  end
end

function play_link(filter, output)
  local input = "null"
  local url = "https://gedenkt.at/jaq/?q=" .. escape_url(filter) .. "&j=" .. escape_url(input)
  local run_attrs = {target = "_blank", rel = "noopener"}
  return pandoc.Link("▶️", url, "Run example", run_attrs)
end

function escape_url(str)
  return str
    :gsub("\n", "\r\n")
    :gsub("([^%w%.%- ])", function(c) return string.format("%%%02X", string.byte(c)) end)
    :gsub(" ", "+")
end

-- Return filter and expected output if given code is a unit test
function code_test(code)
  -- if a code class has been given
  if next(code.classes) ~= nil then
    return
  end
  local startIndex, endIndex = string.find(code.text, "-->", 1, true)
  if startIndex ~= nil then
    local filter = string.sub(code.text, 1, startIndex - 1)
    local output = string.sub(code.text, endIndex + 1)
    return filter, output
  end
end

function trim(s)
  return s:match"^%s*(.*)":match"(.-)%s*$"
end

-- Make headings links to their ancestor sections
function Pandoc(doc)
  if FORMAT == "man" then
    return doc
  end

  local current_sections = {}

  local function walk_blocks(blocks)
    for _, blk in pairs(blocks) do
      if blk.t == "Div" and blk.classes[1] == "section" then
        table.insert(current_sections, blk.identifier)
        walk_blocks(blk.content)
        table.remove(current_sections)
      elseif blk.t == "Header" then
        local ancestor_id = current_sections[#current_sections]
        blk.content = {pandoc.Link(blk.content, "#" .. ancestor_id)}
      end
    end
  end
  walk_blocks(doc.blocks)
  return doc
end

-- Print unit tests when running `pandoc --from filter.lua`
function Writer(doc, opts)
  local test = function(code)
    local filter, output = code_test(code)
    if filter ~= nil then
      print((trim(filter):gsub("#[^\n]*", ""):gsub("\n", " ")))
      print("null") -- input
      print(trim(output))
      print() -- end of test
    end
  end
  doc.blocks:walk{Code = test, CodeBlock = test}
  os.exit(0)
end
