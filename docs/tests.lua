function Pandoc(el)
  el.blocks:walk{Code = function(code)
    local startIndex, endIndex = string.find(code.text, " --> ", 1, true)
    if startIndex ~= nil then
      local filter = string.sub(code.text, 1, startIndex - 1)
      local output = string.sub(code.text, endIndex + 1)
      print(filter)
      print("null") -- input
      print(output)
      print() -- end of test
    end
  end}
  os.exit(0)
end
