local describe = require("busted").describe
local assert = require("busted").assert
local it = require("busted").it
local pp = require("metalua.pprint").print
local M = require("metalua.compiler.ast_to_src")

local mlc = require("metalua.compiler").new()

function scandir(directory)
   local i, t, popen = 0, {}, io.popen
   local pfile = popen('ls -a "' .. directory .. '"')
   for filename in pfile:lines() do
      i = i + 1
      t[i] = filename
   end
   pfile:close()
   return t
end

-- pp(scandir("luatests"))
local targets = scandir("luatests")

local function s2s(x)
   -- os.execute("stylua luatests/" .. x)
   local file = io.open("luatests/" .. x, "r")
   local str = file and file:read("*a")
   local newfile = io.open("rebuild/" .. x, "w")
   if newfile then
      newfile:write(M(mlc:src_to_ast(str)))
      newfile:close()
   end
   -- assert.same(str, newstr)
end

describe("s2s", function()
   it("should work on plain lua", function()
      for i = 3, #targets do
         local v = targets[i]
         print(v)
         s2s(v)
      end
      os.execute("stylua rebuild/")
   end)
   -- it("should work on metalua code by loading proper extension and de-suger the code", function()
   --    s2s()
   -- end)
end)
