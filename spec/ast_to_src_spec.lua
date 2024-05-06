local pp = require("metalua.pprint").print
local M = require("metalua.compiler.ast_to_src")

local mlc = require("metalua.compiler").new()

local function scandir(directory)
   local i, t, popen = 0, {}, io.popen
   local pfile = popen('ls -a "' .. directory .. '"')
   for filename in pfile:lines() do
      i = i + 1
      t[i] = filename
   end
   pfile:close()
   return t
end

local targets = scandir("luatests")

local function s2s(x)
   local file = io.open("luatests/" .. x, "r")
   local str = file and file:read("*a")
   local newfile = io.open("rebuild/" .. x, "w")
   if newfile then
      newfile:write(M(mlc:src_to_ast(str)))
      newfile:close()
   end
end

local function recompile()
   for i = 3, #targets do
      local v = targets[i]
      print(v)
      s2s(v)
   end
   os.execute("stylua rebuild/")
end

local get_output = function(x)
   local file1 = io.popen("lua5.1 luatests/" .. x, "r")
   local file2 = io.popen("lua5.1 rebuild/" .. x, "r")
   local str1 = file1 and file1:read("*a")
   local str2 = file2 and file2:read("*a")
   if str1 == nil then
      print("str1 is nil")
   end

   -- print("str1 " .. str1)
   -- print("str2 " .. str2)
   assert.equals(str1, str2)
end

describe("s2s", function()
   it("should work on plain lua", function()
      for i = 3, #targets do
         local v = targets[i]

         -- print(v)
         if
            v
            ~= (
               "test-print.lua"
               or "regex-dna.lua"
               or "n-body.lua"
               or "base_lib_ipairs_2.lua"
               or "array3d.lua"
               or "base_lib_next.lua"
            )
         then
            print(v)
            get_output(v)
         end
      end
   end)
   -- it("should work on metalua code by loading proper extension and de-suger the code", function()
   --    s2s()
   -- end)
end)
