local describe = require("busted").describe
local assert = require("busted").assert
local it = require("busted").it
local pp = require("metalua.pprint").print
local M = require("metalua.grammar.generator")

describe("sequence", function()
   local sequence = M.sequence
   it("should return a new sequence parser generator with new fields", function()
      local seq = sequence({ name = "hello", builder = unpack, transformer = {}, "try", "catch" })
      assert.same("hello", seq.name)
      assert.same("sequence", seq.kind)
   end)
   it("should find proper name for itself", function()
      local seq = sequence({ builder = unpack, transformer = {}, "try", "catch" })
      local seq1 = sequence({ builder = unpack, transformer = {}, "try", function() end })
      local seq2 = sequence({ builder = unpack, transformer = {}, function() end })
      assert.same("try ... catch", seq.name)
      assert.same("try ...", seq1.name)
      assert.same("unnamed_sequence", seq2.name)
   end)
end)
describe("multisequence", function() end)
describe("list", function() end)
describe("onkeyword", function()
   it("should initialize a new onkeyword parser generator", function()
      local onkeyword = M.onkeyword
      local on = onkeyword({
         name = "method invocation",
         ":",
         transformers = {
            function(a)
               return a
            end,
         },
      })
      assert.same("hello", on.keyword)
      assert.same("on_keyword", on.kind)
   end)
end)
describe("optkeyword", function() end)
