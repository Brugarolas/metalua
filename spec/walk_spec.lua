local describe = require("busted").describe
local assert = require("busted").assert
local it = require("busted").it
local pp = require("metalua.pprint").print
local M = require("metalua.treequery.walk")

describe("tags", function()
   it("should contain all tags", function()
      local stat = {
         Break = 11,
         Call = 14,
         Do = 1,
         Forin = 9,
         Fornum = 8,
         Goto = 12,
         If = 10,
         Invoke = 15,
         Label = 13,
         Local = 5,
         Localrec = 6,
         Repeat = 4,
         Return = 7,
         Set = 2,
         While = 3,
      }
      local expr = {
         Call = 2,
         Dots = 10,
         False = 12,
         Function = 6,
         Id = 15,
         Index = 4,
         Invoke = 3,
         Nil = 9,
         Number = 13,
         Op = 5,
         Paren = 1,
         Stat = 7,
         String = 14,
         Table = 8,
         True = 11,
      }
      assert.same(stat, M.tags.stat)
      assert.same(expr, M.tags.expr)
   end)
end)

describe("guess", function()
   it("try to guess the type of the AST then choose the right walker. ", function()
      -- M.guess({}, { tag = "Op", "add", { tag = "Number", 1 }, { tag = "Number", 2 } })
      -- M.guess({}, { tag = "Op", "add", { tag = "Number", 1 }, { tag = "Number", 2 } })
      stat_ast = {
         [1] = {
            [1] = {
               [1] = "a",
               tag = "Id",
            },
         },
         [2] = {
            [1] = {
               [1] = 1,
               tag = "Number",
            },
         },
         tag = "Local",
      }
      M.guess({}, stat_ast)
   end)
end)
