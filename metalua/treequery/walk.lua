-------------------------------------------------------------------------------
-- Copyright (c) 2006-2013 Fabien Fleutot and others.
--
-- All rights reserved.
--
-- This program and the accompanying materials are made available
-- under the terms of the Eclipse Public License v1.0 which
-- accompanies this distribution, and is available at
-- http://www.eclipse.org/legal/epl-v10.html
--
-- This program and the accompanying materials are also made available
-- under the terms of the MIT public license which accompanies this
-- distribution, and is available at http://www.lua.org/license.html
--
-- Contributors:
--     Fabien Fleutot - API and implementation
--
-------------------------------------------------------------------------------

-- Low level AST traversal library.
-- This library is a helper for the higher-level treequery library.
-- It walks through every node of an AST, depth-first, and executes
-- some callbacks contained in its cfg config table:
--
-- * cfg.down(...) is called when it walks down a node, and receive as
--   parameters the node just entered, followed by its parent, grand-parent
--   etc. until the root node.
--
-- * cfg.up(...) is called when it walks back up a node, and receive as
--   parameters the node just entered, followed by its parent, grand-parent
--   etc. until the root node.
--
-- * cfg.occurrence(binder, id_node, ...) is called when it visits an `Id{ }
--   node which isn't a local variable creator. binder is a reference to its
--   binder with its context. The binder is the `Id{ } node which created
--   this local variable. By "binder and its context", we mean a list starting
--   with the `Id{ }, and followed by every ancestor of the binder node, up until
--   the common root node.
--   binder is nil if the variable is global.
--   id_node is followed by its ancestor, up until the root node.
--
-- cfg.scope is maintained during the traversal, associating a
-- variable name to the binder which creates it in the context of the
-- node currently visited.
--
-- walk.traverse.xxx functions are in charge of the recursive descent into
-- children nodes. They're private helpers.
--
-- corresponding walk.xxx functions also take care of calling cfg callbacks.

-- -{ extension ("match", ...) }

local pp = require("metalua.pprint")

local M = { traverse = {}, tags = {}, debug = false }

local function table_transpose(t)
   local tt = {}
   for a, b in pairs(t) do
      tt[b] = a
   end
   return tt
end

--------------------------------------------------------------------------------
-- Standard tags: can be used to guess the type of an AST, or to check
-- that the type of an AST is respected.
--------------------------------------------------------------------------------
M.tags.stat = table_transpose({
   "Do",
   "Set",
   "While",
   "Repeat",
   "Local",
   "Localrec",
   "Return",
   "Fornum",
   "Forin",
   "If",
   "Break",
   "Goto",
   "Label",
   "Call",
   "Invoke",
})
M.tags.expr = table_transpose({
   "Paren",
   "Call",
   "Invoke",
   "Index",
   "Op",
   "Function",
   "Stat",
   "Table",
   "Nil",
   "Dots",
   "True",
   "False",
   "Number",
   "String",
   "Id",
})
require("moon.all")
--------------------------------------------------------------------------------
-- These [M.traverse.xxx()] functions are in charge of actually going through
-- ASTs. At each node, they make sure to call the appropriate walker.
-- stylua: ignore
function M.traverse.stat(cfg, x, ...)
   if M.debug then
      pp.printf("traverse stat %s", x)
   end
   local ancestors = { ... }
   local B = function(y) M.block(cfg, y, x, unpack(ancestors)) end -- Block
   local S = function(y) M.stat(cfg, y, x, unpack(ancestors)) end -- Statement
   local E = function(y) M.expr(cfg, y, x, unpack(ancestors)) end -- Expression
   local EL = function(y) M.expr_list(cfg, y, x, unpack(ancestors)) end -- Expression List
   local IL = function(y) M.binder_list(cfg, y, x, unpack(ancestors)) end -- Id binders List
   local OS = function() cfg.scope:save() end -- Open scope
   local CS = function() cfg.scope:restore() end -- Close scope
   if not x.tag then
      for _, y in ipairs(x) do
         -- M.stat(cfg, y, ...)
         S(y)
      end
   elseif x.tag == "Do" then
      OS()
      for _, y in ipairs(x) do
         S(y)
      end
      CS()
   elseif x.tag == "Set" then
      EL(x[1]); EL(x[2])
   elseif x.tag == "While" then
      E(x[1]); OS(); B(x[2]); CS()
   elseif x.tag == "Repeat" then
      OS(); B(x[1]); E(x[2]); CS()
   elseif x.tag == "Local" then
      if x[2] then
         EL(x[2]); IL(x[1])
      else
         IL(x[1])
      end
   elseif x.tag == "Localrec" then
      IL(x[1]); EL(x[2])
   elseif x.tag == "Fornum" then
      if x[5] then
         E(x[2]); E(x[3]); E(x[4]); OS(); IL({ x[1] }); B(x[5]); CS()
      else
         E(x[2]); E(x[3]); OS(); IL({ x[1] }); B(x[4]); CS()
      end
   elseif x.tag == "Forin" then
      EL(x[2]); OS(); IL(x[1]); B(x[3]); CS()
   elseif x.tag == "If" then
      for i = 1, #x - 1, 2 do
         E(x[i]); OS(); B(x[i + 1]); CS()
      end
      if #x % 2 == 1 then
         OS(); B(x[#x]); CS()
      end
   elseif x.tag == "Call" or "Invoke" or "Function" then
      EL(x)
   elseif x.tag == "Break" or "Goto" or "Label" then
   else
      if M.tags.stat[x.tag] then
         M.malformed(cfg, x, unpack(ancestors))
      else
         M.unknown(cfg, x, unpack(ancestors))
      end
   end
end
-- stylua: ignore
function M.traverse.expr(cfg, x, ...)
   local tag
   if type(x) == "table" then
      tag = x.tag
   else
      return
   end
   -- pp.print(cfg)
   if M.debug then
      pp.printf("traverse expr %s", x)
   end
   local ancestors = { ... }
   local B = function(y)
      M.block(cfg, y, x, unpack(ancestors))
   end -- Block
   local S = function(y)
      M.stat(cfg, y, x, unpack(ancestors))
   end -- Statement
   local E = function(y)
      M.expr(cfg, y, x, unpack(ancestors))
   end -- Expression
   local EL = function(y)
      M.expr_list(cfg, y, x, unpack(ancestors))
   end -- Expression List
   local IL = function(y)
      M.binder_list(cfg, y, x, unpack(ancestors))
   end -- Id binders list
   local OS = function()
      cfg.scope:save()
   end -- Open scope
   local CS = function()
      cfg.scope:restore()
   end -- Close scope
   if tag == "Paren" then
      E(x[1])
   elseif tag == "Call" or "Invoke" then
      EL(x)
   elseif tag == "Index" then
      E(x[1]); E(x[2])
   elseif tag == "Op" then
      E(x[2])
      if #x == 3 then
         E(x[3])
      end
   elseif tag == "Function" then
      OS(); IL(x[1]); B(x[2]); CS()
   elseif tag == "Stat" then
      OS(); B(x[1]); E(x[2]); CS()
   elseif tag == "Id" then
      M.occurrence(cfg, x, unpack(ancestors))
   elseif tag == "Table" then
      for i = 1, #x do
         if x[i].tag == "Pair" then
            E(x[i][1]); E(x[i][2])
         else
            E(x[i])
         end
      end
   -- elseif tag == ("Nil" or "Dots" or "True" or "False" or "Number" or "String") then
   --    print("hear")
   --    return
   else
      if x.tags.expr[tag] then
         M.malformed(cfg, x, unpack(ancestors))
      else
         M.unknown(cfg, x, unpack(ancestors))
      end
   end
end

function M.traverse.block(cfg, x, ...)
   assert(type(x) == "table", "traverse.block() expects a table")
   if x.tag then
      M.malformed(cfg, x, ...)
   else
      for _, y in ipairs(x) do
         M.stat(cfg, y, x, ...)
      end
   end
end

function M.traverse.expr_list(cfg, x, ...)
   assert(type(x) == "table", "traverse.expr_list() expects a table")
   -- x doesn't appear in the ancestors
   for _, y in ipairs(x) do
      M.expr(cfg, y, ...)
   end
end

function M.malformed(cfg, x, ...)
   local f = cfg.malformed or cfg.error
   if f then
      f(x, ...)
   else
      error("Malformed node of tag " .. (x.tag or "(nil)"))
   end
end

function M.unknown(cfg, x, ...)
   local f = cfg.unknown or cfg.error
   if f then
      f(x, ...)
   else
      error("Unknown node tag " .. (x.tag or "(nil)"))
   end
end

function M.occurrence(cfg, x, ...)
   if cfg.occurrence then
      cfg.occurrence(cfg.scope:get(x[1]), x, ...)
   end
end

-- TODO: Is it useful to call each error handling function?
function M.binder_list(cfg, id_list, ...)
   local f = cfg.binder
   local ferror = cfg.error or cfg.malformed or cfg.unknown
   for i, id_node in ipairs(id_list) do
      if id_node.tag == "Id" then
         cfg.scope:set(id_node[1], { id_node, ... })
         if f then
            f(id_node, ...)
         end
      elseif i == #id_list and id_node.tag == "Dots" then
         -- Do nothing, those are valid `Dots
      elseif ferror then
         -- Traverse error handling function
         ferror(id_node, ...)
      else
         error("Invalid binders list")
      end
   end
end

----------------------------------------------------------------------
-- Generic walker generator.
-- * if `cfg' has an entry matching the tree name, use this entry
-- * if not, try to use the entry whose name matched the ast kind
-- * if an entry is a table, look for 'up' and 'down' entries
-- * if it is a function, consider it as a `down' traverser.
----------------------------------------------------------------------
local walker_builder = function(traverse)
   assert(traverse)
   return function(cfg, ...)
      if not cfg.scope then
         cfg.scope = M.newscope()
      end
      local down, up = cfg.down, cfg.up
      local broken = down and down(...)
      if broken ~= "break" then
         M.traverse[traverse](cfg, ...)
      end
      if up then
         up(...)
      end
   end
end

----------------------------------------------------------------------
-- Declare [M.stat], [M.expr], [M.block] and [M.expr_list]
----------------------------------------------------------------------
for _, w in ipairs({ "stat", "expr", "block" }) do --, "malformed", "unknown" } do
   M[w] = walker_builder(w, M.traverse[w])
end

-- Don't call up/down callbacks on expr lists
M.expr_list = M.traverse.expr_list

----------------------------------------------------------------------
-- Try to guess the type of the AST then choose the right walkker.
----------------------------------------------------------------------
function M.guess(cfg, x, ...)
   assert(type(x) == "table", "arg #2 in a walker must be an AST")
   if M.tags.expr[x.tag] then
      return M.expr(cfg, x, ...)
   end
   if M.tags.stat[x.tag] then
      return M.stat(cfg, x, ...)
   end
   if not x.tag then
      return M.block(cfg, x, ...)
   end
   error("Can't guess the AST type from tag " .. (x.tag or "<none>"))
end

local S = {}
S.__index = S

function M.newscope()
   local instance = { current = {} }
   instance.stack = { instance.current }
   setmetatable(instance, S)
   return instance
end

function S:save(...)
   local current_copy = {}
   for a, b in pairs(self.current) do
      current_copy[a] = b
   end
   table.insert(self.stack, current_copy)
   if ... then
      return self:add(...)
   end
end

function S:restore()
   self.current = table.remove(self.stack)
end

function S:get(var_name)
   return self.current[var_name]
end

function S:set(key, val)
   self.current[key] = val
end

return M
