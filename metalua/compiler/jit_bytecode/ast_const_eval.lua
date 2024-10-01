local ConstRule = {}
local pp = require("metalua.pprint")
-- A function that return a numeric constant if an AST node evaluate to an
-- arithmetic constant or "nil" otherwise.
-- The implementation of the function is given below.
local const_eval

local function dirop_compute(o, a, b)
   if o == "add" then
      return a + b
   elseif o == "sub" then
      return a - b
   elseif o == "mul" then
      return a * b
   elseif o == "div" then
      return (a ~= 0 or b ~= 0) and (a / b) or nil
   elseif o == "mod" then
      return a % b
   elseif o == "pow" then
      return a ^ b
   end
end

function ConstRule.Number(node)
   local v = node[1]
   if type(v) == "number" then
      return v
   end
end

function ConstRule.String(node)
   local v = node[1]
   if type(v) == "string" then
      return v
   end
end

function ConstRule.True()
   return true
end

function ConstRule.False()
   return false
end

function ConstRule.Op(node)
   if #node == 3 then
      return ConstRule.BinaryExpression(node)
   elseif #node == 2 then
      return ConstRule.UnaryExpression(node)
   else
      error("Invalid number of arguments for operator")
   end
end

function ConstRule.BinaryExpression(node)
   local o = node[1]
   local a = const_eval(node[2])
   if a then
      local b = const_eval(node[3])
      if b then
         return dirop_compute(o, a, b)
      end
   end
end

function ConstRule.UnaryExpression(node)
   local o = node[1]
   if o == "unm" then
      local v = const_eval(node[2])
      if v then
         return -v
      end
   end
end

function ConstRule.Paren(node)
   return const_eval(node[1])
end

function const_eval(node)
   local rule = ConstRule[node.tag]
   if rule then
      return rule(node)
   end
end

return const_eval
