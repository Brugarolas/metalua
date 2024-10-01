local BoolConstRule = {}
-- A function that return a numeric constant if an AST node evaluate to an
-- arithmetic constant or "nil" otherwise.
-- The implementation of the function is given below.
local const_eval

local function dirop_compute(o, a, b)
   if o == "and" then
      return a and b
   elseif o == "or" then
      return a or b
   end
end

function BoolConstRule.True(_)
   return true
end

function BoolConstRule.False(_)
   return false
end

function BoolConstRule.BinaryExpression(node)
   local o = node[1]
   local a = const_eval(node[2])
   if a ~= nil then
      local b = const_eval(node[3])
      if b ~= nil then
         return dirop_compute(o, a, b)
      end
   end
end

function BoolConstRule.UnaryExpression(node)
   local o = node.operator
   if o == "not" then
      local v = const_eval(node[1])
      if v ~= nil then
         return not v
      end
   end
end

function BoolConstRule.Op(node)
   if #node == 3 then
      return BoolConstRule.BinaryExpression(node)
   elseif #node == 2 then
      return BoolConstRule.UnaryExpression(node)
   else
      error("Invalid number of arguments for operator")
   end
end

function const_eval(node)
   local rule = BoolConstRule[node.tag]
   if rule then
      return rule(node)
   end
end

return const_eval
