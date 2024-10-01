--
-- LuaJIT Language Toolkit.
--
-- Copyright (C) 2013-2014 Francesco Abbate. All rights reserved.
--
-- Based on the original work of Richard Hundt,
-- https://github.com/richardhundt/nyanga.
--
-- See Copyright Notice in LICENSE
--

local bc = require("metalua.jit.bytecode")
local const_eval = require("metalua.jit.const")
local boolean_const_eval = require("metalua.jit.bool_const")
local pp = require("metalua.pprint")

local ID = 0
local function genid()
   ID = ID + 1
   return "__" .. ID
end

local BC = bc.BC

-- comparison operators with corresponding instruction.
-- the boolean value indicate if the operands should be swapped.

local cmpop = {
   ["lt"] = { "LT", false },
   ["le"] = { "LE", false },
   ["eq"] = { "EQ", false },
   ["ne"] = { "NE", false },
}
local cmpopinv = {
   ["lt"] = { "GE", false },
   ["le"] = { "GT", false },
   ["eq"] = { "NE", false },
   ["ne"] = { "EQ", false },
}

local function lang_error(msg, chunkname, line)
   error(string.format("LLT-ERROR%s:%d: %s", chunkname, line, msg), 0)
end

local MULTIRES = -1

-- this should be considered like binary values to perform
-- bitfield operations
local EXPR_RESULT_TRUE, EXPR_RESULT_FALSE = 1, 2
local EXPR_RESULT_BOTH = 3

-- Infix arithmetic instructions
local EXPR_EMIT_VN = { value = true, number = true }

-- USETx, ISEQx and ISNEx instructions
local EXPR_EMIT_VSNP = { value = true, string = true, number = true, primitive = true }

-- TGETx/TSETx instructions
local EXPR_EMIT_VSB = { value = true, string = true, byte = true }

local function store_bit(cond)
   return cond and EXPR_RESULT_TRUE or EXPR_RESULT_FALSE
end

-- Logical XOR (exclusive OR)
local function xor(a, b)
   return (a and not b) or (not a and b)
end

local Stat = {}
local Expr = {}
local MultiExprRule = {}
local Lhs = {}
local Test = {}

local function is_local_var(ctx, node)
   if node.tag == "Id" then
      local info, uval = ctx:lookup(node[1]) -- HACK: IS THIS RIGHT?
      if info and not uval then
         return info.idx
      end
   end
end

local function is_vcall(node)
   return (MultiExprRule[node.tag] ~= nil)
end

local function mov_toreg(ctx, dest, src)
   if dest ~= src then
      ctx:op_move(dest, src)
   end
end

-- Conditionally move "src" to "dest" and jump to given target
-- if "src" evaluate to true/false according to "cond".
local function cond_mov_toreg(ctx, cond, dest, src, jump_label, jreg)
   if dest ~= src then
      ctx:op_testmov(cond, dest, src, jump_label, jreg)
   else
      ctx:op_test(cond, src, jump_label, jreg)
   end
end

local function is_byte_number(v)
   return type(v) == "number" and v % 1 == 0 and v >= 0 and v < 256
end

-- ExpressionRule's entries take a node and a destination register (dest)
-- used to store the result. At the end of the call no new registers are
-- marked as used.
-- ExpressionRule functions return nothing or a boolean value to indicate if
-- a the expression terminate with a tail call instruction.

function Expr:Number(node, dest)
   self.ctx:op_load(dest, node[1])
end

function Expr:String(node, dest)
   self.ctx:op_load(dest, node[1])
end
function Expr:True(_, _)
   return true
end

function Expr:False(_, _)
   return false
end

function Expr:Id(node, dest)
   local name = node[1]
   local var, uval = self.ctx:lookup(name)
   if var then
      if uval then
         -- Ensure variable is marked as upvalue in proto in take
         -- the upvalue index.
         local uv = self.ctx:upval(name)
         self.ctx:op_uget(dest, uv)
      else
         mov_toreg(self.ctx, dest, var.idx)
      end
   else
      self.ctx:op_gget(dest, name)
   end
end

function Expr:Dots(node, dest)
   self.ctx:op_varg(dest, 1)
end

-- MultiExprRule's entries take a node and a number of wanted results (want)
-- and an optional boolean argument "tail" that indicate to emit tail call
-- if possible.
-- The argument "want" can also be MULTIRES to indicate that the caller want
-- as many results as the instructions returns.
-- The code will store on the stack (starting from freereg) the number of
-- wanted results.
-- Return a first boolean value to indicate if many results are generated.
-- A second boolean value indicate if a tail call was actually done.

function MultiExprRule:Dots(node, want)
   self.ctx:op_varg(self.ctx.freereg, want)
   return true, false -- Multiple results, no tail call.
end

local function expr_isk(_, node)
   -- HACK:
   if type(node) ~= "table" then
      return true, node
   end
   local const = const_eval(node)
   if const then
      return true, const
   elseif node.tag == "Literal" then
      local t = type(node.value)
      return (t == "string" or t == "boolean" or t == "nil"), node.value
   else
      return false
   end
end

local function emit_tdup(self, dest, ins)
   local kidx, t = self.ctx:new_table_template()
   ins:rewrite(BC.TDUP, dest, kidx)
   return t
end

local function is_kint(x)
   return x % 1 == 0 and x >= 0 and x < 2 ^ 31
end

function Expr:Table(node, dest)
   if #node == 0 then
      self.ctx:op_tnew(dest, 0, 0)
      return
   end

   local free = self.ctx.freereg
   local ins = self.ctx:op_tnew(free, 0, 0)
   self.ctx:nextreg()
   local t
   local vtop = self.ctx.freereg
   local narray, nhash = 0, 0
   local na, nh = 0, 0
   local zeroarr = 0
   for k = 1, #node do
      local value, key = node[k], k
      if value.tag == "Pair" then
         value, key = value[2], value[1]
         local k_is_const, kval = expr_isk(self, key)
         local v_is_const, vval = expr_isk(self, value)
         if k_is_const and kval ~= nil and v_is_const then
            if type(kval) == "number" and is_kint(kval) then
               if not t then
                  t = emit_tdup(self, free, ins)
               end
               t.array[kval] = vval
               narray = math.max(narray, kval + 1)
               if kval == 0 then -- Zero-indexed array term.
                  zeroarr = 1
               end
            else
               nhash = nhash + 1
               if not t then
                  t = emit_tdup(self, free, ins)
               end
               -- NB: Adopt the "keyvals" style instead of hash_keys/values.
               t.hash_keys[nhash] = kval
               t.hash_values[nhash] = vval
            end
         else
            local ktag, kval = self:expr_toanyreg_tagged(key, EXPR_EMIT_VSB)
            local v = self:expr_toanyreg(value)
            self.ctx:op_tset(free, ktag, kval, v)
            self.ctx.freereg = vtop
         end
         nh = nh + 1
      else
         na = na + 1
         local is_const, expr_val = expr_isk(self, value)
         if is_const then
            if not t then
               t = emit_tdup(self, free, ins)
            end
            t.array[na] = expr_val
            narray = na + 1
         -- elseif is_vcall(value) and k == #node.keyvals then
         elseif is_vcall(value) and k == #node then
            self:expr_tomultireg(value, MULTIRES)
            self.ctx:op_tsetm(free, na)
         else
            local ktag, kval
            if na < 256 then
               ktag, kval = "B", na
            else
               ktag, kval = "V", self.ctx:nextreg()
               self.ctx:op_load(kval, na)
            end
            local v = self:expr_toanyreg(value)
            self.ctx:op_tset(free, ktag, kval, v)
            self.ctx.freereg = vtop
         end
      end
   end

   if t then
      t.narray, t.nhash = narray, nhash
   else
      na = na + zeroarr
      nh = nh - zeroarr
      local sz = ins.tnewsize(na > 0 and na or nil, nh)
      ins:rewrite(BC.TNEW, free, sz)
   end

   mov_toreg(self.ctx, dest, free)

   self.ctx.freereg = free
end

-- Operations that admit instructions in the form ADDVV, ADDVN, ADDNV
-- TODO: string.upper?
local dirop = {
   ["add"] = "ADD",
   ["mul"] = "MUL",
   ["sub"] = "SUB",
   ["div"] = "DIV",
   ["mod"] = "MOD",
}

function Expr:ConcatenateExpression(node, dest)
   local free = self.ctx.freereg
   for i = 2, #node do
      self:expr_tonextreg(node[i])
   end
   self.ctx.freereg = free
   self.ctx:op_cat(dest, free, free + #node - 2)
end

function Expr:BinaryExpression(node, dest, jreg)
   local free = self.ctx.freereg
   local o = node[1]
   if cmpop[o] then
      local l = genid()
      self:test_emit(node, l, jreg, false, EXPR_RESULT_BOTH, dest)
      self.ctx:here(l)
   elseif dirop[o] then
      local atag, a = self:expr_toanyreg_tagged(node[2], EXPR_EMIT_VN)
      local btag, b = self:expr_toanyreg_tagged(node[3], EXPR_EMIT_VN)
      if atag == "N" and btag == "N" then
         -- handle "nan" values here the same way LuaJIT does
         -- usually, both operands will always be 0 when both constant but
         -- re-check just to make sure, in order to trigger the assert when
         -- there's a bug in the generator
         local aval = const_eval(node[2])
         local bval = const_eval(node[3])
         if aval == 0 and bval == 0 then
            atag, a = "V", self.ctx.freereg
            self.ctx:op_load(self.ctx:nextreg(), 0)
         else
            assert(false, "operands are both constants")
         end
      end
      self.ctx.freereg = free
      self.ctx:op_infix(dirop[o], dest, atag, a, btag, b)
   else
      local a = self:expr_toanyreg(node[2])
      local b = self:expr_toanyreg(node[3])
      self.ctx.freereg = free
      if o == "^" then
         self.ctx:op_pow(dest, a, b)
      else
         error("bad binary operator: " .. o, 2)
      end
   end
end

function Expr:ExpressionValue(node, dest, jreg)
   self:expr_toreg(node.value, dest, jreg)
end

function Expr:UnaryExpression(node, dest)
   local free = self.ctx.freereg
   local a = self:expr_toanyreg(node[2])
   self.ctx.freereg = free
   local o = node[1]
   if o == "unm" then
      self.ctx:op_unm(dest, a)
   elseif o == "len" then
      self.ctx:op_len(dest, a)
   elseif o == "not" then
      self.ctx:op_not(dest, a)
   else
      error("bad unary operator: " .. o, 2)
   end
end

local logical_op = {
   ["and"] = true,
   ["or"] = true,
}

function Expr:Op(node, dest, jreg)
   if logical_op[node[1]] then
      Expr.LogicalExpression(self, node, dest, jreg)
   elseif node[1] == "concat" then
      Expr.ConcatenateExpression(self, node, dest)
   elseif #node == 3 then
      Expr.BinaryExpression(self, node, dest, jreg)
   elseif #node == 2 then
      Expr.UnaryExpression(self, node, dest)
   end
end

function Expr:LogicalExpression(node, dest, jreg)
   local negate = (node[1] == "or")
   local lstore = store_bit(negate)
   local l = genid()
   self:test_emit(node[2], l, jreg, negate, lstore, dest)
   self:expr_toreg(node[3], dest, jreg)
   self.ctx:here(l)
end

function Expr:Index(node, dest)
   local free = self.ctx.freereg
   local lhs = self:lhs_expr_emit(node)
   self.ctx.freereg = free
   self.ctx:op_tget(dest, lhs.target, lhs.key_type, lhs.key)
end

function Stat:Localrec(node)
   -- local path = node.id
   local lhs
   -- TODO: ???
   -- if node.locald then
   -- We avoid calling "lhs_expr_emit" on "path" because
   -- it would mark the variable as mutable.
   local vinfo = self.ctx:newvar(node[1][1][1])
   self:expr_toreg(node[2][1], vinfo.idx)
   local pc = #self.ctx.code + 1
   vinfo.startpc = pc
   vinfo.endpc = pc
   -- else
   --    lhs = self:lhs_expr_emit(path)
   --    self:expr_tolhs(lhs, node)
   -- end
end

function Expr:Function(node, dest)
   local free = self.ctx.freereg
   -- TODO:
   local child = self.ctx:child(node.lineinfo.first.line, node.lineinfo.last.line)
   self.ctx = child
   for i = 1, #node[1] do
      if node[1][i].tag == "Dots" then
         self.ctx.flags = bit.bor(self.ctx.flags, bc.Proto.VARARG)
      else
         self.ctx:param(node[1][i][1])
      end
   end
   self:block_emit(node[2])
   self:close_proto(node.lineinfo.last.line)

   self.ctx = self.ctx:parent()
   self.ctx.freereg = free
   self.ctx:line(node.lineinfo.last.line)
   self.ctx:op_fnew(dest, child.idx)
end

-- TODO:
local function emit_call_expression(self, node, want, use_tail, use_self)
   local free = self.ctx.freereg
   local call_line = node.line --- TODO:
   if use_self then
      pp.print(node[1])
      local obj = self:expr_toanyreg(node[1]) -- TODO:
      self.ctx:op_move(free + 1, obj)
      self.ctx:setreg(free + 2)
      local method_type, method = self:property_tagged(node[2][1])
      self.ctx:op_tget(free, obj, method_type, method)
      self.ctx.freereg = free + 2
   else
      self:expr_tonextreg(node[1])
   end
   local start
   if use_self then
      start = 3
   else
      start = 2
   end
   for i = start, #node do
      self:expr_tonextreg(node[i])
   end
   local narg = use_self and #node - 2 or #node - 1
   local mres = false
   if narg > 0 then
      local lastarg = node[#node]
      mres = self:expr_tomultireg(lastarg, MULTIRES)
      self.ctx:nextreg()
   end

   if use_self then
      narg = narg + 1
   end
   self.ctx.freereg = free
   if mres then
      if use_tail then
         self.ctx:close_uvals()
         self.ctx:op_callmt(free, narg - 1)
      else
         self.ctx:op_callm(free, want, narg - 1)
      end
   else
      if use_tail then
         self.ctx:close_uvals()
         self.ctx:op_callt(free, narg)
      else
         self.ctx:op_call(free, want, narg)
      end
   end
   self.ctx:setpcline(call_line)

   return want == MULTIRES, use_tail
end

function MultiExprRule:Call(node, want, tail)
   return emit_call_expression(self, node, want, tail, false)
end

function MultiExprRule:Invoke(node, want, tail)
   return emit_call_expression(self, node, want, tail, true)
end

function Lhs:Id(node)
   local info, uval = self.ctx:lookup(node[1])
   if uval then
      -- Ensure variable is marked as upvalue in proto and take
      -- upvalue index.
      info.mutable = true
      local uv = self.ctx:upval(node[1])
      return { tag = "upval", uv = uv }
   elseif info then
      info.mutable = true
      return { tag = "local", target = info.idx }
   else
      return { tag = "global", name = node[1] }
   end
end

function Expr:Paren(node, dest)
   if node[1].tag == "Table" then
      self:expr_toreg(node[1])
      -- elseif node[1].tag == "Op" then
      --    Expr.Op(self, node[1], dest)
      -- const_eval(node)
   end
end

function Lhs:Index(node)
   local target = self:expr_toanyreg(node[1])
   local key_type, key
   -- if node.computed then
   key_type, key = self:expr_toanyreg_tagged(node[2], EXPR_EMIT_VSB)
   -- else
   -- TODO: computed??
   --   key_type, key = self:property_tagged(node.property.name)
   -- end
   return { tag = "member", target = target, key = key, key_type = key_type }
end

function Test:Op(node, jmp, jreg, negate, store, dest)
   if logical_op[node[1]] then
      Test.LogicalExpression(self, node, jmp, jreg, negate, store, dest)
   elseif #node == 3 then
      Test.BinaryExpression(self, node, jmp, jreg, negate, store, dest)
   elseif #node == 2 then
      Test.UnaryExpression(self, node, jmp, jreg, negate, store, dest)
   end
end

-- Return true IFF the variable "store" has the EXPR_RESULT_FALSE bit
-- set. If "negate" is true check the EXPR_RESULT_TRUE bit instead.
local function has_branch(store, negate)
   return bit.band(store, store_bit(negate)) ~= 0
end

local function compare_op(negate, op)
   local oper_table = negate and cmpop or cmpopinv
   local e = oper_table[op]
   return e[1], e[2]
end

function Test:BinaryExpression(node, jmp, jreg, negate, store, dest)
   local o = node[1]
   if cmpop[o] then
      local free = self.ctx.freereg
      local atag, a, btag, b
      if o == "eq" or o == "ne" then
         atag, a = self:expr_toanyreg_tagged(node[2], EXPR_EMIT_VSNP)
         if atag == "V" then
            btag, b = self:expr_toanyreg_tagged(node[3], EXPR_EMIT_VSNP)
         else
            btag, b = atag, a
            atag, a = "V", self:expr_toanyreg(node[3])
         end
      else
         a = self:expr_toanyreg(node[2])
         b = self:expr_toanyreg(node[3])
      end
      self.ctx.freereg = free
      local use_imbranch = has_branch(store, negate)
      if use_imbranch then
         -- no need to swap the operands here for metalua AST
         local test, swap = compare_op(not negate, o)
         local altlabel = genid()
         self.ctx:op_comp(test, a, btag, b, altlabel, free, swap)
         self.ctx:op_load(dest, negate)
         self.ctx:jump(jmp, jreg)
         self.ctx:here(altlabel)
         self.ctx.freereg = free
      else
         local test, swap = compare_op(negate, o)
         self.ctx:op_comp(test, a, btag, b, jmp, free, swap)
      end
      if has_branch(store, not negate) then
         self.ctx:op_load(dest, not negate)
      end
   else
      self:expr_test(node, jmp, jreg, negate, store, dest)
   end
end

function Test:UnaryExpression(node, jmp, jreg, negate, store, dest)
   if node.operator == "not" and store == 0 then
      self:test_emit(node, jmp, jreg, not negate)
   else
      self:expr_test(node, jmp, jreg, negate, store, dest or self.ctx.freereg)
   end
end

function Test:LogicalExpression(node, jmp, jreg, negate, store, dest)
   local or_operator = (node.operator == "or")
   local lstore = bit.band(store, store_bit(or_operator))
   local imbranch = xor(negate, or_operator)
   if imbranch then
      local templ = genid()
      self:test_emit(node.left, templ, jreg, not negate, lstore, dest)
      self:test_emit(node.right, jmp, jreg, negate, store, dest)
      self.ctx:here(templ)
   else
      self:test_emit(node.left, jmp, jreg, negate, lstore, dest)
      self:test_emit(node.right, jmp, jreg, negate, store, dest)
   end
end

function Stat:StatementsGroup(node)
   for i = 1, #node.statements do
      self:emit(node.statements[i])
   end
end

function Stat:Call(node)
   self:expr_tomultireg(node, 0, false)
end

function Stat:SendExpression(node)
   self:expr_tomultireg(node, 0, false)
end

function Stat:Label(node)
   local ok, label = self.ctx:goto_label(node[1][1])
   if not ok then
      lang_error(label, self.chunkname, node.lineinfo.first.line)
   end
end

function Stat:Goto(node)
   self.ctx:goto_jump(node[1][1], node.lineinfo.first.line)
end

function Stat:Do(node)
   self:block_enter()
   self:block_emit(node)
   self:block_leave(node.lineinfo.last.line)
end

function Stat:If(node, root_exit)
   local free = self.ctx.freereg
   local ncons = #node / 2
   -- Count the number of branches, including the "else" branch.
   -- local count = node.alternate and ncons + 1 or ncons
   local count = ncons
   local local_exit = count > 1 and genid()
   -- Set the exit point to the extern exit if given or set to local
   -- exit (potentially false).
   local exit = root_exit or local_exit

   local nbranch = (#node % 2 == 0) and #node or #node - 1

   for i = 1, nbranch, 2 do
      local test, block = node[i], node[i + 1]
      local next_test = genid()
      -- -- Set the exit point to jump on at the end of for this block.
      -- -- If this is the last branch (count == 1) set to false.
      local bexit = count > 1 and exit
      --
      self:test_emit(test, next_test, free)
      --
      self:block_enter()
      self:block_emit(block, bexit)
      self:block_leave(block.lineinfo.last.line, bexit)

      self.ctx:here(next_test)
      count = count - 1
   end

   if #node % 2 ~= 0 then
      self:block_enter()
      self:block_emit(node[#node])
      self:block_leave(node[#node].lineinfo.last.line)
   end
   if exit and exit == local_exit then
      self.ctx:here(exit)
   end
   self.ctx.freereg = free
end
-- HACK:
function Stat:ExpressionStatement(node)
   return self:emit(node.expression)
end

function Stat:Local(node)
   local nvars = #node[1]
   local nexps = #node[2]
   local base = self.ctx.freereg
   local slots = nvars
   for i = 1, nexps - 1 do
      if slots == 0 then
         break
      end
      self:expr_tonextreg(node[2][i])
      slots = slots - 1
   end

   if slots > 0 then
      if nexps > 0 then
         self:expr_tomultireg(node[2][nexps], slots)
      else
         self.ctx:op_nils(base, slots)
      end
      self.ctx:nextreg(slots)
   end

   for i = 1, nvars do
      local lhs = node[1][i]
      self.ctx:newvar(lhs[1], base + (i - 1))
   end
end

-- Eliminate write-after-read hazards for local variable assignment.
-- Implement the same approach found in lj_parse.c from luajit.
-- Check left-hand side against variable register "reg".
local function assign_hazard(self, lhs, reg)
   local tmp = self.ctx.freereg -- Rename to this temp. register (if needed).
   local hazard = false
   for i = #lhs, 1, -1 do
      if lhs[i].tag == "member" then
         if lhs[i].target == reg then -- t[i], t = 1, 2
            hazard = true
            lhs[i].target = tmp
         end
         if lhs[i].key_type == "V" and lhs[i].key == reg then -- t[i], i = 1, 2
            hazard = true
            lhs[i].key = tmp
         end
      end
   end
   if hazard then
      self.ctx:nextreg()
      self.ctx:op_move(tmp, reg)
   end
end

function Stat:Set(node)
   local free = self.ctx.freereg
   local nvars = #node[1]
   local nexps = #node[2]

   local lhs = {}
   for i = 1, nvars do
      local va = self:lhs_expr_emit(node[1][i])
      if va.tag == "local" then
         assign_hazard(self, lhs, va.target)
      end
      lhs[i] = va
   end

   local slots = nvars
   local exprs = {}
   for i = 1, nexps - 1 do
      if slots == 0 then
         break
      end
      -- LuaJIT compatibility:
      -- Use a temporary register even the LHS is not an immediate local
      -- variable.
      local use_reg = true
      -- local use_reg = is_local_var(self.ctx, node[1][i])
      if use_reg then
         exprs[i] = self:expr_tonextreg(node[2][i])
      else
         exprs[i] = self:expr_toanyreg(node[2][i])
      end
      slots = slots - 1
   end
   local i = nexps
   if slots == 1 then
      -- Case where (nb of expression) >= (nb of variables).
      self:expr_tolhs(lhs[i], node[2][i])
   else
      -- Case where (nb of expression) < (nb of variables). In this case
      -- we cosider that the last expression can generate multiple values.
      local exp_base = self.ctx.freereg
      self:expr_tomultireg(node[2][i], slots)
      for k = slots - 1, 0, -1 do
         self:assign(lhs[i + k], exp_base + k)
      end
   end

   for i = nvars - slots, 1, -1 do
      self:assign(lhs[i], exprs[i])
   end

   self.ctx.freereg = free
end

function Stat:While(node)
   local base_register = self.ctx.freereg
   local loop_begin_location, loop_exit_location = genid(), genid()
   self:loop_enter(loop_exit_location, base_register)
   self.ctx:here(loop_begin_location)
   self:test_emit(node[1], loop_exit_location, base_register)
   self.ctx:loop(loop_exit_location)
   self:block_emit(node[2])
   self.ctx:scope_jump(loop_begin_location, base_register, self.ctx.scope.need_uclo)
   self.ctx:here(loop_exit_location)
   self.ctx:fscope_end()
   self.ctx:leave()
   if node.lineinfo.last.line then
      self.ctx:line(node.lineinfo.last.line)
   end
   self.ctx.freereg = base_register
end

function Stat:Repeat(node)
   local base_register = self.ctx.freereg
   local loop_begin_location, loop_exit_location = genid(), genid()
   local loop_uclo_location = genid()
   self:loop_enter(loop_exit_location, base_register)
   self.ctx:here(loop_begin_location)
   self.ctx:loop(loop_exit_location)
   self:block_emit(node[1])
   local need_body_uclo = self.ctx.scope.need_uclo
   if need_body_uclo then
      self:test_emit(node[2], loop_uclo_location, self.ctx.freereg)
   else
      self:test_emit(node[2], loop_begin_location, base_register)
   end
   if need_body_uclo then
      self.ctx:scope_jump(loop_exit_location, base_register, true)
      self.ctx:here(loop_uclo_location)
      self.ctx:scope_jump(loop_begin_location, base_register, true)
   else
      self.ctx:close_block(self.ctx.scope.basereg)
   end
   self.ctx:here(loop_exit_location)
   self.ctx:fscope_end()
   self.ctx:leave()
   if node.lineinfo.last.line then
      self.ctx:line(node.lineinfo.last.line)
   end
   self.ctx.freereg = base_register
end

function Stat:BreakStatement()
   local base, exit, need_uclo = self.ctx:current_loop()
   self.ctx:scope_jump(exit, base, need_uclo)
   self.ctx.scope.need_uclo = false
end

function Stat:Fornum(node)
   local free = self.ctx.freereg
   local exit = genid()
   local name = node[1][1]
   -- local line = node.line

   self:expr_tonextreg(node[2])
   self:expr_tonextreg(node[3])
   if #node == 5 then
      self:expr_tonextreg(node[4])
   else
      self.ctx:op_load(self.ctx.freereg, 1)
      self.ctx:nextreg()
   end
   local forivinfo = self.ctx:forivars(0x01)
   local loop = self.ctx:op_fori(free)
   self:loop_enter(exit, free)
   self.ctx:newvar(name)
   self:block_enter()
   self:block_emit(node[#node])
   self:block_leave()
   self:loop_leave(node[#node].lineinfo.last.line)
   self.ctx:op_forl(free, loop)
   self.ctx:setpcline(line)
   forivinfo.endpc = #self.ctx.code
   self.ctx:here(exit)
   self.ctx.freereg = free
end

function Stat:Forin(node)
   local free = self.ctx.freereg
   local iter = free + 3
   -- local line = node.line

   local loop, exit = genid(), genid()

   local vars = node[1]
   local iter_list = node[2]

   local iter_count = 0
   -- ???
   for i = 1, #iter_list - 1 do
      self:expr_tonextreg(iter_list[i])
      iter_count = iter_count + 1
      if iter_count == 2 then
         break
      end
   end
   self:expr_tomultireg(iter_list[1], 3 - iter_count) -- func, state, ctl
   self.ctx:setreg(iter)
   local forivinfo = self.ctx:forivars(0x04)
   self.ctx:jump(loop, self.ctx.freereg)

   self:loop_enter(exit, free)

   for i = 1, #vars do
      local name = vars[i][1]
      self.ctx:newvar(name, iter + i - 1)
      self.ctx:setreg(iter + i)
   end

   local ltop = self.ctx:here(genid())
   self:block_emit(node[#node])
   self:loop_leave(node.lineinfo.last.line)
   self.ctx:here(loop)
   self.ctx:op_iterc(iter, #vars)
   self.ctx:setpcline(line)
   self.ctx:op_iterl(iter, ltop)
   self.ctx:setpcline(line)
   forivinfo.endpc = #self.ctx.code
   self.ctx:here(exit)
   self.ctx.freereg = free
end

function Stat:Return(node)
   local narg = #node
   -- local narg = #node
   local local_var = narg == 1 and is_local_var(self.ctx, node[1])
   if narg == 0 then
      self.ctx:close_uvals()
      self.ctx:op_ret0()
   elseif local_var then
      self.ctx:close_uvals()
      self.ctx:op_ret1(local_var)
   else
      local base = self.ctx.freereg
      for i = 1, narg - 1 do
         self:expr_tonextreg(node[i])
      end
      local lastarg = node[narg]
      local request_tcall = (narg == 1)
      local mret, tail = self:expr_tomultireg(lastarg, MULTIRES, request_tcall)
      self.ctx.freereg = base
      if not tail then
         self.ctx:close_uvals()
         if mret then
            self.ctx:op_retm(base, narg - 1)
         elseif narg == 1 then
            self.ctx:op_ret1(base)
         else
            self.ctx:op_ret(base, narg)
         end
      end
   end
   if self.ctx:is_root_scope() then
      self.ctx.explret = true
   end
end

function Stat:Chunk(node, name)
   self:block_emit(node)
   self:close_proto()
end

local function generate(tree, name)
   local self = { line = 0 }
   local first, last = tree.lineinfo and tree.lineinfo.first.line or 1, tree.lineinfo and tree.lineinfo.last.line or 1
   self.main = bc.Proto.new(bc.Proto.VARARG, first, last)
   -- self.main = bc.Proto.new(bc.Proto.VARARG, 1, 1)
   self.ctx = self.main
   self.chunkname = tree.chunkname

   -- for metalua
   tree.tag = "Chunk"

   function self:block_enter()
      self.ctx:enter()
   end

   function self:block_leave(lastline, exit)
      self.ctx:fscope_end()
      self.ctx:close_block(self.ctx.scope.basereg, exit)
      self.ctx:leave()
      if lastline then
         self.ctx:line(lastline)
      end
   end

   function self:loop_enter(exit, exit_reg)
      self:block_enter()
      self.ctx:loop_register(exit, exit_reg)
   end

   function self:loop_leave(lastline)
      self:block_leave(lastline)
   end

   function self:assign(lhs, expr)
      local saveline = self.ctx.currline
      self.ctx:line(lhs.line)
      if lhs.tag == "member" then
         -- SET instructions with a Primitive "P" index are not accepted.
         -- The method self:lhs_expr_emit does never generate such requests.
         assert(lhs.key_type ~= "P", "invalid assignment instruction")
         self.ctx:op_tset(lhs.target, lhs.key_type, lhs.key, expr)
      elseif lhs.tag == "upval" then
         self.ctx:op_uset(lhs.uv, "V", expr)
      elseif lhs.tag == "local" then
         mov_toreg(self.ctx, lhs.target, expr)
      else
         self.ctx:op_gset(expr, lhs.name)
      end
      self.ctx:line(saveline)
   end

   function self:emit(node, ...)
      -- if node.line then
      --    self.ctx:line(node.line)
      -- end
      local rule = Stat[node.tag] -- HACK: ?
      if not rule then
         error("cannot find a statement rule for " .. node.tag)
      end
      rule(self, node, ...)
   end

   function self:block_emit(stmts, if_exit)
      local n = #stmts
      for i = 1, n - 1 do
         self:emit(stmts[i])
      end
      if n > 0 then
         self:emit(stmts[n], if_exit)
      end
   end

   -- Emit the code to evaluate "node" and perform a conditional
   -- jump based on its value.
   -- The arguments "jmp" and "jreg" are respectively the jump location
   -- and the rbase operand for the JMP operation if the store is performed.
   -- When no store is done JMP will use "freereg" as rbase operand.
   -- If "negate" is false the jump on FALSE and viceversa.
   -- The argument "store" is a bitfield that specifies which
   -- computed epxression should be stored. The bit EXPR_RESULT_TRUE
   -- means that the value should be stored when its value is "true".
   -- If "store" is not ZERO than dest should be the register
   -- destination for the result.
   function self:test_emit(node, jmp, jreg, negate, store, dest)
      if node.line then
         self.ctx:line(node.line)
      end
      local rule = Test[node.tag]
      store = store or 0
      if rule then
         rule(self, node, jmp, jreg, negate, store, dest)
      else
         self:expr_test(node, jmp, jreg, negate, store, dest)
      end
   end

   -- Emit code to test an expression as a boolean value
   function self:expr_test(node, jmp, jreg, negate, store, dest)
      local free = self.ctx.freereg
      local const_val = boolean_const_eval(node)
      if const_val ~= nil then
         if bit.band(store, store_bit(const_val)) ~= 0 then
            self.ctx:op_load(dest, const_val)
         end
         if xor(negate, not const_val) then
            self.ctx:jump(jmp, jreg)
         end
      else
         local expr = self:expr_toanyreg(node)
         if store ~= 0 then
            cond_mov_toreg(self.ctx, negate, dest, expr, jmp, self.ctx.freereg)
         else
            self.ctx:op_test(negate, expr, jmp, self.ctx.freereg)
         end
      end
      self.ctx.freereg = free
   end

   -- Emit code to compute the "node" expression in any register. Return
   -- the register itself and an optional boolean value to indicate if a
   -- tail call was used.
   -- If a new register is needed to store the results one is automatically
   -- allocated and marked as used.
   function self:expr_toanyreg(node, tail)
      local localvar = is_local_var(self.ctx, node)
      if localvar then
         return localvar, false
      else
         local dest = self.ctx.freereg
         local tailcall = self:expr_toreg(node, dest, dest + 1, tail)
         return self.ctx:nextreg(), tailcall
      end
   end

   -- Emit code to compute the "node" expression by storing the result in
   -- the given register "dest". The argument "jreg" indicate the next free
   -- register to jump in for "test_emit" call (logical expressions).
   -- The function does return an optional boolean value to indicate if
   -- a tail call was actually used.
   -- This function always leave the freereg counter to its initial value.
   function self:expr_toreg(node, dest, jreg, tail)
      -- HACK:
      -- if node.line then
      --    self.ctx:line(node.line)
      -- end
      local const_val = const_eval(node)
      if const_val then
         self.ctx:op_load(dest, const_val)
      else
         local rule = Expr[node.tag]
         if rule then
            rule(self, node, dest, jreg or self.ctx.freereg)
         elseif MultiExprRule[node.tag] then
            rule = MultiExprRule[node.tag]
            local base = self.ctx.freereg
            local mres, tailcall = rule(self, node, 1, base == dest and tail)
            mov_toreg(self.ctx, dest, base)
            return tailcall
         else
            error("Cannot find an ExpressionRule for " .. node.tag)
         end
      end
      return false -- no tail call
   end

   -- Emit code to compute the "node" expression in the next available register
   -- and increment afterward the free register counter.
   -- It does call "expr_toreg" with (dest + 1) as "jreg" argument to inform
   -- an eventual "test_emit" call that the next free register after the expression
   -- store is (dest + 1).
   function self:expr_tonextreg(node)
      local dest = self.ctx.freereg
      self:expr_toreg(node, dest, dest + 1)
      self.ctx:setreg(dest + 1)
      return dest
   end

   -- Generate the code to store multiple values in consecutive registers
   -- starting from the current "freereg". The argument "want" indicate
   -- how many values should be generated or MULTIRES.
   -- The optional boolean parameter "tail" indicate if a tail call instruction
   -- should be generated if possible.
   -- Return two boolean values. The first indicate if it does return multi
   -- results. The second if a tail call was actually generated.
   function self:expr_tomultireg(node, want, tail)
      -- HACK:
      -- if node.line then
      --    self.ctx:line(node.line)
      -- end
      local rule = MultiExprRule[node.tag]
      if rule then
         return rule(self, node, want, tail)
      elseif want > 0 or want == MULTIRES then
         local dest = self.ctx.freereg
         self:expr_toreg(node, dest, dest + 1)
         self.ctx:maxframe(dest + 1)
         if want > 1 then
            self.ctx:op_nils(dest + 1, want - 1)
            self.ctx:maxframe(dest + want)
         end
         return false, false
      end
   end

   -- Like "expr_toreg" but it can return an expression (register) or
   -- an immediate constant. It does return a tag and then the value
   -- itself.
   function self:expr_toanyreg_tagged(node, emit)
      local const_val = const_eval(node)
      if emit.byte and const_val and is_byte_number(const_val) then
         return "B", const_val
      elseif emit.number and const_val then
         return "N", self.ctx:const(const_val)
      end
      -- TODO:
      if node.tag == "Literal" then
         local value = node.value
         local tv = type(value)
         if emit.primitive and (tv == "nil" or tv == "boolean") then
            return "P", self.ctx:kpri(value)
         elseif emit.string and tv == "string" then
            return self:property_tagged(value)
         end
         -- fall through
      end
      return "V", self:expr_toanyreg(node)
   end

   function self:property_tagged(property_name)
      local kprop = self.ctx:const(property_name)
      if kprop < 255 then
         return "S", kprop
      else
         local prop = self.ctx:nextreg()
         self.ctx:op_load(prop, property_name)
         return "V", prop
      end
   end

   -- Emit code to store an expression in the given LHS.
   function self:expr_tolhs(lhs, expr)
      local free = self.ctx.freereg
      if lhs.tag == "upval" then
         local tag, expr = self:expr_toanyreg_tagged(expr, EXPR_EMIT_VSNP)
         self.ctx:op_uset(lhs.uv, tag, expr)
         self.ctx:setpcline(lhs.line)
      elseif lhs.tag == "local" then
         self:expr_toreg(expr, lhs.target)
      else
         local reg = self:expr_toanyreg(expr)
         self:assign(lhs, reg)
      end
      self.ctx.freereg = free
   end

   function self:lhs_expr_emit(node)
      local line = self.ctx.currline
      local rule = assert(Lhs[node.tag], 'undefined assignment rule for node type: "' .. node.tag .. '"')
      local lhs = rule(self, node)
      -- TODO:
      lhs.line = line
      return lhs
   end

   function self:close_proto(lastline)
      if lastline then
         self.ctx:line(lastline)
      end
      local err, line = self.ctx:close_proto()
      if err then
         lang_error(err, self.chunkname, line)
      end
   end

   self:emit(tree)

   local dump = bc.Dump.new(self.main, name)
   return dump:pack()
end

return generate
