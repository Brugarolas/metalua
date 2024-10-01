--*-lua-*--
rockspec_format = "3.0"
package = "metalua"
version = "0.7.2-1"
source = {
   url = "git://git.eclipse.org/gitroot/koneki/org.eclipse.koneki.metalua.git",
   -- tag = "v0.7.2",
}

description = {
   summary = "Metalua's compiler: converting (Meta)lua source strings and files into executable Lua 5.1 bytecode",
   detailed = [[
           This is the Metalua copmiler, packaged as a rock, depending
           on the spearate metalua-parser AST generating library. It
           compiles a superset of Lua 5.1 into bytecode, which can
           then be loaded and executed by a Lua 5.1 VM. It also allows
           to dump ASTs back into Lua source files.
   ]],
   homepage = "http://git.eclipse.org/c/koneki/org.eclipse.koneki.metalua.git",
   license = "EPL + MIT",
}

dependencies = {
   "lua ~> 5.1", -- Lua 5.2 bytecode not supported
   "checks >= 1.0", -- Argument type checking
   "luafilesystem >= 1.6.2", -- Cached compilation based on file timestamps
   "readline >= 1.3", -- Better REPL experience
   -- "metalua-parser == 0.7.2", -- AST production
}

deploy = {
   wrap_bin_scripts = false,
}

build = {
   type = "builtin",
   modules = {
      ["metalua"] = "metalua.lua",
      ["metalua.compiler.globals"] = "metalua/compiler/globals.lua",
      ["metalua.compiler.bytecode"] = "metalua/compiler/bytecode.lua",
      ["metalua.compiler.bytecode.compile"] = "metalua/compiler/bytecode/compile.lua",
      ["metalua.compiler.bytecode.lcode"] = "metalua/compiler/bytecode/lcode.lua",
      ["metalua.compiler.bytecode.lopcodes"] = "metalua/compiler/bytecode/lopcodes.lua",
      ["metalua.compiler.bytecode.ldump"] = "metalua/compiler/bytecode/ldump.lua",
      ["metalua.loader"] = "metalua/loader.lua",

      ["metalua.grammar.generator"] = "metalua/grammar/generator.lua",
      ["metalua.grammar.lexer"] = "metalua/grammar/lexer.lua",
      ["metalua.compiler.parser"] = "metalua/compiler/parser.lua",
      ["metalua.compiler.parser.table"] = "metalua/compiler/parser/table.lua",
      ["metalua.compiler.parser.ext"] = "metalua/compiler/parser/ext.lua",
      ["metalua.compiler.parser.stat"] = "metalua/compiler/parser/stat.lua",
      ["metalua.compiler.parser.misc"] = "metalua/compiler/parser/misc.lua",
      ["metalua.compiler.parser.lexer"] = "metalua/compiler/parser/lexer.lua",
      ["metalua.compiler.parser.meta"] = "metalua/compiler/parser/meta.lua",
      ["metalua.compiler.parser.expr"] = "metalua/compiler/parser/expr.lua",
      ["metalua.compiler"] = "metalua/compiler.lua",
      ["metalua.pprint"] = "metalua/pprint.lua",
      ["metalua.treequery"] = "metalua/treequery.lua",
      ["metalua.repl"] = "metalua/repl.lua",

      ["metalua.log"] = "metalua/log.lua",

      ["metalua.compiler.ast_to_src"] = "metalua/compiler/ast_to_src.lua",
      ["metalua.treequery.walk"] = "metalua/treequery/walk.lua",

      ["metalua.jit.compiler"] = "metalua/compiler/jit_bytecode/compiler.lua",
      ["metalua.jit.bytecode"] = "metalua/compiler/jit_bytecode/bytecode.lua",
      ["metalua.jit.bool_const"] = "metalua/compiler/jit_bytecode/ast_boolean_const_eval.lua",
      ["metalua.jit.const"] = "metalua/compiler/jit_bytecode/ast_const_eval.lua",
   },
   install = {
      lua = {
         -- ["metalua.compiler.ast_to_src"] = "metalua/compiler/ast_to_src.mlua",
         ["metalua.extension.match"] = "metalua/extension/match.mlua",
         ["metalua.extension.comprehension"] = "metalua/extension/comprehension.mlua",
         ["metalua.extension.macro"] = "metalua/extension/macro.mlua",
         -- ["metalua.extension.goto"] = "metalua/extension/goto.mlua",
         -- ["metalua.extension.types"] = "metalua/extension/types.mlua",
         ["metalua.extension.optional"] = "metalua/extension/optional.mlua",
         ["metalua.extension.comptime"] = "metalua/extension/comptime.mlua",
         -- ["metalua.treequery.walk"] = "metalua/treequery/walk.mlua",
         -- ["metalua"] = "metalua.lua",
      },
      bin = {
         ["metalua"] = "bin/metalua",
         ["ml52"] = "bin/ml52",
         ["mlj"] = "bin/metaj",
      },
   },
}

--[==[-- Generate file lists
for _, ext in ipairs{ 'lua', 'mlua' } do
    for filename in io.popen("find metalua -name '*."..ext.."'") :lines() do
        local modname = filename :gsub ('/', '.') :gsub ('%.'..ext..'$', '')
        print((' '):rep(8)..'["' .. modname .. '"] = "' ..  filename .. '",')
    end
    print""
end
--]==]
--
