bcread = require("bcread")

local function id(a)
   return a
end

print(bcread.dump(string.dump(id)))
