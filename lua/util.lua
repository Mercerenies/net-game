
local P = {}
util = P
setmetatable(P, {__index = _G})

function P.exists(fname)
   local f = io.open(fname)
   local exists = false
   if f ~= nil then
      exists = true
      io.close(f)
   end
   return exists
end

return P
