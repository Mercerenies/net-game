
local P = {}
util = P
setmetatable(P, {__index = _G})

local pid_cache = nil

function P.exists(fname)
   local f = io.open(fname)
   local exists = false
   if f ~= nil then
      exists = true
      io.close(f)
   end
   return exists
end

function P.pid()
   if pid_cache == nil then
      local f = assert(io.open('/proc/self/stat', 'r'))
      pid_cache = f:read("*number")
      io.close(f)
   end
   return pid_cache
end

return P
