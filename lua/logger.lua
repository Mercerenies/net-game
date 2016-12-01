
local P = {}
logger = P
setmetatable(P, {__index = _G})

local debug_level = 0

function P.echo(level, str)
   if debug_level >= level then
      io.stderr:write(util.pid() .. ' [0] ' .. str .. '\n')
   end
end

function P.set_debug_level(level)
   debug_level = level
end

function P.get_debug_level()
   return debug_level
end

return P
