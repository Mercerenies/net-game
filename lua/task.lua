
local P = {}
task = P
setmetatable(P, {__index = _G})

P.Task = {}
local Task = P.Task

-- TODO Shall we have the logger report the command w/o "echo $!" since that's kind of redundant
function Task.new(cmd)
   local new = {_pid = ''}
   setmetatable(new, {__index = Task})
   local cmd1 = util.background(cmd) .. " echo $!"
   local file = util.execute {
      command = cmd1,
      func = function (c) return io.popen(c, 'r') end
   }
   new._pid = file:read("*line")
   file:close()
   return new
end

function Task:pid()
   return self._pid
end

-- TODO This spawns a process each time; should we favor luaposix to avoid that?
function Task:is_completed()
   local code = util.execute {
      command = "kill -0 " .. self:pid() .. " >/dev/null 2>/dev/null",
      quiet = true
   }
   return (code ~= 0)
end

return P
