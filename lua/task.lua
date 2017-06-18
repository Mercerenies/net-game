
local P = {}
task = P
setmetatable(P, {__index = _G})

P.Task = {}
local Task = P.Task

function Task.new(cmd)
   local new = {_pid = ''}
   setmetatable(new, {__index = Task})
   local cmd1 = util.background(cmd) .. " echo $!"
   local file = util.execute_stdout(cmd1)
   new._pid = file:read("*line")
   file:close()
   return new
end

function Task:pid()
   return self._pid
end

function Task:is_completed()
   local code = util.execute_quietly("kill -0 " .. self:pid() .. " >/dev/null 2>/dev/null")
   return (code ~= 0)
end

return P
