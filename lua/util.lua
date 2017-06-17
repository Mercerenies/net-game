
local P = {}
util = P
setmetatable(P, {__index = _G})

local pid_cache = nil
local pgid_cache = nil

function P.exists(fname)
   local f = io.open(fname)
   local exists = false
   if f ~= nil then
      exists = true
      io.close(f)
   end
   return exists
end

local function load_caches()
   local f = assert(io.open('/proc/self/stat', 'r'))
   pid_cache = f:read("*number") -- Read the PID
   while f:read(1) ~= ')' do end -- Ignore the name
   f:read(3) -- Ignore the state
   f:read("*number") -- Ignore the PPID
   pgid_cache = f:read("*number") -- Read the PGID
   io.close(f)
end

function P.pid()
   if pid_cache == nil then
      load_caches()
   end
   return pid_cache
end

function P.pgid()
   if pgid_cache == nil then
      load_caches()
   end
   return pgid_cache
end

function P.background(cmd)
   return "(" .. cmd .. ")&"
end

local function execute_with(func, cmd)
   logger.echo(1, "Running: " .. cmd)
   return func(cmd)
end

function P.execute(cmd)
   execute_with(os.execute, cmd)
end

function P.execute_bg(cmd)
   P.execute(P.background(cmd))
   -- ///// TODO Think about getting the pid from a background job here without race conditions
   -- local file = io.popen("echo $!", 'r')
   -- local pid = file:read("*line")
   -- io.close(file)
   -- return pid
end

function P.execute_stdout(cmd)
   return execute_with(function (c) return io.popen(c, 'r') end, cmd)
end

function P.execute_stdin(cmd)
   return execute_with(function (c) return io.popen(c, 'w') end, cmd)
end

return P
