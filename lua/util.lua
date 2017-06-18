
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

-- TODO This execute API is now a mess. Let's factor it out into one really big one
--      (i.e. clean up execute_with and make it public)

--[[
 - execute_with{
 -  command - Required; the command to execute
 -  func    - Required; the function to use to execute it
 -  quiet   - Optional; whether or not to use quiet mode
 - }
--]]
local function execute_with(arg)
   local cmd = arg.command
   local func = arg.func
   local quiet = arg.quiet
   if quiet then
      logger.echo(3, "Running: " .. cmd)
   else
      logger.echo(1, "Running: " .. cmd)
   end
   return func(cmd)
end

function P.execute(cmd)
   return execute_with {
      func = os.execute,
      command = cmd
   }
end

function P.execute_quietly(cmd)
   return execute_with {
      func = os.execute,
      command = cmd,
      quiet = true
   }
end

function P.execute_bg(cmd)
   P.execute(P.background(cmd))
end

function P.execute_stdout(cmd)
   return execute_with {
      func = function (c) return io.popen(c, 'r') end,
      command = cmd
   }
end

function P.execute_stdin(cmd)
   return execute_with {
      func = function (c) return io.popen(c, 'w') end,
      command = cmd
   }
end

return P
