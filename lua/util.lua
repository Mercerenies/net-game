
local P = {}
util = P
setmetatable(P, {__index = _G})

local pid_cache = nil
local ppid_cache = nil
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
   ppid_cache = f:read("*number") -- Read the PPID
   pgid_cache = f:read("*number") -- Read the PGID
   io.close(f)
end

function P.pid()
   if pid_cache == nil then
      load_caches()
   end
   return pid_cache
end

function P.ppid()
   if ppid_cache == nil then
      load_caches()
   end
   return ppid_cache
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

--[[
 - execute{
 -  command    - Required; the command to execute
 -  func       - Optional; the function to use to execute it
 -  quiet      - Optional; whether or not to use quiet mode
 -  background - Optional; whether or not to run the process in the background
 - }
--]]
function P.execute(arg)
   local cmd = arg.command
   local func = arg.func
   local quiet = arg.quiet
   local back = arg.background
   if quiet then
      logger.echo(3, "Running: " .. cmd)
   else
      logger.echo(1, "Running: " .. cmd)
   end
   if not func then
      func = os.execute
   end
   if back then
      cmd = P.background(cmd)
   end
   return func(cmd)
end

return P
