
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

function P.execute(cmd)
   logger.echo(1, "Running: " .. cmd)
   os.execute(cmd)
end

return P
