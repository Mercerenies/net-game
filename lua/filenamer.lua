
-- TODO This is a whole lot of files; we want to delete old files during the program
--      and reuse their filenames so as not to clutter the directory

local P = {}
filenamer = P
setmetatable(P, {__index = _G})

local prefix = 'dat'
local suffix = '.txt'
local n = 0

local function generate_name(i)
   return string.format("./temp/%s%03d%s", prefix, i, suffix)
end

function P.get_filename()
   local name = generate_name(n)
   n = n + 1
   return name
end

function P.cleanup()
   logger.echo(1, "Terminating processes...")
   local pid = util.pid()
   local pgid = util.pgid()
   local ps = 'ps xao pid,ppid,pgid | '
   local awk = 'awk \'($3 ~ /' .. pgid .. '/) && ($1 !~ /' .. pid .. '/) { print $1; }\' | '
   local kill = 'xargs kill -15'
   util.execute { command = ps .. awk .. kill }
   logger.echo(1, "Processes ended.")
   logger.echo(1, "Cleaning up files...")
   for i = 0, n do
      local name = generate_name(i)
      os.remove(name)
   end
   logger.echo(1, "Files cleaned up.")
   logger.echo(1, "Have a nice day.")
end

return P
