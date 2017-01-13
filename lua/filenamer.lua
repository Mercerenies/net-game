
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
   for i = 0, n do
      local name = generate_name(i)
      os.remove(name)
   end
   util.execute 'kill -TERM -- -$(ps | awk \'$1 ~ /\\<\'"$$"\'\\>/ { print $3; }\')'
end

return P
