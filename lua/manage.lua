
local dispatch = {
   quit = function () os.exit(1) end
}

local socket = require 'socket'

local server = socket.tcp()
server:bind('localhost', arg[1])
server:listen()

local conn = assert( server:accept() )
local data
while true do
   data = assert( conn:receive '*l' )
   local tokens = {}
   for elem in data:gmatch '%S+' do
      table.insert(tokens, elem)
   end
   local func = dispatch[ tokens[1] ]
   if type(func) == 'function' then
      func()
   else
      io.stderr:write("WARNING: Unknown message '" .. tokens[1] .. "' received!\n")
   end
end
