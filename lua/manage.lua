
local util = require 'lua/util'
local logger = require 'lua/logger'
local gensema = require 'lua/gensema'
local filenamer = require 'lua/filenamer'
local query = require 'lua/query'
local pquery = require 'lua/pquery'

local dispatch = {
   quit = function (_) filenamer.cleanup() os.exit(1) end,
   ter = function (_) checkin() end,
   need = function (x) request(x[2], x[3], x[4]) end
}

local req_objs = {
   quests    = function () return pquery.PQuery.new():people(1):celebs(1) end,
   map       = function () return pquery.PQuery.new():places(3) end,
   wildlife  = function () return pquery.PQuery.new():animals(2) end,
   foliage   = function () return pquery.PQuery.new():foods(2) end,
   equipment = function () return pquery.PQuery.new():weapons(2) end
}

local allqueries = {}

local socket = require 'socket'
local conn = nil

function setup_and_run()

   logger.set_debug_level(tonumber(arg[2]))

   logger.echo(2, "Debugger is running")

   local server = socket.tcp()
   server:bind('localhost', arg[1])
   server:listen()

   conn = assert( server:accept() )
   local data
   while true do
      data = assert( conn:receive '*l' )
      local tokens = {}
      for elem in data:gmatch '%S+' do
         table.insert(tokens, elem)
      end
      local func = dispatch[ tokens[1] ]
      if type(func) == 'function' then
         func(tokens)
      else
         io.stderr:write("WARNING: Unknown message '" .. tokens[1] .. "' received!\n")
      end
   end

end

function checkin()
   -- Check in with the semaphore for the generator
   gensema.check_unlock()
   -- Inform each query that a 'ter' check has occurred
   for _, v in ipairs(allqueries) do
      v:ter()
   end
   -- Send finished query data
   local done = {}
   for i, v in ipairs(allqueries) do
      if v._finished then
         os.execute("touch " .. v._finalname)
         table.insert(done, i)
      end
   end
   -- Remove finished elements from the query list
   for _, j in ipairs(done) do
      table.remove(allqueries, j)
   end
end

function request(type_, wname, dname)
   local curr = req_objs[type_]
   if type(curr) == 'function' then
      local result = curr()
      result._worldname = wname
      result._finalname = dname
      result:req()
      table.insert(allqueries, result)
   else
      io.stderr:write("WARNING: Unknown request type '" .. type_ .. "'!\n")
   end
end

pcall(setup_and_run)
filenamer.cleanup()
