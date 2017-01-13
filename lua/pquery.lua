
local P = {}
pquery = P
setmetatable(P, {__index = _G})

P.PQuery = {}
local PQuery = P.PQuery

setmetatable(P.PQuery, {__index = query.Query})

local para_cmd = '(./bash/stage1.sh %s -d %d %s | ./bash/stage2.sh %d >%s ; touch %s)&'

local rein = ""

local function spawn_parallel(argument)
   local rname = filenamer.get_filename()
   local sname = filenamer.get_filename()
   local lvl = logger.get_debug_level()
   local cmd = string.format(para_cmd, rein, lvl, argument, lvl, rname, sname)
   util.execute(cmd)
   return rname, sname
end

local function spawn_and_store(qobj, argm)
   local rname, sname = spawn_parallel(argm)
   table.insert(qobj._resultnames, rname)
   table.insert(qobj._checknames, sname)
end

function P.use_reinforcement()
   rein = "-r"
end

function PQuery.new()
   local new = query.Query.new()
   setmetatable(new, {__index = PQuery})
   return new
end

function PQuery:req()
   self._resultnames = {}
   self._checknames = {}
   for i = 1, (self._people or 0) do
      spawn_and_store(self, '-p 1')
   end
   for i = 1, (self._celebs or 0) do
      spawn_and_store(self, '-c 1')
   end
   for i = 1, (self._places or 0) do
      spawn_and_store(self, '-P 1')
   end
   for i = 1, (self._weapons or 0) do
      spawn_and_store(self, '-w 1')
   end
   for i = 1, (self._monsters or 0) do
      spawn_and_store(self, '-m 1')
   end
   for i = 1, (self._animals or 0) do
      spawn_and_store(self, '-a 1')
   end
   for i = 1, (self._foods or 0) do
      spawn_and_store(self, '-f 1')
   end
   self._process = 1
   return self
end

function PQuery:ter1()
   local okay = true
   for i, v in ipairs(self._checknames) do
      if not util.exists(v) then
         okay = false
      end
   end
   if okay then
      self._gennerparm = ''
      for i, v in ipairs(self._resultnames) do
         self._gennerparm = self._gennerparm .. ' ' .. v
      end
      gensema.genner_lock(self)
   end
end

return P
