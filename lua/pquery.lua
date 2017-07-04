
local P = {}
pquery = P
setmetatable(P, {__index = _G})

P.PQuery = {}
local PQuery = P.PQuery

setmetatable(P.PQuery, {__index = query.Query})

local cmd_opt_table = {
   ['-c'] = 'celeb',
   ['-p'] = 'person',
   ['-P'] = 'place',
   ['-w'] = 'weapon',
   ['-m'] = 'monster',
   ['-a'] = 'animal',
   ['-f'] = 'food'
}

local para_cmd =
   './bash/stage1.sh -d %d -e \'legacy-crawl args: [%s %s %s]\' | ' ..
   './bash/stage2.sh %d >%s'

local using_cmd =
   '"crawl type: %s base: * count: %d rein: %s"' -- TODO Proper escaping

local full_cmd =
   './bash/stage1.sh -d %d -e %s | ./bash/stage2.sh %d >%s'

local rein = ""

local function option_to_symbol(opt)
   return cmd_opt_table[opt]
end

local function spawn_parallel(argument, count)
   local rname = filenamer.get_filename()
   local lvl = logger.get_debug_level()
   local reinf = rein == "-r" and 'yes' or 'no'
   local expr = string.format(using_cmd, option_to_symbol(argument), count, reinf)
   local cmd = string.format(full_cmd, lvl, expr, lvl, rname)
   local tsk = task.Task.new(cmd)
   return rname, tsk
end

local function spawn_and_store(qobj, argm, count)
   local rname, tsk = spawn_parallel(argm, count)
   table.insert(qobj._resultnames, rname)
   table.insert(qobj._tasks, tsk)
end

local function spawn_parallel_full(expr)
   local rname = filenamer.get_filename()
   local lvl = logger.get_debug_level()
   expr = expr:gsub([[']], [['"'"']])
   expr = "'" .. expr .. "'"
   -- logger.echo(1, "*** " .. expr .. " ***")
   local cmd = string.format(full_cmd, lvl, expr, lvl, rname, sname)
   local tsk = task.Task.new(cmd)
   return rname, tsk
end

local function spawn_and_store_full(qobj, expr)
   local rname, tsk = spawn_parallel_full(expr)
   table.insert(qobj._resultnames, rname)
   table.insert(qobj._tasks, tsk)
end

function P.use_reinforcement()
   rein = "-r"
end

function P.is_using_reinforcement()
   return rein == "-r"
end

function PQuery.new()
   local new = query.Query.new()
   setmetatable(new, {__index = PQuery})
   new.customs = {}
   return new
end

function PQuery:req()
   self._resultnames = {}
   self._tasks = {}
   for i = 1, (self._people or 0) do
      spawn_and_store(self, '-p', 1)
   end
   for i = 1, (self._celebs or 0) do
      spawn_and_store(self, '-c', 1)
   end
   for i = 1, (self._places or 0) do
      spawn_and_store(self, '-P', 1)
   end
   for i = 1, (self._weapons or 0) do
      spawn_and_store(self, '-w', 1)
   end
   for i = 1, (self._monsters or 0) do
      spawn_and_store(self, '-m', 1)
   end
   for i = 1, (self._animals or 0) do
      spawn_and_store(self, '-a', 1)
   end
   for i = 1, (self._foods or 0) do
      spawn_and_store(self, '-f', 1)
   end
   for i, v in ipairs(self.customs) do
      spawn_and_store_full(self, v)
   end
   self._process = 1
   return self
end

function PQuery:custom(expr)
   self.customs[#self.customs + 1] = expr
   return self
end

function PQuery:ter1()
   local okay = true
   for i, v in ipairs(self._tasks) do
      if not v:is_completed() then
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
