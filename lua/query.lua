
local P = {}
query = P
setmetatable(P, {__index = _G})

P.Query = {}
local Query = P.Query

local QueryDispatch = {
   [0] = function (_) end, -- Reading input from Lisp/Lua
   [1] = function (x) x:ter1() end, -- Running Python/Perl to get data
   [2] = function (_) end, -- Waiting on lock for Ruby access
   [3] = function (_) end, -- Running Ruby code
   [4] = function (x) x:ter4() end -- Finished Ruby code
}

function Query.new()
   local new = {_process = 0, _finished = false}
   setmetatable(new, {__index = Query})
   return new
end

function Query:people(n)
   self._people = (self._people or 0) + n
   return self
end

function Query:celebs(n)
   self._celebs = (self._celebs or 0) + n
   return self
end

function Query:places(n)
   self._places = (self._places or 0) + n
   return self
end

function Query:req()
   local fn0 = filenamer.get_filename()
   local fn1 = filenamer.get_filename()
   local cmd = './bash/stage1.sh'
   if self._people then
      cmd = cmd .. ' -p ' .. self._people
   end
   if self._celebs then
      cmd = cmd .. ' -c ' .. self._celebs
   end
   if self._places then
      cmd = cmd .. ' -P ' .. self._places
   end
   cmd = cmd .. ' | ./bash/stage2.sh >' .. fn0 .. ' ; touch ' .. fn1
   cmd = '(' .. cmd .. ')&'
   os.execute(cmd)
   self._resultname = fn0
   self._donename = fn1
   self._process = 1
   return self
end

function Query:ter()
   disp = QueryDispatch[self._process]
   if type(disp) == 'function' then
      disp(self)
   else
      io.stderr:write("Internal error at TER" .. self._process .. "!\n")
   end
end

function Query:ter1()
   local f = io.open(self._donename)
   local exists = false
   if f ~= nil then
      exists = true
      io.close(f)
   end
   if exists then
      gensema.genner_lock(self)
   end
end

function Query:ter4()
   self._finished = true
end

return P
