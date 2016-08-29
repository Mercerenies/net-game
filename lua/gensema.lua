
local P = {}
gensema = P
setmetatable(P, {__index = _G})

local alpha = "./temp/alpha.txt"

local queue = {first = 0, last = 0}
local active = nil

local function enqueue(x)
   -- TODO Should we block nil/false here, since they are returned on error in dequeue()?
   queue[queue.last] = x
   queue.last = queue.last + 1
end

local function dequeue(x)
   if queue.first >= queue.last then
      return nil, "attempted dequeue from empty queue"
   end
   local result = queue[queue.first]
   queue[queue.first] = nil
   queue.first = queue.first + 1
   return result
end

local function peek(x)
   if queue.first >= queue.last then
      return nil, "attempted peek from empty queue"
   end
   return queue[queue.first]
end

local function is_empty_queue()
   return (queue.first >= queue.last)
end

local function run_ruby(query)
   local old_e_fn = query._resultname
   local old_a_fn = alpha
   local new_a_fn = filenamer.get_filename()
   local new_d_fn = query._worldname
   local new_exit_fn = filenamer.get_filename()
   query._donename = new_exit_fn
   query._process = 3
   alpha = new_a_fn
   local cmd = './bash/stage3.sh'
   cmd = cmd .. ' -D ' .. new_d_fn .. ' -0 ' .. old_a_fn
   cmd = cmd .. ' <' .. old_e_fn .. ' >' .. new_a_fn
   cmd = cmd .. ' ; touch ' .. new_exit_fn
   cmd = '(' .. cmd .. ')&'
   os.execute(cmd)
end

function P.genner_lock(query)
   if active then
      query._process = 2
      enqueue(query)
   else
      active = query
      run_ruby(query)
   end
end

function P.check_unlock()
   if active then
      local f = io.open(active._donename)
      local exists = false
      if f ~= nil then
         exists = true
         io.close(f)
      end
      if exists then
         active._process = 4
         active = nil
         if not is_empty_queue() then
            active = assert(dequeue())
            run_ruby(active)
         end
      end
   end
end

return P
