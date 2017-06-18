
local P = {}
gensema = P
setmetatable(P, {__index = _G})

-- gensema = generator semaphore

local alpha = "./temp/alpha.txt"

local excess = "./temp/excess.txt"

local queue = {first = 0, last = 0}
local active = nil

local function enqueue(x)
   if not x then
      return nil, "attempted enqueue of falsy value"
   end
   queue[queue.last] = x
   queue.last = queue.last + 1
   return x
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
   local old_e_fn = query._gennerparm
   local old_a_fn = alpha
   local new_a_fn = filenamer.get_filename()
   local new_d_fn = query._worldname
   local new_e_fn = filenamer.get_filename()
   local prior_excess = excess
   query._process = 3
   alpha = new_a_fn
   excess = new_e_fn
   local cmd = './bash/stage3.sh'
   cmd = cmd .. ' -d ' .. logger.get_debug_level()
   cmd = cmd .. ' -D ' .. new_d_fn .. ' -0 ' .. old_a_fn .. ' -E ' .. new_e_fn
   cmd = cmd .. ' -- ' .. old_e_fn .. ' ' .. prior_excess .. ' >' .. new_a_fn
   query._world_task = task.Task.new(cmd)
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
      if active._world_task:is_completed() then
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
