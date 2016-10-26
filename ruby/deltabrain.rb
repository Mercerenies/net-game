
class DeltaKnowledgeBase < KnowledgeBase
  include Delta

  def initialize(old_kb)
    @old = old_kb.each.collect { |k, dat| [k, DeltaNPCBrain.new(dat)] }.to_h
    @new = {}
  end

  def each(&block)
    @old.each(&block)
    @new.each(&block)
  end

  def [](key)
    if @old.include? key
      @old[key]
    elsif @new.include? key
      @new[key]
    else
      @new[key] = NPCBrain.new
      @new[key]
    end
  end

  def []=(key, value)
    if @old.include? key
      @old[key] = value
    else
      @new[key] = value
    end
  end

  def to_dsxp
    arr = @old.collect_concat { |k, dat| [k, dat.to_dsxp] }
    arr_new = @new.to_a.flatten 1
    [:'knowledge-base', :':new', arr_new, :':mod', arr]
  end

end

class DeltaNPCBrain < NPCBrain
  include Delta

  def initialize(brain)
    @old_quests = brain.each.to_a
    @new_quests = []
  end

  def each(&block)
    @old.each(&block)
    @new.each(&block)
  end

  def add_quest(q) # Expects a quest identifier
    @new_quests.push q
  end

  def to_dsxp
    ([:'npc-brain'] + @new_quests)
  end

end
