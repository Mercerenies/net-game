
class DeltaKnowledgeBase < KnowledgeBase
  include Delta

  def initialize(old_kb)
    @old = old_kb.each.collect { |k, dat| [k, DeltaNPCBrain.new(dat)] }.to_h
    @new = {}
  end

  def each(&block)
    if block.nil?
      Enumerator.new do |y|
        @old.each { |obj| y << obj }
        @new.each { |obj| y << obj }
      end
    else
      @old.each(&block)
      @new.each(&block)
    end
  end

  def [](key)
    if @old.include? key.id
      @old[key.id]
    elsif @new.include? key.id
      @new[key.id]
    else
      @new[key.id] = NPCBrain.new key.id, key.name, key.job
      @new[key.id]
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
    super brain.id, brain.name, brain.job
    @old_quests = brain.each.to_a
    @new_quests = []
  end

  def each(&block)
    if block.nil?
      Enumerator.new do |y|
        @old_quests.each { |obj| y << obj }
        @new_quests.each { |obj| y << obj }
      end
    else
      @old_quests.each(&block)
      @new_quests.each(&block)
    end
  end

  def add_quest(q) # Expects a quest identifier
    @new_quests.push q
  end

  def to_dsxp
    ([:'npc-brain'] + @new_quests)
  end

end
