
class DeltaKnowledgeBase < KnowledgeBase
  include Delta

  def initialize(old_kb)
    @old = old_kb.each.collect { |k, dat| [k, migrate_brain(dat)] }.to_h
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
      @new[key.id] = KnowledgeBase.new_brain key
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

  def to_h
    @old.merge @new
  end

  def to_dsxp
    arr = @old.collect { |k, dat| dat.to_dsxp }
    arr_new = @new.values
    [:'knowledge-base', :':new', arr_new, :':mod', arr]
  end

  # TODO This is a temporary solution; we want to factor this out into a method in the brain object
  def migrate_brain(dat)
    case dat
    when NPCBrain
      DeltaNPCBrain.new(dat)
    when CityBrain
      DeltaCityBrain.new(dat)
    else
      dat
    end
  end

  private :migrate_brain

end

class DeltaNPCBrain < NPCBrain
  include Delta

  def initialize(brain)
    super brain.id, brain.name, brain.job
    @motives = brain.motives
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
    [:'npc-brain', id, :':quests', @new_quests]
  end

end

class DeltaCityBrain < CityBrain
  include Delta

  def initialize(dat)
    @id = dat.id
  end

  def to_dsxp
    [:'city-brain', id]
  end

end
