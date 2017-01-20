
# A GData instance for the DeltaGenner object. While regular GData objects can be used to create AlphaStructure
# instances, DeltaGData objects can create either AlphaStructure objects or DeltaStructure objects, by calling
# the appropriate method.
#
# There are a few important things to note about the delta format as a whole.
# * The map can be modified fairly freely. The documentation of DeltaMap details which parts of the
#   map can be modified.
# * New creatures, spawners, quests, and any global list-based entity can be added and modified freely,
#   but the pre-existing ones shall not be modified.
# * Knowledge base entries can be modified in limited ways. The documentation of DeltaKnowledgeBase details
#   this.
# It is infeasible to check many of these conditions, so it is the programmer's responsibility to modify
# the delta structure responsibly. Failure to satisfy the above rules may result in inaccurate data being
# produced.
class DeltaGData < GData
  include Delta

  def initialize(old_data, everything)
    super everything
    @creatures = old_data.each_creature.to_a
    @new_creatures = CreatureSet.new
    @spawners = old_data.each_spawner.to_a
    @new_spawners = SpawnerSet.new
    @quests = old_data.each_quest.to_a
    @knowledge_base = DeltaKnowledgeBase.new old_data.knowledge_base
    @new_quests = QuestSet.new
    @new_map = DeltaMap.new old_data.map
    @old_key = old_data.file_key
  end

  def map
    @new_map
  end

  def load_creature(elem)
    @new_creatures.load_from_page elem
  end

  def has_creature?
    (not @new_creatures.empty?) or super
  end

  def each_creature(&block)
    if block.nil?
      Enumerator.new do |y|
        super { |obj| y << obj }
        @new_creatures.each { |obj| y << obj }
      end
    else
      super
      @new_creatures.each(&block)
    end
  end

  def has_spawner?
    (not @new_spawners.empty?) or super
  end

  def add_spawners(*elems)
    @new_spawners.push(*elems)
  end

  def each_spawner(&block)
    if block.nil?
      Enumerator.new do |y|
        super { |obj| y << obj }
        @new_spawners.each { |obj| y << obj }
      end
    else
      super
      @new_spawners.each(&block)
    end
  end

  def add_quests(*elems)
    @new_quests.push(*elems)
  end

  def each_quest(&block)
    if block.nil?
      Enumerator.new do |y|
        super { |obj| y << obj }
        @new_quests.each { |obj| y << obj }
      end
    else
      super
      @new_quests.each(&block)
    end
  end

  def select_nodes(&block)
    @new_map.select(&block)
  end

  def file_key
    @old_key + 1
  end

  def result_structure
    creatures = ListLikeChain.new @new_creatures, @creatures
    spawners = ListLikeChain.new @new_spawners, @spawners
    quests = ListLikeChain.new @new_quests, @quests
    AlphaStructure.new map, creatures, spawners, quests, @knowledge_base, file_key, get_meta_data
  end

  # Returns a DeltaStructure object representing the changed properties of the DeltaGData.
  def delta_structure
    DeltaStructure.new map, @new_creatures, @new_spawners, @new_quests, @knowledge_base, file_key
  end

end

class DeltaStructure

  def initialize(map, creatures, spawners, quests, knowledge_base, file_key)
    @map = map
    @creatures = creatures
    @spawners = spawners
    @quests = quests
    @knowledge_base = knowledge_base
    @file_key = file_key
  end

  def to_dsxp
    [:delta, @file_key, @map.to_dsxp, @creatures, @spawners, @quests, @knowledge_base.to_dsxp].to_sxp
  end

end
