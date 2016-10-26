
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

  def result_structure
    creatures = ListLikeChain.new @new_creatures, @creatures
    spawners = ListLikeChain.new @new_spawners, @spawners
    quests = ListLikeChain.new @new_quests, @quests
    AlphaStructure.new map, creatures, spawners, quests, @knowledge_base, get_meta_data
  end

  def delta_structure
    # A couple of notes about the delta system and what is/isn't read-only.
    #  - The map can be modified, for the most part. Check DeltaMap's documentation
    #    for specifics.
    #  - New creatures, spawners, and quests can be added and modified freely, but
    #    the pre-existing ones should be left unmodified.
    # These conditions are NOT checked; if you violate them, a potentially
    # inaccurate delta file will be produced.
    DeltaStructure.new map, @new_creatures, @new_spawners, @new_quests, @knowledge_base
  end

end

class DeltaStructure

  def initialize(map, creatures, spawners, quests, knowledge_base)
    @map = map
    @creatures = creatures
    @spawners = spawners
    @quests = quests
    @knowledge_base = knowledge_base
  end

  def to_dsxp
    [:delta, @map.to_dsxp, @creatures, @spawners, @quests, @knowledge_base.to_dsxp].to_sxp
  end

end
