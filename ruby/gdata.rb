
# The persistent data available to the generator. A Genner object will use and modify the GData
# information in order to produce a coherent world. At the end, #result_structure should be called,
# which will yield the resulting data structure in the form of an AlphaStructure.
class GData
  include DeltaAble

  attr_reader :node, :map, :knowledge_base, :file_key

  def initialize(everything)
    @arr = everything.clone
    @node = nil
    @bridges = [] # TODO Store bridges, etc in a metadata part of the system.txt file for later.
    @map = nil
    @creatures = CreatureSet.new
    @spawners = SpawnerSet.new
    @quests = QuestSet.new
    @knowledge_base = KnowledgeBase.new
    @file_key = 1
  end

  def to_delta
    DeltaGData.new self, each.to_a
  end

  # Assigns the singular node.
  def node=(val)
    case val
    when Node, NilClass
      @node = val
    end
  end

  # Checks if the list of bridges is non-empty.
  def has_bridge?
    not @bridges.empty?
  end

  # Gets (and removes) a bridge, if available.
  def get_a_bridge
    if has_bridge?
      result = @bridges.sample
      @bridges = @bridges.delete result
      result
    end
  end

  # Adds bridges to the list of available bridges.
  def add_bridges(*bridges)
    @bridges.push(*bridges)
  end

  # Iterates over each element in the data array.
  def each(&block)
    @arr.each &block
  end

  # Iterates over the data in the array. The block should return
  # truthy if it "consumes" the element, in which case the element
  # will be removed.
  def consume_each(&block)
    @arr = @arr.reject(&block)
  end

  # Loads a creature into the CreatureSet, using load_from_page.
  def load_creature(elem)
    @creatures.load_from_page elem
  end

  # Checks whether we have any creatures.
  def has_creature?
    not @creatures.empty?
  end

  # Iterates over the creatures in the creature set.
  def each_creature(&block)
    @creatures.each(&block)
  end

  # Checks whether we have any spawners.
  def has_spawner?
    not @spawners.empty?
  end

  # Adds one or more spawners to the spawner set.
  def add_spawners(*elems)
    @spawners.push(*elems)
  end

  # Iterates over the spawners in the spawner set.
  def each_spawner(&block)
    @spawners.each(&block)
  end

  # Adds one or more quests to the quest set.
  def add_quests(*elems)
    @quests.push(*elems)
  end

  # Iterates over the quests in the quest set.
  def each_quest(&block)
    @quests.each(&block)
  end

  # Selects all nodes on the map for which the predicate returns truthy.
  def select_nodes(&block)
    @map.select(&block)
  end

  # Converts the node into a map and add the locations to it.
  def node_to_map
    if map.nil?
      @map = Map.new @node.expand_to_map(gdata: self)
    else
      new_map = @node.expand_to_map(existing: map, gdata: self)
      if new_map.size > 1 # TODO Detect trivial integrations in node.rb, not here
        new_map.each { |loc| map.push loc }
      end
    end
  end

  # Returns the generator data in an appropriate output list format.
  def result_structure
    AlphaStructure.new map, @creatures, @spawners, @quests, @knowledge_base, file_key, get_meta_data
  end

  # Returns the metadata object that will be stored with the result structure.
  def get_meta_data
    MetaData.new(:':curr-id' => Node.current_id,
                 :':curr-quest-flag' => QuestMaker.current_quest_flag)
  end

  # JSON-ifies the remaining world data.
  def excess_to_json
    @arr.to_json
  end

  def self.from_sxp(arg)
    fk, map, creatures, spawners, quests, kb, debug, meta = Reloader.assert_first :alpha, arg
    reloader = Reloader.instance
    meta = reloader.load meta
    GData.new([]).tap do |gdata|
      gdata.instance_variable_set :@file_key, fk
      gdata.instance_variable_set :@map, reloader.load(map)
      gdata.instance_variable_set :@creatures, reloader.load(creatures)
      gdata.instance_variable_set :@spawners, reloader.load(spawners)
      gdata.instance_variable_set :@quests, reloader.load(quests)
      gdata.instance_variable_set :@knowledge_base, reloader.load(kb)
      Node.current_id = meta[:':curr-id']
      QuestMaker.current_quest_flag = meta[:':curr-quest-flag']
    end
  end

end

# An immutable structure consisting of data designed to be the root node of an alpha file.
class AlphaStructure

  def initialize(map, creatures, spawners, quests, kb, file_key, meta)
    @map = map
    @creatures = creatures
    @spawners = spawners
    @quests = quests
    @knowledge_base = kb
    @file_key = file_key
    @meta = meta
    # The file key is included as a unique integer to identify which alpha state is associated with which
    # delta state, to eliminate the possibility of integrating two or more delta files in the wrong order.
  end

  def to_sxp
    [:alpha, @file_key, @map, @creatures, @spawners, @quests, @knowledge_base, Logger.instance.debug_level, @meta].to_sxp
  end

end
