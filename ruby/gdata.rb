
# ///// Continue factoring things into methods; we want to minimize any direct read/write on the fields

class GData
  attr_accessor :spawners, :quests
  attr_reader :node, :map

  def initialize(everything)
    @arr = everything.clone
    @node = nil
    @bridges = [] # TODO Store bridges, etc in a metadata part of the system.txt file for later.
    @map = nil
    @creatures = CreatureSet.new
    @spawners = SpawnerSet.new
    @quests = QuestSet.new
  end

  # Assigns the singular node
  def node=(val)
    case val
    when Node, NilClass
      @node = val
    end
  end

  # Checks if the list of bridges is non-empty
  def has_bridge?
    not @bridges.empty?
  end

  # Gets (and removes) a bridge, if available
  def get_a_bridge
    if has_bridge?
      result = @bridges.sample
      @bridges = @bridges.delete result
      result
    end
  end

  # Adds bridges to the list of available bridges
  def add_bridges(*bridges)
    @bridges.push(*bridges)
  end

  # Iterates over the data in the array. The block should return
  # truthy if it "consumes" the element, in which case the element
  # will be removed
  def consume_each(&block)
    @arr = @arr.reject(&block)
  end

  # Loads a creature into the CreatureSet, using load_from_page
  def load_creature(elem)
    @creatures.load_from_page elem
  end

  # Checks whether we have any creatures
  def has_creature?
    not @creatures.empty?
  end

  # Iterates over the creatures in the creature set
  def each_creature(&block)
    @creatures.each(&block)
  end

  # Checks whether we have any spawners
  def has_spawner?
    not @spawners.empty?
  end

  # Select all nodes on the map for which the predicate returns truthy
  def select_nodes(&block)
    @map.select(&block)
  end

  # Convert the node into a map and add the locations to it
  def node_to_map
    new_map = Map.new @node.expand_to_map(gdata: self)
    if @map.nil?
      @map = new_map
    else
      # TODO Connect the old and the new using connectors
      new_map.each { |loc| @map.push loc }
    end
  end

  # Return the generator data in an appropriate output list format
  def result_structure
    [@map, @creatures, @spawners, @quests]
  end

end
