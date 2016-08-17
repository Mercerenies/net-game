
class GData
  attr_accessor :nodes, :bridges, :map, :creatures, :spawners, :quests

  def initialize(everything)
    @arr = everything.clone
    @nodes = []
    @bridges = []
    @map = nil
    @creatures = CreatureSet.new
    @spawners = SpawnerSet.new
    @quests = QuestSet.new
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

  # Checks whether we have any spawners
  def has_spawner?
    not @spawners.empty?
  end

  # Select all nodes on the map for which the predicate returns truthy
  def select_nodes(&block)
    @map.select(&block)
  end

  # Return the generator data in an appropriate output list format
  def result_structure
    [@map, @creatures, @spawners, @quests]
  end

end
