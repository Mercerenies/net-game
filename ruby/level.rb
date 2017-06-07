
# Levels classify the Node structures into layers, indexed by their "size".
class Level

  # The individual layer is used for nodes which should translate to individual positions in the
  # final map. Individual nodes have no children.
  def self.individual
    @@individual ||= LevelZero.new
  end

  # The city node layer is used for nodes which contain small collections of individual nodes. It
  # is a similar but smaller scale version of the district node layer.
  def self.city
    @@city ||= LevelOne.new 1..6
  end

  # The district node layer is used for larger collections of individual nodes than the city node
  # layer.
  def self.district
    @@district ||= LevelOne.new 4..8
  end

  # The state node layer is for small collections of cities and districts.
  def self.state
    @@state ||= LevelTwo.new 2..4
  end

  # The country node layer is for large collections of cities and districts.
  def self.country
    @@country ||= LevelTwo.new 4..8
  end

  # The toplevel layer occupies the topmost position on the world map. It is not designed to
  # belong to another node and should only be used at the topmost level. Note that the toplevel
  # layer instance is a LevelZero layer type, so it should not be passed into Node#waterfall.
  def self.top
    self.individual
  end

  def self.[](*args)
    self.new(*args)
  end

  def initialize(num)
    @number = num
  end

  def to_i
    @number
  end

  def to_int
    @number
  end

  # Creates a list of children to the current level. If the node is intended as a topmost or bottommost
  # node, the empty list is returned. Otherwise, the size of the layer is sampled and an appropriate
  # collection of child layers is returned.
  def make_children
    []
  end

  # Returns the logical next level up from the current layer type. That is, this method returns the
  # node layer type that would logically contain the given layer.
  def level_up
    nil
  end

end

# The level zero layer is for nodes which should not automatically produce any children.
class LevelZero < Level

  def initialize
    super 0
  end

  def level_up
    Level.city
  end

  def to_s
    "LevelZero[]"
  end

end

# The level one layer groups LevelZero nodes into collections such as cities and districts.
class LevelOne < Level
  attr_accessor :node_count

  def initialize(node_count)
    super 1
    @node_count = node_count
  end

  #def make_children
  #  count = case @node_count
  #          when Integer then @node_count
  #          when Range then @node_count.to_a.sample
  #          else @node_count.to_i
  #          end
  #  count.times.collect { Level.individual }
  #end

  def level_up
    Level.country
  end

  def to_s
    "LevelOne[#{@node_count}]"
  end

end

# The level two layer groups LevelOne cities and districts into states and countries.
class LevelTwo < Level
  attr_accessor :node_count

  def initialize(node_count)
    super 2
    @node_count = node_count
  end

  def make_children
    count = case @node_count
            when Integer then @node_count
            when Range then @node_count.to_a.sample
            else @node_count.to_i
            end
    count.times.collect { [Level.city, Level.district].sample }
  end

  def level_up
    nil
  end

  def to_s
    "LevelTwo[#{@node_count}]"
  end

end

