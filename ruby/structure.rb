
require 'forwardable'

# A node in a structure that is being built. When the structure is ready, #to_loc will return a
# location instance that can be placed on the map. The StructureNode class is intended to be
# subclassed and the #expand method overriden to define custom behavior for various structures.
# For this reason, several of the #StructureBuilder methods are redefined in this class to delegate
# to the builder object.
class StructureNode
  extend Forwardable

  attr_reader :id
  attr_accessor :fitness

  def_delegators :@builder, :make_node, :connect, :add_names, :get_a_name, :sample_node, :sample_nodes,
                            :core_name=, :core_name, :each_node, :select_nodes, :each_exit, :country,
                            :core_node=, :core_node

  # Initializes a node with a reference back to its builder instance.
  def initialize(builder, name)
    @builder = builder
    @id = Node.get_id
    @name = name
    @country = nil
    @exit = false
    @creatures = nil
    @plants = nil
    @fitness = Fitness.null
  end

  # Called during recursive expansion of the structure node to define the expansion for the
  # individual node.
  def expand
  end

  # Constructs a #Location instance from the structure node.
  def to_loc
    Location.new(id,
                 @name,
                 country,
                 generic_name: core_name,
                 valid_creatures: @creatures,
                 valid_plants: @plants,
                 fitness: fitness,
                 structure_key: core_node || id)
  end

  # Returns whether the node is intended as an exit.
  def exit?
    @exit
  end

  # Marks the structure node as an exit.
  def mark_as_exit
    @exit = true
  end

end

# A builder which consists of #StructureNode instances. Like #StructureNode, #StructureBuilder is
# intended to be subclassed with #construct overriden to determine how to start the building process.
class StructureBuilder
  attr_accessor :core_name, :country, :core_node

  # Initializes an empty structure.
  def initialize
    @nodes = []
    @connections = []
    @names = []
    @core_name = nil
    @country = nil
    @core_node = nil
  end

  # Adds nodes to the structure.
  def add(*nodes)
    @nodes.push(*nodes)
  end

  # Once a builder is allocated and initialized, #construct is intended to be called. The
  # #construct method should call the other builder methods to initialize the construction
  # and make the first few #StructureNode instances, which will then be automatically expanded
  # by the helper methods here.
  def construct
  end

  # Transforms the structure builder's internal state into a #StructureResult object, consisting
  # of finalized locations and marked exit positions.
  def to_loc
    nodes = @nodes.collect(&:to_loc)
    @connections.each do |n0, n1|
      nodes.find { |n| n.id == n0.id }.add_link n1.id
    end
    exits = @nodes.select(&:exit?)
    # Verify that @exits is a subset of @nodes
    exits = exits.map { |e| nodes.find { |n| n.id == e.id } }
    StructureResult.new nodes, exits
  end

  # Constructs a node of the given type, passing the arguments to the initializer. When using the
  # +StructureBuilder+ interface, calling through to #make_node is preferable to directly calling
  # the constructors.
  #
  # The #make_node method constructs the +StructureNode+ derivative, adds it to the structure
  # builder's state, and then expands the node. The consequences are undefined if this method
  # is called with a type which is not +StructureNode+ itself or a child thereof. If the
  # #core_node of the builder has not yet been set, then it will be set the first time #make_node
  # is called, to the ID value of the resulting node.
  def make_node(type, *args)
    elem = type.new self, *args
    @nodes << elem
    self.core_node ||= elem.id
    elem.expand
    elem
  end

  # Connects the two nodes in the structure. Using this method, the exits to the node are added
  # symmetrically, creating a two-way link.
  def connect(node0, node1)
    @connections << [node0, node1] unless @connections.include? [node0, node1]
    @connections << [node1, node0] unless @connections.include? [node1, node0]
  end

  # Adds names to the builder's internal name data. These names are not automatically used; they
  # are retrieved only when #get_a_name is called.
  def add_names(*names)
    @names.push(*names)
  end

  # Retrieves and removes a name from the builder's name data, or +nil+ if there are no names left.
  def get_a_name
    @names.sample.tap { |n| @names.delete n }
  end

  # Returns a random node from the builder's state. This method does not remove the node.
  def sample_node
    @nodes.sample
  end

  # Returns a collection of random nodes of the given size from the builder's state. Nodes will not be
  # repeated in the returned list. If the number requested exceeds the number of available nodes,
  # a list containing all of the nodes is returned.
  def sample_nodes(n)
    if n >= @nodes.length
      @nodes.dup
    else
      result = []
      while result.length < n
        node = sample_node
        result << node unless result.include? node
      end
      result
    end
  end

  # Iterates over the nodes in the builder.
  def each_node(&block)
    @nodes.each &block
  end

  # Filters the nodes in the builder, returning an enumerable.
  def select_nodes(&block)
    each_node.select &block
  end

  # Iterates over the nodes in the builder which have been marked as exits.
  def each_exit(&block)
    @nodes.select(&:exit?).each &block
  end

end

# A simple structure which is returned when a #StructureBuilder finishes constructing its data.
class StructureResult
  attr_reader :nodes, :exits

  def initialize(nodes, exits)
    @nodes = nodes.to_a
    @exits = exits.to_a
  end

end
