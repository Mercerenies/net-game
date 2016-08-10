
require 'forwardable'

class StructureNode
  extend Forwardable

  attr_reader :id

  def_delegators :@builder, :make_node, :connect, :add_names, :get_a_name, :sample_node, :sample_nodes

  def initialize(builder, name)
    @builder = builder
    @id = Node.get_id
    @name = name
    @exit = false
  end

  def expand
  end

  def to_loc
    Location.new @id, @name, nil
  end

  def exit?
    @exit
  end

  def mark_as_exit
    @exit = true
  end

end

class StructureBuilder

  def initialize
    @nodes = []
    @connections = []
    @names = []
  end

  def add(*nodes)
    @nodes.push(*nodes)
  end

  def construct
  end

  def to_loc
    nodes = @nodes.collect(&:to_loc)
    @connections.each do |n0, n1|
      nodes.find { |n| n.id == n0.id }.add_link n1.id
    end
    exits = @nodes.select(&:exit?)
    exits = exits.map { |e| nodes.find { |n| n.id == e.id } }
    StructureResult.new nodes, exits
  end

  def make_node(type, *args)
    elem = type.new self, *args
    @nodes << elem
    elem.expand
    elem
  end

  def connect(node0, node1)
    @connections << [node0, node1] unless @connections.include? [node0, node1]
    @connections << [node1, node0] unless @connections.include? [node1, node0]
  end

  def add_names(*names)
    @names.push(*names)
  end

  def get_a_name
    @names.sample.tap { |n| @names.delete n }
  end

  def sample_node
    @nodes.sample
  end

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

end

class StructureResult
  attr_reader :nodes, :exits

  def initialize(nodes, exits)
    @nodes = nodes.to_a
    @exits = exits.to_a
  end

end

# ///// Mountains, churches, then other structures and meaning to the nodes
