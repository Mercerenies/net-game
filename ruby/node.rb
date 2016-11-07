
require 'forwardable'

# A node is a part of the tree-like hierarchy used in the first stages of the world generation. Nodes
# can be empty or can contain a collection of other "smaller" nodes.
class Node
  extend Forwardable
  include Enumerable

  attr_reader :id, :name, :level, :contents

  def_delegators :@contents, :each, :[]=, :[], :clear, :size, :length, :empty?

  @@curr_id = 1

  # The Node class itself keeps a running ID count, to ensure that node identifiers are unique. This
  # method returns the current identifier without increasing it.
  def self.current_id
    @@curr_id
  end

  # Assigns the current Node identifier. This method should be used very carefully, as incorrect use can
  # invalidate the uniqueness of node identifiers.
  def self.current_id=(val)
    @@curr_id = val.to_i
  end

  # Returns the next Node identifier and increments the internal counter.
  def self.get_id
    @@curr_id += 1
    @@curr_id
  end

  # Constructs a new node of the given name with the given Level layer type.
  def initialize(name, level)
    @id = Node.get_id
    @name = name.to_s
    @level = level
    @contents = []
  end

  # Adds a node as a child.
  def add(x)
    @contents.push x if x.is_a? Node
  end

  # Removes the node from the layer.
  def delete(x)
    @contents.delete x
  end

  # Adds a node as a child.
  def <<(x)
    @contents << x if x.is_a? Node
  end

  def name=(val)
    @name = val.to_s
  end

  # Runs the waterfall algorithm on the node. The waterfall algorithm is designed to take a node with
  # no children but whose Level instance claims that it should have children and "fills in" the node's
  # children with randomly generated nodes, which it then recursively runs the waterfall algorithm on.
  # Any existing children on the node are eliminated when #waterfall is run. 
  def waterfall
    children = @level.make_children
    names = if children.all? { |x| x.is_a? LevelZero }
              AffixSet.instance.sampler @name, children.size
            else
              Namer.instance
            end
    @contents = children.collect { |lvl| Node.new(names.sample, lvl) }
    @contents.each &:waterfall
  end

  # Takes the node and converts it into a collection of real map Location instances. This method will
  # recursively take all of the node's direct and indirect children into consideration as well, so that
  # the LevelZero nodes are converted directly into Location instances while the higher-level nodes are
  # used to augment the map location objects with their names and stats.
  #
  # First, the #expand_to_map method expands any child nodes recursively. Then, it connects each child
  # node's resulting collection with randomized links until the resulting graph is connected. Next,
  # the algorithm determines (based on the size of the node and its immediate children) whether or
  # not a nontrivial bridge is required, implementing one into the nodal structure if necessary.
  # Finally, for sufficiently large regions, a WarpPoint instance is placed randomly.
  #
  # Small nodes will always receive TrivialBridge bridges, which simply connect the nodes together.
  # Larger nodes, such as countries, will recieve nontrivial bridges, either from real world data or
  # using Bridge#create_random to generate a sufficiently large bridge for the situation.
  def expand_to_map(existing: nil, country: nil, gdata:) # TODO Should the country: arg be required?
    if @contents.empty?
      Array[Location.new id, name, country, generic_name: country || "Map"]
    else
      links = @contents.size.times.collect { [] }
      connected = []
      traverse = Proc.new do |n|
        unless connected.include? n
          connected.push n
          links[n].each &traverse
        end
      end
      loop do
        connected = []
        traverse.call 0
        break if connected.size >= links.size
        i = rand links.size
        j = rand links.size
        unless i == j
          links[i].push j
          links[j].push i
        end
      end
      country ||= self.name unless self.name.empty?
      nodes = @contents.map { |obj| obj.expand_to_map(existing: existing, country: country, gdata: gdata) }
      use_real_bridge = (Util.median(nodes.map &:size) > 3)
      bridge_nodes = []
      nodes.zip(links).map.with_index do |arg, n0|
        node, lnks = arg
        lnks.each do |n1|
          if n0 < n1
            if not use_real_bridge
              bridge = TrivialBridge.new
            elsif gdata.has_bridge?
              bridge = gdata.get_a_bridge
            else
              bridge = Bridge.create_random
            end
            bridge.each_node { |bridge_node| bridge_nodes << bridge_node }
            bridge.bridge_on node, nodes[n1]
          end
        end
      end
      nodes = nodes.flatten
      # Bridge to the rest of the map, if needed
      if existing and use_real_bridge
        if gdata.has_bridge?
          bridge = gdata.get_a_bridge
        else
          bridge = Bridge.create_random
        end
        bridge.each_node { |bridge_node| bridge_nodes << bridge_node }
        bridge.bridge_on nodes, existing.to_a
      end
      if @level.kind_of? LevelTwo
        # Put a warp point somewhere on the map, about once per country
        # TODO This distribution is not working; sometimes they end up on the same spot
        somewhere = nodes.sample
        somewhere.push WarpPoint.new
      end
      nodes + bridge_nodes
    end
  end

  def to_s
    "#<Node(#{@id}) \"#{@name}\" #{@level} [#{@contents.join ', '}]>"
  end

  # Pretty-prints the node and its children recursively. Intended for debugging use only.
  def pretty_print(depth = 0)
    depth.times { STDERR.print ' *' }
    STDERR.print ' '
    STDERR.puts "#{@name} (#{@id})"
    @contents.each { |obj| obj.pretty_print(depth + 1) }
  end

  # Takes the given node and pads it until it is the maximum level. For example, if an "individual" LevelZero
  # node were provided to this method, this method would return a randomly generated country node, containing
  # a single randomly generated city node, inside which is the original node instance.
  def self.node_level_up(node)
    lvl = nil
    loop do
      lvl = node.level.level_up
      # TODO What if we waterfall'd here?
      break unless lvl
      node = Node.new(Namer.instance.sample, lvl).tap { |o| o << node }
    end
    node
  end

end
