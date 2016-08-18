
require 'forwardable'

class Node
  extend Forwardable
  include Enumerable

  attr_reader :id, :name, :level, :contents

  def_delegators :@contents, :each, :[]=, :[], :clear, :size, :length, :empty?

  @@curr_id = 1

  def self.get_id
    @@curr_id += 1
    @@curr_id
  end

  def initialize(name, level)
    @id = Node.get_id
    @name = name.to_s
    @level = level
    @contents = []
  end

  def add(x)
    @contents.push x if x.is_a? Node
  end

  def delete(x)
    @contents.delete x
  end

  def <<(x)
    @contents << x if x.is_a? Node
  end

  def name=(val)
    @name = val.to_s
  end

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

  def expand_to_map(country: nil, gdata:) # TODO Should the country: arg be required?
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
      nodes = @contents.map { |obj| obj.expand_to_map(country: country, gdata: gdata) }
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

  def pretty_print(depth = 0)
    depth.times { print ' *' }
    print ' '
    puts "#{@name} (#{@id})"
    @contents.each { |obj| obj.pretty_print(depth + 1) }
  end

end

def node_level_up(node)
  lvl = nil
  loop do
    lvl = node.level.level_up
    break unless lvl
    node = Node.new(Namer.instance.sample, lvl).tap { |o| o << node }
  end
  node
end
