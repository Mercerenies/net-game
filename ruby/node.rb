
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

  def expand_to_map(country = nil)
    if @contents.empty?
      Array[Location.new id, name, country]
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
      nodes = @contents.map { |obj| obj.expand_to_map country }
      nodes.zip(links).map do |arg|
        node, lnks = arg
        lnks.each do |n1|
          node0 = node.sample
          node1 = nodes[n1].sample
          node0.add_link node1.id
          node1.add_link node0.id
        end
      end
      nodes = nodes.flatten
      if @level.kind_of? LevelTwo
        # Put a warp point somewhere on the map, about once per country
        somewhere = nodes.sample
        somewhere.push WarpPoint.new
      end
      nodes
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
