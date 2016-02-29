
class Building

  def initialize
    @nodes = []
    @exits = []
  end

  def push(*xs)
    xs.each { |x| @nodes.push x }
  end

  def entry_point(x)
    @exits.push x
  end

  def load(node)
    Building.new
  end

  def self.load_building(data)
    if data.kind_of? Place
      case data.type
      when :tower
        Tower.load data
      end
    end
  end

  def [](n)
    @nodes[n]
  end

  def each_node(&block)
    @nodes.each &block
  end

  def each_exit(&block)
    @exits.each &block
  end

end

class Tower < Building

  def self.load(data)
    tower = Tower.new
    tower.load data
    tower
  end

  def load(data)
    floors = (4..8).to_a.sample
    nodes = floors.times.collect do |i|
      id = Node.get_id
      name = "#{data.name} Floor #{Numeral.to_numeral(i + 1)}"
      Location.new id, name, nil
    end
    nodes.size.times.each do |i|
      unless i == nodes.size - 1
        nodes[i + 1].add_link nodes[i].id
        nodes[i].add_link nodes[i + 1].id
      end
    end
    @nodes = nodes
    @exits = [ nodes[0] ]
  end

end
