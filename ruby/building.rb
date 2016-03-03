
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
    if data.kind_of? PlacePage
      case data.type
      when :tower
        Tower.load data
      when :crater
        Crater.load data
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

  def integrate_with(map)
    curr = map.to_ary.sample
    each_exit do |exit|
      curr = map[ curr.each_link.to_a.sample ]
      exit.add_link curr.id
      curr.add_link exit.id
    end
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

class Crater < Building

  def self.load(data)
    crater = Crater.new
    crater.load data
    crater
  end

  def load(data)
    crater_name = data.name
    edge_name = "#{crater_name} Site"
    id1 = Node.get_id
    id2 = Node.get_id
    unless crater_name =~ /crater$/i
      crater_name = "#{crater_name} #{Util.titlecase data.keyword}"
    end
    main_crater = Location.new id1, crater_name, nil
    edge_crater = Location.new id2, edge_name, nil
    main_crater.add_link id2
    edge_crater.add_link id1
    @exits = [edge_crater]
    @nodes = [main_crater, edge_crater]
  end

  def integrate_with(map)
    node1 = map.to_ary.sample
    node2 = map[ node1.each_link.to_a.sample ]
    node1.remove_link node2.id
    node2.remove_link node1.id
    if @exits.empty?
      super
    else
      node1.add_link @exits[0].id
      node2.add_link @exits[0].id
      @exits[0].add_link node1.id
      @exits[0].add_link node2.id
    end
  end

end
