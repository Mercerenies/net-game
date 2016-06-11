
class Bridge < Feature

  def self.load_bridge(data)
    if data.kind_of? PlacePage
      case data.type
      when :forest
        Forest.new.tap { |o| o.load data }
      end
    end
  end

  def self.create_random(nodal_size = nil)
    nodal_size ||= (2..6).to_a.sample
    Forest.new.tap do |o|
      points = [(nodal_size * 3 / 4).round, 2].max
      o.establish_nodes "#{Natural.namer.sample} Forest", points
    end
  end

  # Each node in (*nodes) is a list of Node instances as obtained in Node#expand_to_map
  def bridge_on(*nodes)
    nodes.zip(each_exit.cycle) do |curr, exit|
      node = curr.sample
      node.add_link exit.id
      exit.add_link node.id
    end
  end

end

class TrivialBridge < Bridge

  def load(data)
  end

  def bridge_on(*nodes)
    (0 ... nodes.size).each do |i|
      (i + 1 ... nodes.size).each do |j|
        n0 = nodes[i].sample
        n1 = nodes[j].sample
        n0.add_link n1.id
        n1.add_link n0.id
      end
    end
  end

end

class Forest < Bridge

  def load(data)
    name = data.name
    unless name =~ /forest$/i
      name = "#{name} #{Util.titlecase data.keyword}"
    end
    points = (3..6).to_a.sample
    establish_nodes name, points
  end

  def establish_nodes(name, points)

    names = ["#{name} Edge", "#{name} Depths", "Deep #{name}",
             "#{name} Treeline", "#{name} Loop", "#{name} Branch",
             "#{name} Wall", "#{name} Center", "Inner #{name} Region",
             "Outer #{name} Region"]
    nodes = points.times.collect do
      id = Node.get_id
      name = names.sample
      names.delete name
      Location.new id, name, nil,
                   valid_creatures: Animal,
                   valid_plants: Plants[:tree, :plant, :bush, :grass, :flower]
    end

    # Standard Path
    i = 0
    loop do
      if i >= nodes.length - 3 or rand < 0.5 # Straight
        nodes[i].add_link nodes[i + 1].id
        nodes[i + 1].add_link nodes[i].id
        i += 1
      else # Branch
        nodes[i].add_link nodes[i + 1].id
        nodes[i + 1].add_link nodes[i].id
        nodes[i].add_link nodes[i + 2].id
        nodes[i + 2].add_link nodes[i].id
        nodes[i + 1].add_link nodes[i + 3].id
        nodes[i + 3].add_link nodes[i + 1].id
        nodes[i + 2].add_link nodes[i + 3].id
        nodes[i + 3].add_link nodes[i + 2].id
        i += 3
      end
      break if i >= nodes.length - 1
    end

    # Loop Edges
    (2..4).to_a.sample.times do
      n0 = nodes.sample
      n1 = nodes.sample
      unless n0 == n1
        n0.add_link n1.id
        n1.add_link n0.id
      end
    end

    @nodes = nodes
    @exits = [ nodes[0], nodes[-1] ]

  end

end
