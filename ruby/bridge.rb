
class Bridge < Feature

  def self.load_bridge(data)
    if data.kind_of? PlacePage
      case data.type
      when :forest
        Forest.new.tap { |o| o.load data }
      end
    end
  end

  def bridge_on(*nodes)
    nodes.zip(each_exit.cycle) do |node, exit|
      node.add_link exit.id
      exit.add_link node.id
    end
  end

end

class Forest < Bridge

  def load(data)
    name = data.name
    unless name =~ /forest$/i
      name = "#{name} #{Util.titlecase data.keyword}"
    end
    establish_nodes name
  end

  def establish_nodes(name)

    names = ["#{name} Edge", "#{name} Depths", "Deep #{name}",
             "#{name} Treeline", "#{name} Loop", "#{name} Branch",
             "#{name} Wall", "#{name} Center", "Inner #{name} Region",
             "Outer #{name} Region"]
    points = (3..6).to_a.sample
    @nodes = points.times.collect do
      id = Node.get_id
      name = names.sample
      names = names.delete name
      Location.new id, name, nil
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
      break if i >= nodes.length
    end

    # Loop Edges
    (2..4).times do
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
