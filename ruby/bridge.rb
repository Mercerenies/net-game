
# A bridge is a Feature which is designed to connect two or more other components. Non-bridge features
# may have multiple exits as well, but unlike with bridges they will not be used as the "glue" to hold
# other components together. Bridges will frequently be used as the only connector or the shortest
# connector between two otherwise distant nodes.
class Bridge < Feature

  # Given a Page instance, determines whether or not it is a location page which can be made into a
  # bridge. If it can, the appropriate bridge is generated and returned. Otherwise, +nil+ is returned.
  def self.load_bridge(data)
    if data.kind_of? PlacePage
      case data.type
      when :forest
        Forest.new.tap { |o| o.load data }
      when :lake
        Lake.new.tap { |o| o.load data }
      end
    end
  end

  # Generates a random, nontrivial bridge with an appropriate randomly generated name and structure.
  def self.create_random
    case [1, 2].sample
    when 1
      Forest.new.tap { |o| o.load nil }
    when 2
      Lake.new.tap { |o| o.load nil }
    end
  end

  # Given a collection of lists of Location instances, integrates the bridge into the map.
  #
  # Each exit (according to the #each_exit method) of the bridge will be linked to a location that was
  # provided. Assuming that there are +n+ exits in the bridge and +k+ arguments are provided, one of
  # three things will happen.
  # * If +k+ < +n+, then each of the first +k+ exits will be linked to a random node in the
  #   corresponding argument. The later exits will not be used.
  # * If +k+ = +n+, then every exit will be linked to exactly one random node from the corresponding
  #   argument list. Each exit will be used exactly once.
  # * If +k+ > +n+, each list of arguments will have one randomly selected node linked to the
  #   corresponding exit. For arguments after the +n+th argument, the exit list will cycle, meaning
  #   that exits may be used multiple times but each argument will be used exactly once.
  def bridge_on(*nodes)
    nodes.zip(each_exit.cycle) do |curr, exit|
      node = curr.sample
      node.add_link exit.id
      exit.add_link node.id
    end
  end

end

# A trivial bridge is used in circumstances where a bridge is expected but no bridge is actually
# desired. A trivial bridge contains no nodes or edges and performs its #bridge_on operation in a
# very trivial way.
class TrivialBridge < Bridge

  # Trivial bridges have nothing to load from data. Attempting to do so is a no-op.
  def load(data)
  end

  # This method bridges between the nodes in a very trivial way. Considering the +n+ lists of nodes
  # given as arguments to be a graph of +n+ vertices, bridging with a TrivialBridge will connect all
  # of the nodes in a way that the graph is the
  # {complete graph}[https://en.wikipedia.org/wiki/Complete_graph] +K_n+
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
