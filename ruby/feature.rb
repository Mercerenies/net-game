
# A collection of locations that form a cohesive structure when taken together. Note that features are
# intended to be finalized structures, not modifiable data, which is why there are relatively few
# editing methods provided in this interface. Features should be built up using #StructureBuilder or
# similar interfaces and then compiled.
class Feature

  # Constructs an empty feature, consisting of no nodes.
  def initialize
    @nodes = []
    @exits = []
  end

  # Given a data instance from a page, loads the structure from the information on the page. Note that,
  # unless otherwise stated, none of the loading preconditions are checked here. Any checking to verify
  # that the page is valid for this purpose should be done by the caller.
  def load(data)
  end

  # Adds a collection of locations to the world feature.
  def push(*xs)
    xs.each { |x| @nodes.push x }
  end

  # Pushes a location onto the exit list of the feature. The exit nodes should always form a subset of
  # the set of nodes. If #push_exit is passed a node which is not in the node set, it is a no-op.
  def push_exit(x)
    @exits.push x if @nodes.find? x
  end

  # Accesses the nth node in the node list, returning +nil+ if out of bounds.
  def [](n)
    @nodes[n]
  end

  # Iterates over the node list.
  def each_node(&block)
    @nodes.each &block
  end

  # Iterates over the exit list.
  def each_exit(&block)
    @exits.each &block
  end

end
