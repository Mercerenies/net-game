
class Feature

  def initialize
    @nodes = []
    @exits = []
  end

  def load(node)
  end

  def push(*xs)
    xs.each { |x| @nodes.push x }
  end

  def push_exit(x)
    @exits.push x
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
