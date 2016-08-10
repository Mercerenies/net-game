
class CraterNode < StructureNode

  def initialize(builder, kw = "Crater")
    nickname = builder.core_name
    unless nickname =~ /crater$/i
      nickname = "#{builder.core_name} #{Util.titlecase kw}"
    end
    super builder, "#{nickname}"
  end

  def expand
    edge = make_node CraterEdgeNode
    connect self, edge
  end

end

class CraterEdgeNode < StructureNode
  def initialize(builder)
    super builder, "#{builder.core_name} Site"
    mark_as_exit
  end
end

class CraterBuilder < StructureBuilder

  def initialize(name, kw)
    super()
    self.core_name = name
    @keyword = kw
  end

  def construct
    make_node CraterNode, @keyword
  end

end

class Crater < Building

  def load(data)
    structure = CraterBuilder.new(data.name, data.keyword).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
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
