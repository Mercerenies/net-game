
class SiteNode < StructureNode

  def initialize(builder)
    super builder, builder.core_name
    mark_as_exit
  end

end

class SiteBuilder < StructureBuilder

  def initialize(name)
    super()
    self.core_name = name
  end

  def construct
    make_node SiteNode
  end

  def to_loc
    # Have a chance of duplicating the exits, so they each have two ways back to the main map
    if rand < 0.65
      super.tap { |o| o.exits.push(*o.exits) }
    else
      super
    end
  end

end

class Site < Building

  def load(data)
    structure = SiteBuilder.new(data.name).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
