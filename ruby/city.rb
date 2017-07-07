
# ///// TODO Our next step is to make city planning more "creative" instead of making the same city each time
# TODO With CrossCity, we have the square which has four exits and makes it look really busy; is that okay?

class City < Feature
  attr_reader :builder

  def initialize
    @builder = nil
  end

  def structure_node
    each_node.detect { |x| x.id == builder.core_node }
  end

end

class CrossCityEdgeNode < StructureNode

  include PredefFitness

  def initialize(builder, prefix)
    super builder, "#{prefix} #{builder.core_name}"
    mark_as_exit
    self.fitness = Outdoors
  end

end

class CrossCitySquareNode < StructureNode

  include PredefFitness # TODO :city-exit is being ignored, I think...

  def initialize(builder)
    super builder, "#{builder.core_name} Square"
    self.fitness = Outdoors
  end

  def expand
    connect self, make_node(CrossCityEdgeNode, "Northern")
    connect self, make_node(CrossCityEdgeNode, "Eastern")
    connect self, make_node(CrossCityEdgeNode, "Western")
    connect self, make_node(CrossCityEdgeNode, "Southern")
  end

end

class CrossCityBuilder < StructureBuilder

  def initialize(name, country)
    super()
    self.core_name = name
    self.country = country
  end

  def construct
    make_node CrossCitySquareNode
  end

  def to_loc
    super().tap do |result|
      # We want to mark each exit as being an exit node for bookkeeping and linkage purposes
      result.exits.each { |x| x.linkage = :'city-exit' }
    end
  end

end

class CrossCity < City

  def load(data)
    load_named(data.name, nil)
  end

  def load_named(city_name, country_name)
    city_name ||= Namer.instance.sample
    country_name ||= Namer.instance.sample
    @builder = CrossCityBuilder.new(city_name, country_name).tap(&:construct)
    structure = builder.to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
