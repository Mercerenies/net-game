
class ForestNode < StructureNode

  def initialize(builder, number)
    @name = builder.get_a_name
    super builder, @name
    @number = number
    mark_as_exit if number == 1
  end

  def expand
    if @number < 7 and (rand < 0.65 or @number < 5)
      next_node = make_node ForestNode, @number + 1
      connect self, next_node
    else
      mark_as_exit
    end
  end

  def to_loc
    super.tap do |o|
      o.whitelist_creatures Animal
      o.whitelist_plants Plants[:tree, :plant, :bush, :grass, :flower]
    end
  end

end

class ForestBuilder < StructureBuilder

  def initialize(name)
    super()
    @name = name
  end

  def construct
    add_names "#{@name} Edge", "#{@name} Depths", "Deep #{@name}",
              "#{@name} Treeline", "#{@name} Loop", "#{@name} Branch",
              "#{@name} Wall", "#{@name} Center", "Inner #{@name} Region",
              "Outer #{@name} Region"
    make_node ForestNode, 1
    (2..4).to_a.sample.times do
      node1, node2 = sample_nodes 2
      connect node1, node2
    end
  end

end

class Forest < Bridge

  def load(data)
    if data.nil?
      name = "#{Natural.namer.sample} Forest"
    else
      name = data.name
    end
    structure = ForestBuilder.new(name).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
