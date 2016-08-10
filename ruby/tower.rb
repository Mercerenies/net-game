
class TowerNode < StructureNode

  def initialize(builder, tower_name, number)
    super builder, "#{tower_name} Floor #{Numeral.to_numeral number}"
    @tower_name = tower_name
    @number = number
    mark_as_exit if number == 1
  end

  def expand
    if @number < 8 and (rand < 0.5 or @number < 4)
      next_node = make_node TowerNode, @tower_name, @number + 1
      connect self, next_node
    end
  end

end

class TowerBuilder < StructureBuilder

  def initialize(name)
    super()
    @name = name
  end

  def construct
    make_node TowerNode, @name, 1
  end

end

class Tower < Building

  def load(data)
    structure = TowerBuilder.new(data.name).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
