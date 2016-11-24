
class LakeNode < StructureNode

  def initialize(builder, number_left)
    @name = builder.get_a_name
    super builder, @name
    @number_left = number_left
    @creatures = AnimalValidator.new
    @plants = PlantTypesValidator[:tree, :plant, :bush, :grass, :flower]
  end

  def expand
    if @number_left > 1
      next_node = make_node LakeNode, @number_left - 1
      connect self, next_node
    end
  end

end

class LakeBuilder < StructureBuilder
  @@namesets = [["XXX West Edge", "XXX North Edge", "XXX East Edge"],
                ["XXX Lower Corner", "XXX Shallow End", "XXX Deep End"],
                ["XXX West Edge", "XXX North Edge", "XXX East Edge", "XXX South Edge"],
                ["XXX Lower Corner", "XXX Shallow End", "XXX Deep End", "XXX Western Edge"],
                ["XXX SW Edge", "XXX SE Edge", "XXX NE Edge", "XXX NW Edge"],
                ["XXX West Edge", "XXX North Edge", "XXX East Edge", "XXX South Edge", "XXX Clearing"],
                ["XXX SW Edge", "XXX SE Edge", "XXX Eastern Edge", "XXX NE Edge", "XXX Northern Edge"],
                ["XXX West Edge", "XXX North Edge", "XXX Shallow Break", "XXX East Edge",
                 "XXX South Edge", "XXX Clearing"],
                ["XXX SW Edge", "Misty XXX", "XXX SE Edge", "XXX Eastern Edge",
                 "XXX NE Edge", "XXX Northern Edge"],
                ["XXX West Edge", "Deep XXX End", "XXX North Edge", "XXX Shallow Break", "XXX East Edge",
                 "XXX South Edge", "XXX Clearing"],
                ["XXX West", "XXX Northwest", "XXX North", "XXX Northeast",
                 "XXX East", "XXX Southeast", "XXX South", "XXX Southwest"]]

  def initialize(name)
    super()
    self.core_name = name
  end

  def construct
    total = [3, 4, 4, 4, 5, 6, 6, 7, 8, 8, 8].sample
    names = @@namesets.select { |x| x.length == total }.sample.map { |str| str.gsub "XXX", core_name }
    add_names(*names)
    make_node LakeNode, total
    connect @nodes[0], @nodes[-1] # Complete the ring
    sample_nodes(3).each &:mark_as_exit
    sample_node.mark_as_exit while each_exit.count < 2
  end

  def get_a_name
    @names.shift
  end

end

class Lake < Bridge

  def load(data)
    if data.nil?
      name = "#{Natural.namer.sample} Lake"
    else
      name = data.name
    end
    structure = LakeBuilder.new(name).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
