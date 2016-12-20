
# The main central node of the Bank structure.
class BankMainNode < StructureNode

  include PredefFitness

  def initialize(builder)
    super builder, "#{builder.core_name} Lobby"
    mark_as_exit
    self.fitness = Safekeeping
  end

end

# A trivial builder for a Bank.
class BankBuilder < StructureBuilder

  def initialize(name)
    super()
    self.core_name = name
  end

  def construct
    make_node BankMainNode
  end

end

# A bank is a relatively simple building, with one main room that is marked by the fitness system as
# a safekeeping location.
class Bank < Building

  def can_integrate_with?(node)
    node.civilized?
  end

  def load(data)
    structure = BankBuilder.new(data.name).tap(&:construct).to_loc
    @nodes = structure.nodes
    @exits = structure.exits
  end

end
