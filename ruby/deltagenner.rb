
class DeltaGenner < Genner
  attr_reader :data

  def initialize(everything, alpha)
    @data = DeltaGData.new alpha, everything
    stages = [DeltaNodeStage, BridgeStage, MapStage, BuildingStage, CreatureStage,
              ItemStage, FoodStage, PersonStage, QuestStage]
    @stages = stages.collect(&:new)
  end

  def delta_structure
    @data.delta_structure
  end

end
