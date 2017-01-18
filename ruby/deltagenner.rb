
# A Genner for the delta generation process. A DeltaGenner behaves in much the same way as a traditional
# generator, except that its default sequence of stages is altered. Additionally, the new functionality
# #delta_structure is provided by DeltaGenner and not Genner.
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
