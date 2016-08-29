
class Genner
  attr_reader :data

  def initialize(everything)
    @data = GData.new everything
    stages = [NodeStage, BridgeStage, MapStage, BuildingStage, CreatureStage,
              ItemStage, FoodStage, PersonStage, QuestStage, PlayerStage]
    @stages = stages.collect(&:new)
  end

  def generate
    @stages.each { |stage| stage.run @data }
    # Return result
    @data.result_structure
  end

  def alpha_structure
    @data.result_structure
  end

end
