
# The master generator instance. A generator has a corresponding GData instance, as well as a
# sequence of Stage instances. When run, it executes the stages in sequence and uses the
# corresponding GData instance as the result of the generation process.
class Genner
  attr_reader :data

  # Initializes the generator, given a list of pages.
  def initialize(everything, small_world: false)
    @data = GData.new everything, small_world: small_world
    stages = [NodeStage, BridgeStage, MapStage, BuildingStage, CreatureStage,
              PoolStage, FoodStage, PersonStage, QuestStage, PlayerStage, RequestStage]
    @stages = stages.collect(&:new)
  end

  # Runs each stage of the generator, returning the resulting structure.
  def generate
    Logger.echo 1, "Beginning world generation"
    @stages.each do |stage|
      Logger.echo 2, "Running #{stage.name}"
      stage.run @data
    end
    Logger.echo 1, "World generation complete"
    # Return result
    @data.result_structure
  end

  # Returns the resulting structure from the generation. The value of #alpha_structure is undefined
  # if #generate has not been run yet.
  def alpha_structure
    @data.result_structure
  end

end
