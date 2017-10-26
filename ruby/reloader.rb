
require 'singleton'

# A general exception for errors while modifying or accessing delta fields.
class DeltaError < StandardError
  def initialize(msg = "Error in delta class")
    super
  end
end

# An exception for errors while reloading data from the alpha file.
class SExprLoadError < StandardError
  def initialize(msg = "Error loading S-Expression")
    super
  end
end

# The reloader singleton is responsible for reloading existing world data from an alpha file that was
# produced by this program earlier in execution.
class Reloader
  include Singleton

  def initialize
    @names = {
      :meta => MetaData,
      :'creature-set' => CreatureSet,
      :'spawner-set' => SpawnerSet,
      :'quest-set' => QuestSet,
      :map => Map,
      :location => Location,
      :'warp-point' => WarpPoint,
      :npc => NPC,
      :player => Player,
      :item => Item,
      :weapon => Weapon,
      :animal => Animal,
      :spawner => Spawner,
      :quest => Quest,
      :validity => ValidityWrapper,
      :plant => Plant,
      :food => Food,
      :'knowledge-base' => KnowledgeBase,
      :'npc-brain' => NPCBrain,
      :'land-only' => LandBasedValidator,
      :'any-animal' => AnimalValidator,
      :plants => PlantTypesValidator,
      :'no-validator' => EmptyValidator,
      :fitness => Fitness,
      :'neo-spawner' => NeoSpawner,
      :request => Request,
      :'request-set' => RequestSet,
      :'city-brain' => CityBrain,
      :motives => MotivePriorities,
      :monster => Monster,
      :'monster-instance' => MonsterInstance
    }
  end

  def load(sexpr)
    inst = @names[sexpr.first]
    raise SExprLoadError, "Invalid instance name #{sexpr.first}" unless inst
    inst.from_sxp sexpr
  end

  def self.load(sexpr)
    Reloader.instance.load sexpr
  end

  # Asserts that the first argument in +args+ matches +sym+. If it does not, SExprLoadError is raised.
  # If it does, all of the remaining arguments are returned in a list.
  def self.assert_first(sym, args)
    if args.first == sym
      args.drop 1
    else
      raise SExprLoadError, "Not a #{sym} field"
    end
  end

  # Iterates over a list of reloadable objects, loading each one and passing it to the block. If a
  # block is not supplied, an array is returned.
  def self.list_like(arr, &block)
    if block.nil?
      arr.each.collect { |x| Reloader.instance.load x }
    else
      arr.each.collect { |x| block.(Reloader.instance.load x) }
    end
  end

  # Iterates over a list of objects, interpreting the list as a plist of key-value pairs. Each
  # key-value pair is passed to the block. If no block is supplied, a hash is returned.
  def self.hash_like(arr, &block)
    if block.nil?
      {}.tap do |hash|
        arr.each_slice(2) { |k, v| hash[k] = v }
      end
    else
      arr.each_slice(2) { |k, v| block.(k, v) }
    end
  end

end

