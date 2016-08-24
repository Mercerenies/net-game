
require 'singleton'

class DeltaError < StandardError
  def initialize(msg = "Error in delta class")
    super
  end
end

class SExprLoadError < StandardError
  def initialize(msg = "Error loading S-Expression")
    super
  end
end

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
      :validity => ValidityWrapper
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

  def self.assert_first(sym, args)
    if args.first == sym
      args.drop 1
    else
      raise SExprLoadError, "Not a #{sym} field"
    end
  end

  def self.list_like(arr, &block)
    if block.nil?
      arr.each.collect { |x| Reloader.instance.load x }
    else
      arr.each.collect { |x| block.(Reloader.instance.load x) }
    end
  end

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

